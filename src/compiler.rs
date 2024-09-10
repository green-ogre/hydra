use crate::ast::*;

pub fn compile(mut scope: Scope) -> String {
    let mut visitor = Ast2AsmVisitor::default();
    visitor.write("global main\n").section("text").write("\n");

    walk_scope(&mut visitor.fns, &mut scope);
    walk_scope(&mut visitor, &mut scope);
    println!("{visitor:#?}");

    //     ; rsi -- *char
    //     ; rdx -- buf_len
    // __print:
    // 	mov rax, 1
    //     mov rdi, 1
    //     mov rsi, message
    //     mov rdx, 14
    // 	syscall
    //     ret

    // visitor
    //     .comment(" rsi -- *char")
    //     .comment(" rdx -- buf_len")
    //     .write("__print:\n")
    //     .write("\tmov rax, 1\n")
    //     .write("\tmov rsi, message\n")
    //     .write("\tmov rdx, 14\n")
    //     .write("\tsyscall\n")
    //     .write("\tret\n\n");

    visitor
        .write("main:\n")
        .write("\tcall _main\n")
        .write("\tmov rdi, rax\n")
        .write("\tmov rax, 60\n")
        .write("\tsyscall\n\n");

    visitor
        .asm(visitor.global.clone())
        .write("\nmessage: db \"Hello, World!\"\n");

    visitor.output
}

#[derive(Debug, Clone)]
struct SVar {
    ident: Ident,
    offset: usize,
    size: usize,
    locality: Locality,
    ty: Type,
}

#[derive(Debug, Clone, Copy)]
enum Locality {
    Local,
    Param,
}

#[derive(Debug)]
enum Var {
    Value(SVar),
    Ptr(SVar),
}

impl Var {
    pub fn new(var: SVar) -> Self {
        if var.size > 8 {
            Self::Ptr(var)
        } else {
            Self::Value(var)
        }
    }

    pub fn offset(&self) -> usize {
        match self {
            Var::Value(var) => var.offset,
            Var::Ptr(var) => var.offset,
        }
    }

    pub fn val_size(&self) -> usize {
        match self {
            Var::Value(var) => var.size,
            Var::Ptr(var) => var.size,
        }
    }
}

impl Asm for Var {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        match self {
            Self::Value(var) => ast2asm.write(&format!("[rsp+{}]", var.offset)),
            Self::Ptr(var) => todo!(),
        };
    }
}

/// Stores metadata about the variables on the stack within a function scope.
///
/// This includes parameters, which are denoted by the locality field in [`SVar`].
#[derive(Debug, Default)]
struct SFrame {
    vars: Vec<SVar>,
}

impl SFrame {
    pub fn push(&mut self, var: Ident, ty: Type, locality: Locality) {
        let offset = if let Some(var) = self.vars.last() {
            var.offset + ty.size()
        } else {
            0
        };

        self.vars.push(SVar {
            ident: var,
            offset,
            size: ty.size(),
            locality,
            ty: ty.clone(),
        });
    }

    pub fn get(&self, var: &Ident) -> Option<&SVar> {
        self.vars
            .iter()
            .find_map(|v| if &v.ident == var { Some(v) } else { None })
    }

    pub fn size(&self) -> usize {
        if let Some(var) = self.vars.last() {
            var.offset + var.size
        } else {
            0
        }
    }

    pub fn size_aligned(&self) -> usize {
        let size = self.size();
        if size > 8 {
            8 + (size / 16 + 1) * 16
        } else {
            8
        }
    }
}

/// Generates the [`SFrame`] for a given function.
#[derive(Debug, Default)]
struct SFrameVisitor {
    frame: SFrame,
}

impl Visitor for SFrameVisitor {
    fn visit_fn_param(&mut self, param: &mut Param) {
        self.frame
            .push(param.ident.clone(), param.ty.clone(), Locality::Param);
    }

    fn visit_let(&mut self, lt: &mut Let) {
        self.frame
            .push(lt.lhs.clone(), lt.ty.clone(), Locality::Local);
    }
}

/// Moves a pointer to [`SVar`] into [`Reg::A`].
///
/// ...
/// mov rax, rsp
/// add rax, <offset>
/// ...
#[derive(Debug)]
struct SVarRef {
    offset: usize,
    locality: Locality,
}

impl SVarRef {
    pub fn new(var: &SVar) -> Self {
        Self {
            offset: var.offset,
            locality: var.locality,
        }
    }

    pub fn new_with_offset(var: &SVar, offset: usize) -> Self {
        Self {
            offset: var.offset + offset,
            locality: var.locality,
        }
    }
}

impl Asm for SVarRef {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        let frame_offset = match self.locality {
            Locality::Local => 0,
            Locality::Param => ast2asm.current_frame.frame.size_aligned() + 8,
        };

        ast2asm
            .mov(
                RegSized(Reg::Sp, Size::Qword),
                RegSized(Reg::A, Size::Qword),
            )
            .add_usize(Reg::A, self.offset + frame_offset, Size::Qword);
    }
}

/// Dereferences [`SVar`] inline.
///
/// ...
/// <size> [rsp+<offset>]
/// ...
#[derive(Debug)]
struct SVarDeref {
    size: usize,
    offset: usize,
    locality: Locality,
}

impl SVarDeref {
    pub fn new(var: &SVar) -> Self {
        Self {
            size: var.size,
            offset: var.offset,
            locality: var.locality,
        }
    }

    pub fn new_with_offset(var: &SVar, offset: usize) -> Self {
        Self {
            size: var.size,
            offset: var.offset + offset,
            locality: var.locality,
        }
    }
}

impl Asm for SVarDeref {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        let frame_offset = match self.locality {
            Locality::Local => 0,
            Locality::Param => ast2asm.current_frame.frame.size_aligned() + 8,
        };

        ast2asm
            .asm(Size::from(self.size))
            .write(&format!("[rsp+{}]", self.offset + frame_offset));
    }
}

/// Dereferences a position on the stack.
///
/// ...
/// <size> [rsp+<offset>]
/// ...
#[derive(Debug)]
struct SDeref {
    size: Size,
    offset: usize,
}

impl SDeref {
    pub fn new(size: Size, offset: usize) -> Self {
        Self { size, offset }
    }
}

impl Asm for SDeref {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        ast2asm
            .asm(self.size)
            .write(&format!("[rsp+{}]", self.offset));
    }
}

#[derive(Debug, Clone)]
struct CStr {
    val: String,
}

#[derive(Debug, Clone)]
enum Const {
    Str(CStr),
}

/// Global data stored in the data section of the program.
#[derive(Debug, Default, Clone)]
struct Global {
    data: Vec<Const>,
}

impl Asm for Global {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        ast2asm.section("data");
    }
}

/// Generates function information for [`Ast2AsmVisitor`].
#[derive(Debug, Default)]
struct FnDeclVisitor {
    decls: Vec<FnDecl>,
}

impl FnDeclVisitor {
    pub fn find(&self, ident: &Ident) -> Option<&FnDecl> {
        self.decls.iter().find(|f| &f.ident == ident)
    }
}

impl Visitor for FnDeclVisitor {
    fn visit_fn_decl(&mut self, decl: &mut FnDecl) {
        self.decls.push(decl.clone());
    }
}

/// Walks the Ast and generates assembly.
#[derive(Debug, Default)]
struct Ast2AsmVisitor {
    fns: FnDeclVisitor,
    output: String,
    current_frame: SFrameVisitor,
    global: Global,
}

trait Asm {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor);
}

impl<T: Asm> Asm for &T {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        <T as Asm>::asm(self, ast2asm);
    }
}

impl Asm for usize {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        ast2asm.write(&format!("{}", self));
    }
}

impl Asm for Lit {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        match self.kind {
            LitKind::Int => ast2asm.write(&format!("{}", self.val)),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reg {
    A,
    B,
    C,
    Sp,
    Bp,
    D,
    Si,
    Dx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Size {
    Byte,
    Word,
    Dword,
    Qword,
}

impl Asm for Size {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        ast2asm.write(match self {
            Self::Byte => "byte ",
            Self::Word => "word ",
            Self::Dword => "dword ",
            Self::Qword => "qword ",
        });
    }
}

impl From<usize> for Size {
    fn from(value: usize) -> Self {
        match value {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => panic!("invalid size"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RegSized(Reg, Size);

impl Asm for RegSized {
    fn asm(&self, ast2asm: &mut Ast2AsmVisitor) {
        let reg = match self.0 {
            Reg::A => "ax",
            Reg::B => "bx",
            Reg::C => "cx",
            Reg::Sp => "sp",
            Reg::Bp => "bp",
            Reg::D => "di",
            Reg::Si => "si",
            Reg::Dx => "dx",
        };

        let prefix = match self.1 {
            Size::Qword => "r",
            Size::Dword => "e",
            _ => "",
        };

        if self.1 == Size::Byte {
            panic!("byte size not supported");
        }

        ast2asm.write(&format!("{}{}", prefix, reg));
    }
}

impl Ast2AsmVisitor {
    pub fn write(&mut self, str: &str) -> &mut Self {
        self.output.push_str(str);
        self
    }

    pub fn asm(&mut self, asm: impl Asm) -> &mut Self {
        asm.asm(self);
        self
    }

    pub fn comment(&mut self, cmt: &str) -> &mut Self {
        self.write(&format!("\t;{}\n", cmt))
    }

    pub fn section(&mut self, str: &str) -> &mut Self {
        self.write(&format!("section .{}\n", str))
    }

    pub fn grow_stack(&mut self, bytes: usize) -> &mut Self {
        self.write(&format!("\tsub rsp, {}\n", bytes))
    }

    pub fn shrink_stack(&mut self, bytes: usize) -> &mut Self {
        self.write(&format!("\tadd rsp, {}\n", bytes))
    }

    // pub fn move_var_to_reg(&mut self, ident: &Ident, reg: RegSized) -> &mut Self {
    //     let var = self.current_frame.frame.get(&ident).unwrap();
    //     let offset = match var.locality {
    //         Locality::Param => {
    //             StackOffset::Param(var.offset + self.current_frame.frame.size_aligned() + 8)
    //         }
    //         Locality::Local => StackOffset::Local(var.offset),
    //     };
    //
    //     self.mov(StackPtr::new(Size::from(var.size), offset), reg);
    //     self
    // }

    pub fn mov(&mut self, src: impl Asm, dst: impl Asm) -> &mut Self {
        self.write("\tmov ")
            .asm(dst)
            .write(", ")
            .asm(src)
            .write("\n")
    }

    pub fn var(&self, ident: &Ident) -> &SVar {
        self.current_frame
            .frame
            .get(ident)
            .expect("checked in parsing")
    }

    pub fn call(&mut self, call: &Call) -> &mut Self {
        self.comment("calling into function");
        if !call.input.is_empty() {
            let call_stack_size = call.size_aligned();
            self.comment("pushing parameters onto stack")
                .grow_stack(call_stack_size);

            let fn_input = self.fns.find(&call.ident).unwrap().input.clone();

            let mut param_offset = 0;
            for (call_input, fn_input) in call.input.iter().zip(fn_input.iter()) {
                // let param_size = self.var(&fn_input.ident).size;
                // let param_ty = self.var(&fn_input.ident).ty.clone();

                if fn_input.ty.size() > 8 {
                    // panic!("{:?}", call_input.ident);

                    if call_input.ty != fn_input.ty {
                        assert!(call_input.ty.can_coerce(&fn_input.ty));
                        match fn_input.ty {
                            Type::Slice(_) => match &call_input.ty {
                                Type::Ptr(inner) => match &**inner {
                                    Type::Array(arr) => {
                                        SVarRef::new_with_offset(
                                            self.var(&call_input.ident),
                                            call_stack_size,
                                        )
                                        .asm(self);
                                        self.mov(
                                            arr.len,
                                            SDeref::new(Size::Qword, param_offset + 8),
                                        )
                                        .mov(
                                            RegSized(Reg::A, Size::Qword),
                                            SDeref::new(Size::Qword, param_offset),
                                        );
                                    }
                                    _ => panic!(),
                                },
                                _ => panic!(),
                            },
                            _ => panic!(),
                        }
                    } else {
                        SVarRef::new_with_offset(self.var(&call_input.ident), call_stack_size)
                            .asm(self);
                        self.mov(
                            RegSized(Reg::A, Size::Qword),
                            SDeref::new(Size::Qword, param_offset),
                        );
                    }
                } else {
                    let reg = RegSized(Reg::A, Size::from(fn_input.ty.size()));
                    self.mov(
                        SVarDeref::new_with_offset(self.var(&call_input.ident), call_stack_size),
                        reg,
                    )
                    .mov(
                        reg,
                        SDeref::new(Size::from(fn_input.ty.size()), param_offset),
                    );
                }

                // let reg = RegSized(Reg::A, Size::from(param.size));
                // let rf = StackPtr::new(Size::from(param.size), StackOffset::Param(param_offset));

                param_offset += fn_input.ty.size();

                // let param = StackPtr {
                //     size: Size::from(param.size),
                //     offset: StackOffset::Param(param.offset + call.size_aligned()),
                // };
                // self.mov(param, reg).mov(reg, rf);
            }
        }
        self.write(&format!("\tcall _{}\n", call.ident.value()));

        if !call.input.is_empty() {
            self.shrink_stack(call.size_aligned());
        }

        self
    }

    pub fn evaluate_expr_to_reg(&mut self, expr: &Expr, reg: Reg) -> &mut Self {
        let secondary = match reg {
            Reg::A => Reg::B,
            Reg::B => Reg::A,
            _ => panic!("reg must be either a or b in order to evaluate"),
        };

        match expr {
            Expr::Ref(r) => {
                // self.move_var_to_reg(&r.ident, RegSized(reg, Size::from(r.ty.size())));
                // if r.ty.size() > 8 {
                // } else {
                //     self.mov(self.var(&r.ident))
                // }

                if r.ty.size() > 8 {
                    panic!();
                } else {
                    self.mov(
                        SVarDeref::new(self.var(&r.ident)),
                        RegSized(reg, Size::from(r.ty.size())),
                    );
                }
            }
            Expr::Lit(lit) => {
                // overwrite entire reg
                self.mov(lit, RegSized(reg, Size::Qword));
            }
            Expr::Call(call) => {}
            Expr::Add(lhs, rhs) => {
                // TODO: have to add stuff to stack
                // self.evaluate_expr_to_reg(lhs, Reg::A);
                // self.evaluate_expr_to_reg(rhs, Reg::B);
            }
            _ => panic!("could not evaluate expr"),
        }

        self
    }

    pub fn clear_memory(&mut self, ident: &Ident) -> &mut Self {
        let var = self.var(ident);
        let offset = var.offset;
        let size = var.size;
        self.comment("clear memory")
            .mov(
                RegSized(Reg::Sp, Size::Qword),
                RegSized(Reg::A, Size::Qword),
            )
            .add_usize(Reg::A, offset, Size::Qword)
            .mov(RegSized(Reg::A, Size::Qword), RegSized(Reg::D, Size::Qword))
            .mov(size, RegSized(Reg::C, Size::Qword))
            .mov(0x21, RegSized(Reg::A, Size::Qword))
            //.zero_reg(Reg::A)
            .write("\trep stosb\n")

        // mov edi, buffer             ; Destination address (pointer to buffer)
        // mov ecx, size               ; Number of bytes to write (size of buffer)
        // xor eax, eax                 ; Zero out EAX register (value to write, which is 0)
        // rep stosb                   ; Repeat storing EAX (0) into [EDI] for ECX times
    }

    pub fn zero_reg(&mut self, reg: Reg) -> &mut Self {
        self.write("\txor ")
            .asm(RegSized(reg, Size::Qword))
            .write(", ")
            .asm(RegSized(reg, Size::Qword))
            .write("\n")
    }

    pub fn add_usize(&mut self, lhs: Reg, rhs: usize, size: Size) -> &mut Self {
        self.write("\tadd ")
            .asm(RegSized(lhs, size))
            .write(&format!(", {}\n", rhs))
    }

    pub fn add_reg(&mut self, lhs: Reg, rhs: Reg, size: Size) -> &mut Self {
        self.write("\tadd ")
            .asm(RegSized(lhs, size))
            .write(", ")
            .asm(RegSized(rhs, size))
            .write("\n")
    }

    /// Add lhs and rhs into [`Reg::A`].
    pub fn add(&mut self, lhs: &Expr, rhs: &Expr) -> &mut Self {
        self.evaluate_expr_to_reg(lhs, Reg::A);
        self.evaluate_expr_to_reg(rhs, Reg::B);
        // TODO: this needs to consider the size of the arguments
        self.add_reg(Reg::A, Reg::B, Size::Qword)
    }
}

impl Visitor for Ast2AsmVisitor {
    fn visit_fn(&mut self, f: &mut Func) {
        self.current_frame = SFrameVisitor::default();
        self.current_frame.visit_fn(f);

        let frame_size = self.current_frame.frame.size_aligned();
        self.write(&format!("_{}:\n", f.decl.ident.value()))
            .comment("allocating stack")
            .grow_stack(frame_size)
            .comment("body");

        self.visit_fn_body(&mut f.body);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Ret(ret) => {
                self.comment("move output into the a reg");
                if let Some(ret) = ret {
                    match &**ret {
                        Expr::Ref(r) => {
                            if r.ty.size() > 8 {
                                self.mov(
                                    SVarRef::new(self.var(&r.ident)),
                                    RegSized(Reg::A, Size::Qword),
                                );
                            } else {
                                self.mov(
                                    SVarDeref::new(self.var(&r.ident)),
                                    RegSized(Reg::A, Size::from(r.ty.size())),
                                );
                            }
                        }
                        Expr::Lit(lit) => {
                            self.mov(lit, RegSized(Reg::A, Size::Qword));
                        }
                        Expr::Call(call) => {
                            self.call(call);
                        }
                        Expr::Add(lhs, rhs) => {
                            self.add(lhs, rhs);
                        }
                        _ => panic!("invalid return statement: {:#?}", ret),
                    }
                } else {
                    // self.write("\t;nothing to output");
                    // self.write("\txor rax, rax");
                }

                let frame_size = self.current_frame.frame.size_aligned();
                self.comment("clean stack")
                    .shrink_stack(frame_size)
                    .write("\tret\n\n");
            }
            Expr::Call(call) => {
                self.call(call);
            }
            _ => panic!(),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Let(lt) => {
                // let var = self.current_frame.frame.get(&lt.lhs).unwrap();

                match &lt.rhs {
                    Some(Expr::Lit(lit)) => {
                        // self.output.push_str(&format!(
                        //     "\tmov {} [rbp-{}], {}\n",
                        //     Ast2AsmVisitor::usize_to_keyword(var.size),
                        //     var.offset,
                        //     lit.val
                        // ));
                        //self.mov(lit, StackPtr::local(self.var(&lt.lhs).unwrap()));
                        if lt.ty.size() > 8 {
                            panic!("this is too big");
                        } else {
                            self.mov(lit, SVarDeref::new(self.var(&lt.lhs)));
                        }
                    }
                    Some(Expr::Call(call)) => {
                        self.call(call);
                        match &call.output {
                            FnType::Void => {}
                            FnType::Ty(ty) => {
                                self.comment("assigning result to var");
                                if ty.size() > 8 {
                                    panic!("need to copy data here");
                                // self.mov(
                                //     RegSized(Reg::A, Size::from(ty.size())),
                                //     SVarRef(),
                                //     // StackPtr::local(self.var(&lt.lhs).unwrap()),
                                // );
                                } else {
                                    self.mov(
                                        RegSized(Reg::A, Size::from(ty.size())),
                                        SVarDeref::new(self.var(&lt.lhs)),
                                        // StackPtr::local(self.var(&lt.lhs).unwrap()),
                                    );
                                }
                            }
                        }
                    }
                    Some(Expr::Ref(r)) => {
                        todo!();
                        // let new_var = self.current_frame.frame.get(&r.ident).unwrap();
                        // let var = self.current_frame.frame.get(&lt.lhs).unwrap();

                        // self.output.push_str(&format!(
                        //     "\tmov {} [rbp-{}], [rdp-{}]\n",
                        //     Ast2AsmVisitor::usize_to_keyword(var.size),
                        //     new_var.offset,
                        //     var.offset,
                        // ));
                    }
                    Some(Expr::Empty) => {
                        panic!("was this supposed to be here?");
                    }
                    Some(Expr::Add(lhs, rhs)) => {
                        // let rf = StackPtr::local(self.var(&lt.lhs).unwrap());
                        // self.evaluate_expr_to_reg(lhs, Reg::A)
                        //     .evaluate_expr_to_reg(rhs, Reg::B)
                        //     // TODO: might have to clear these
                        //     .write("\tadd rax, rbx\n")
                        //     .mov(RegSized(Reg::A, Size::from(rf.size)), rf);
                    }
                    None => {
                        self.clear_memory(&lt.lhs);
                    }
                    _ => panic!(),
                }
            }
            Stmt::Item(item) => match item {
                Item::Fn(f) => self.visit_fn(f),
            },
            Stmt::InlineAsm(asm) => {
                self.write(asm);
            }
            _ => panic!(),
        }
    }
}
