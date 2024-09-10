#[derive(Debug)]
pub struct Func {
    pub decl: FnDecl,
    pub body: Scope,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub ident: Ident,
    pub input: Vec<Param>,
    pub output: FnType,
}

impl FnDecl {
    pub fn call(&self) -> Call {
        Call {
            ident: self.ident.clone(),
            input: self.input.clone(),
            output: self.output.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub ident: Ident,
    pub input: Vec<Param>,
    pub output: FnType,
}

impl Call {
    /// Size required to store all input parameters onto the stack.
    ///
    /// Assumes current 16 bytes alignment.
    pub fn size_aligned(&self) -> usize {
        let size = self.input.iter().map(|p| p.ty.size()).sum::<usize>();
        (size / 16 + 1) * 16
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub ident: Ident,
    pub ty: Type,
    // pub pat: Pat,
}

#[derive(Debug)]
pub enum Pat {
    Ident(Ident),
    // Lit(Lit),
    // Call(Call),
    // Add(Add),
}

#[derive(Debug)]
pub struct Add {
    lhs: Box<Pat>,
    rhs: Box<Pat>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum FnType {
    #[default]
    Void,
    Ty(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTy {
    pub len: usize,
    pub element: Box<Type>,
}

impl ArrayTy {
    pub fn size(&self) -> usize {
        self.len * self.element.size()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    Char,
    Array(ArrayTy),
    // [ Ptr(Array(_)) ] [ len ]
    Slice(Box<Type>),
    Ptr(Box<Type>),
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match &*value {
            "i32" => Type::I32,
            "char" => Type::Char,
            _ => panic!("invalid type: {}", value),
        }
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Self::I32 => 4,
            Self::Char => 1,
            Self::Array(arr) => arr.size(),
            // [ Ptr(Array(_)) ] [ len ]
            Self::Slice(_) => 16,
            Self::Ptr(_) => 8,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Self::Slice(_) | Self::Ptr(_) => true,
            Self::I32 | Self::Char | Self::Array(_) => false,
        }
    }

    pub fn can_coerce(&self, other: &Type) -> bool {
        println!("{self:?}, {other:?}");
        match other {
            Self::Slice(ty) => match self {
                Self::Ptr(inner) => match &**inner {
                    Self::Array(arr) => **ty == *arr.element,
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit {
    pub kind: LitKind,
    pub ty: Option<Type>,
    pub val: String,
}

impl Lit {
    pub fn int(val: String) -> Self {
        assert!(val.chars().all(|c| c.is_numeric()));
        Self {
            kind: LitKind::Int,
            ty: None,
            val,
        }
    }

    pub fn with_type(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LitKind {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    Curly,
    Paren,
}

impl Ident {
    pub fn new(ident: String) -> Self {
        Ident(ident)
    }

    pub fn value(&self) -> &String {
        &self.0
    }
}

#[derive(Debug)]
pub struct Let {
    pub lhs: Ident,
    pub ty: Type,
    pub rhs: Option<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Ret(Option<Box<Expr>>),
    Lit(Lit),
    Call(Call),
    Ref(Ref),
    Add(Box<Expr>, Box<Expr>),
    Empty,
}

impl Expr {
    pub fn evaluate_type(&self, ctx: &mut ScopeCtx) -> Option<Type> {
        match &self {
            Self::Ret(ret) => {
                if let Some(ret) = ret {
                    ret.evaluate_type(ctx)
                } else {
                    None
                }
            }
            Self::Lit(lit) => lit.ty.clone(),
            Self::Call(call) => match &call.output {
                FnType::Ty(ty) => Some(ty.clone()),
                FnType::Void => None,
            },
            // Self::Ref(r) => {}
            Self::Empty => None,
            Self::Add(lhs, rhs) => {
                let lhs = lhs.evaluate_type(ctx);
                let rhs = rhs.evaluate_type(ctx);
                assert_eq!(lhs, rhs);

                rhs
            }
            Self::Ref(r) => Some(r.ty.clone()),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, Default)]
pub struct ScopeCtx {
    pub vars: Vec<Var>,
    pub fns: Vec<FnDecl>,
}

impl ScopeCtx {
    pub fn find_var(&self, ident: &Ident) -> Option<&Var> {
        self.vars.iter().find(|v| &v.ident == ident)
    }

    pub fn find_fn(&self, ident: &Ident) -> Option<&FnDecl> {
        self.fns.iter().find(|v| &v.ident == ident)
    }
}

// TODO: store ref as pointer to initial assignment?
#[derive(Debug)]
pub struct Ref {
    pub ident: Ident,
    pub ty: Type,
}

pub struct Pgrm {
    items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Fn(Func),
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Let(Let),
    Item(Item),
    InlineAsm(String),
    Empty,
}

#[derive(Debug)]
pub struct Scope {
    pub stmts: Vec<Stmt>,
}

#[allow(unused)]
pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
    }

    fn visit_pat(&mut self, pat: &mut Pat) {
        walk_pat(self, pat);
    }

    fn visit_fn(&mut self, f: &mut Func) {
        walk_fn(self, f);
    }

    fn visit_fn_body(&mut self, body: &mut Scope) {
        walk_fn_body(self, body);
    }

    fn visit_scope(&mut self, scope: &mut Scope) {
        walk_scope(self, scope);
    }

    fn visit_call(&mut self, call: &mut Call) {
        walk_call(self, call);
    }

    fn visit_ref(&mut self, r: &mut Ref) {
        walk_ref(self, r);
    }

    fn visit_lit(&mut self, lit: &mut Lit) {
        walk_lit(self, lit);
    }

    fn visit_fn_decl(&mut self, decl: &mut FnDecl) {
        walk_fn_decl(self, decl);
    }

    fn visit_fn_param(&mut self, param: &mut Param) {
        walk_fn_param(self, param);
    }

    fn visit_return(&mut self, ret: &mut Expr) {
        walk_return(self, ret);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_let(&mut self, lt: &mut Let) {
        walk_let(self, lt);
    }

    fn visit_item(&mut self, item: &mut Item) {
        walk_item(self, item);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &mut Expr) {
    match expr {
        Expr::Lit(lit) => visitor.visit_lit(lit),
        Expr::Call(call) => visitor.visit_call(call),
        Expr::Ret(ret) => {
            if let Some(ret) = ret {
                visitor.visit_return(ret);
            }
        }
        Expr::Ref(r) => visitor.visit_ref(r),
        Expr::Empty => {}
        Expr::Add(rhs, lhs) => {
            visitor.visit_expr(rhs);
            visitor.visit_expr(lhs);
        }
    }
}

pub fn walk_ref<V: Visitor>(visitor: &mut V, r: &mut Ref) {}

pub fn walk_call<V: Visitor>(visitor: &mut V, call: &mut Call) {}

pub fn walk_pat<V: Visitor>(visitor: &mut V, pat: &mut Pat) {
    // match pat {
    //     _ => panic!(),
    // }
}

pub fn walk_fn<V: Visitor>(visitor: &mut V, f: &mut Func) {
    visitor.visit_fn_decl(&mut f.decl);
    visitor.visit_fn_body(&mut f.body);
}

pub fn walk_fn_body<V: Visitor>(visitor: &mut V, body: &mut Scope) {
    visitor.visit_scope(body);
}

pub fn walk_fn_decl<V: Visitor>(visitor: &mut V, decl: &mut FnDecl) {
    let FnDecl { input, .. } = decl;

    for param in input.iter_mut() {
        visitor.visit_fn_param(param);
    }
}

pub fn walk_fn_param<V: Visitor>(visitor: &mut V, param: &mut Param) {}

pub fn walk_return<V: Visitor>(visitor: &mut V, expr: &mut Expr) {
    visitor.visit_expr(expr);
}

pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &mut Stmt) {
    match stmt {
        Stmt::Let(lt) => visitor.visit_let(lt),
        Stmt::Expr(expr) => visitor.visit_expr(expr),
        Stmt::Item(item) => visitor.visit_item(item),
        Stmt::InlineAsm(_) => {}
        Stmt::Empty => {}
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &mut Item) {
    match item {
        Item::Fn(f) => visitor.visit_fn(f),
    }
}

pub fn walk_scope<V: Visitor>(visitor: &mut V, scope: &mut Scope) {
    for expr in scope.stmts.iter_mut() {
        visitor.visit_stmt(expr);
    }
}

pub fn walk_lit<V: Visitor>(visitor: &mut V, lit: &mut Lit) {
    // match lit.kind {
    //     LitKind::Int =>
    // }
}

pub fn walk_let<V: Visitor>(visitor: &mut V, lt: &mut Let) {}

// #[derive(Debug, Default)]
// struct VarVisitor {
//     vars: Vec<(Ident, Option<Type>)>,
//     fn_return_ty: FnType,
// }
//
// impl VarVisitor {
//     pub fn remaining_unknown_types(&self) -> usize {
//         self.vars.iter().filter(|(_, t)| t.is_none()).count()
//     }
// }
//
// impl VarVisitor {
//     pub fn set_var_ty(&mut self, ident: &Ident, ty: Type) {
//         for (i, t) in self.vars.iter_mut() {
//             if i == ident {
//                 *t = Some(ty);
//             }
//         }
//     }
// }
//
// impl Visitor for VarVisitor {
//     fn visit_fn_decl(&mut self, decl: &mut FnDecl) {
//         self.fn_return_ty = decl.output;
//     }
//
//     fn visit_fn_param(&mut self, param: &mut Param) {
//         self.vars.push((param.ident.clone(), Some(param.ty)));
//     }
//
//     fn visit_let(&mut self, lt: &mut Let) {
//         let ty = if let Some(ty) = lt.ty {
//             Some(ty)
//         } else {
//             match &lt.pat {
//                 Pat::Ident(ident) => {
//                     if let Some(ty) = self
//                         .vars
//                         .iter()
//                         .find_map(|(i, ty)| if i == ident { *ty } else { None })
//                     {
//                         Some(ty)
//                     } else {
//                         panic!("variable is not declared in the local scope");
//                     }
//                 }
//                 Pat::Call(_) => None,
//                 Pat::Lit(_) => None,
//             }
//         };
//
//         self.vars.push((lt.ident.clone(), ty))
//     }
// }
//
// struct VarSetterVisitor {
//     vars: Vec<(Ident, Option<Type>)>,
// }
//
// impl Visitor for VarSetterVisitor {
//     fn visit_expr(&mut self, expr: &mut Expr) {
//         match expr {
//             Expr::Let(lt) => {
//                 if let Some(ty) = self
//                     .vars
//                     .iter()
//                     .find_map(|(i, ty)| if i == &lt.ident { *ty } else { None })
//                 {
//                     lt.ty = Some(ty);
//                 } else {
//                     println!("visiting wrong function");
//                 }
//             }
//             _ => {}
//         }
//     }
// }
//
// #[derive(Debug, Default)]
// struct TyInferVisitor {
//     fn_decls: Vec<FnDecl>,
//     vars: HashMap<Ident, VarVisitor>,
//     passes: usize,
//     is_setting: bool,
// }
//
// impl TyInferVisitor {
//     pub fn more_to_infer(&mut self) -> bool {
//         self.passes += 1;
//         let remaining = self
//             .vars
//             .values()
//             .map(|v| v.remaining_unknown_types())
//             .sum::<usize>();
//
//         if self.passes > 1000 {
//             panic!("inference recursion limit reached, could not infer var");
//         }
//
//         remaining > 0 || self.passes == 1
//     }
//
//     pub fn set_inferred_types(&mut self, scope: &mut Scope) {
//         self.is_setting = true;
//         self.visit_scope(scope);
//     }
// }
//
// impl Visitor for TyInferVisitor {
//     fn visit_fn(&mut self, f: &mut Fn) {
//         if self.is_setting {
//             let var = self.vars.remove(&f.decl.ident).unwrap();
//             let mut setter = VarSetterVisitor { vars: var.vars };
//             setter.visit_fn(f);
//         } else {
//             if self.passes <= 1 {
//                 let mut visitor = VarVisitor::default();
//                 visitor.visit_fn(f);
//                 self.vars.insert(f.decl.ident.clone(), visitor);
//             }
//
//             self.fn_decls.push(f.decl.clone());
//             self.visit_fn_body(&mut f.body);
//         }
//     }
//
//     fn visit_return(&mut self, ret: &mut Pat) {
//         if !self.is_setting {
//             let fn_return_ty = self.fn_decls.last().unwrap().output;
//             let fn_return_ty = match fn_return_ty {
//                 FnType::Ty(ty) => ty,
//                 FnType::Void => panic!("returning value in void fn"),
//             };
//             let fn_name = self.fn_decls.last().as_ref().unwrap().ident.clone();
//
//             match ret {
//                 Pat::Ident(ident) => {
//                     if let Some(var) = self.vars.get_mut(&fn_name) {
//                         var.set_var_ty(ident, fn_return_ty);
//                     }
//                 }
//                 Pat::Lit(lit) => {
//                     lit.ty = Some(fn_return_ty);
//                 }
//                 Pat::Call(_) => {}
//             }
//         }
//     }
//
//     fn visit_let(&mut self, lt: &mut Let) {
//         if !self.is_setting {
//             let fn_return_ty = self.fn_decls.last().unwrap().output;
//             let fn_return_ty = match fn_return_ty {
//                 FnType::Ty(ty) => ty,
//                 FnType::Void => panic!("returning value in void fn"),
//             };
//             let fn_name = self.fn_decls.last().as_ref().unwrap().ident.clone();
//
//             match &lt.pat {
//                 Pat::Call(call) => {
//                     if let Some(decl) = self.fn_decls.iter().find(|f| f.ident == call.ident) {
//                         if let Some(var) = self.vars.get_mut(&fn_name) {
//                             var.set_var_ty(
//                                 &lt.ident,
//                                 match decl.output {
//                                     FnType::Ty(ty) => ty,
//                                     FnType::Void => panic!("attemping to assign var to void fn"),
//                                 },
//                             );
//                         }
//                     }
//                 }
//                 Pat::Ident(_) => {}
//                 Pat::Lit(_) => {}
//             }
//         }
//     }
// }

pub fn parse_ast(scope: &mut Scope) {
    // let mut visitor = TyInferVisitor::default();
    // visitor.visit_scope(scope);
    // while visitor.more_to_infer() {
    //     visitor.visit_scope(scope);
    // }
    //
    // visitor.set_inferred_types(scope);

    // panic!("{visitor:#?}\n{scope:#?}");
}
