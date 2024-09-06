use crate::ast::{
    walk_fn_decl, walk_fn_param, walk_scope, Expr, Fn, FnDecl, Ident, Let, Param, Pat, Scope, Type,
    Visitor,
};
use std::collections::HashMap;

pub fn compile(mut scope: Scope) -> String {
    let mut walker = Ast2AsmWalker::default();
    walker.output.push_str("global _start\n");
    walker.output.push_str("section .text\n");
    walker.output.push('\n');

    walk_scope(&mut walker, &mut scope);
    //println!("{walker:#?}");

    walker.output.push_str("_start:\n");
    walker.output.push_str("\tcall main\n");
    walker.output.push_str("\tmov rdi, rax\n");
    walker.output.push_str("\tmov rax, 60\n");
    walker.output.push_str("\tsyscall\n");

    walker.output
}

#[derive(Debug, Clone)]
struct SVar {
    ident: Ident,
    offset: usize,
    size: usize,
}

#[derive(Debug, Default)]
struct SFrame {
    param: Vec<SVar>,
    local: Vec<SVar>,
}

#[derive(Clone, Copy)]
enum SFrameLocality {
    Local,
    Param,
}

impl SFrame {
    pub fn push(&mut self, var: Ident, ty: Type, locality: SFrameLocality) {
        let frame = match locality {
            SFrameLocality::Local => &mut self.local,
            SFrameLocality::Param => &mut self.param,
        };

        let offset = if let Some(var) = frame.last() {
            var.offset + ty.size()
        } else {
            0
        };

        frame.push(SVar {
            ident: var,
            offset,
            size: ty.size(),
        });
    }

    pub fn get(&self, var: &Ident, locality: SFrameLocality) -> Option<&SVar> {
        let frame = match locality {
            SFrameLocality::Local => &self.local,
            SFrameLocality::Param => &self.param,
        };

        frame
            .iter()
            .find_map(|v| if &v.ident == var { Some(v) } else { None })
    }

    pub fn get_or_push(&mut self, var: &Ident, ty: Type, locality: SFrameLocality) -> SVar {
        let frame = match locality {
            SFrameLocality::Local => &mut self.local,
            SFrameLocality::Param => &mut self.param,
        };

        if let Some(offset) = self.get(&var, locality) {
            offset.clone()
        } else {
            self.push(var.clone(), ty, locality);
            self.local.last().unwrap().clone()
        }
    }

    pub fn size(&self) -> usize {
        if let Some(var) = self.local.last() {
            var.offset + var.size
        } else {
            0
        }
    }

    pub fn size_aligned(&self) -> usize {
        let size = self.size();
        (size / 16 + 1) * 16
    }
}

#[derive(Debug, Default)]
struct SFrameWalker {
    frame: SFrame,
}

impl Visitor for SFrameWalker {
    fn visit_fn_param(&mut self, param: &mut Param) {
        self.frame
            .push(param.ident.clone(), param.ty, SFrameLocality::Param);
        // walk_fn_param(self, param);
    }

    fn visit_let(&mut self, lt: &mut Let) {
        self.frame
            .push(lt.ident.clone(), lt.ty.unwrap(), SFrameLocality::Local);
    }
}

#[derive(Debug)]
struct FnMeta {
    decl: FnDecl,
    frame: SFrameWalker,
}

#[derive(Debug, Default)]
struct Ast2AsmWalker {
    fn_meta: HashMap<Ident, FnMeta>,
    current_fn: Option<Ident>,
    output: String,
}

impl Visitor for Ast2AsmWalker {
    fn visit_fn(&mut self, f: &mut Fn) {
        let mut walker = SFrameWalker::default();
        walker.visit_fn(f);

        self.current_fn = Some(f.decl.ident.clone());
        self.output
            .push_str(&format!("{}:\n", f.decl.ident.value()));
        self.output.push_str("\t;allocating stack\n");
        self.output.push_str("\tpush rbp\n");
        self.output.push_str("\tmov rbp, rsp\n");
        self.output
            .push_str(&format!("\tsub rsp, {}\n\n", walker.frame.size_aligned()));
        self.output.push_str("\t;body\n");

        self.fn_meta.insert(
            f.decl.ident.clone(),
            FnMeta {
                decl: f.decl.clone(),
                frame: walker,
            },
        );
        self.visit_fn_body(&mut f.body);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Fn(f) => self.visit_fn(f),
            Expr::Let(lt) => {
                let var = self
                    .fn_meta
                    .get(self.current_fn.as_ref().unwrap())
                    .unwrap()
                    .frame
                    .frame
                    .get(&lt.ident, SFrameLocality::Local)
                    .unwrap();

                match &lt.pat {
                    Pat::Lit(lit) => {
                        self.output.push_str(&format!(
                            "\tmov {} [rbp-{}], {}\n",
                            usize_to_keyword(var.size),
                            var.offset,
                            lit.val
                        ));
                    }
                    Pat::Call(call) => {
                        self.output.push_str("\t;calling into function\n");
                        self.output
                            .push_str(&format!("\tcall {}\n", call.ident.value()));
                        self.output.push_str("\t;moving result into stack\n");
                        self.output.push_str(&format!(
                            "\tmov {} [rbp-{}], {}\n",
                            usize_to_keyword(var.size),
                            var.offset,
                            usize_to_a_reg(var.size),
                        ));
                    }
                    Pat::Ident(ident) => {
                        let new_var = self
                            .fn_meta
                            .get(self.current_fn.as_ref().unwrap())
                            .unwrap()
                            .frame
                            .frame
                            .get(&ident, SFrameLocality::Local)
                            .unwrap();

                        self.output.push_str(&format!(
                            "\tmov {} [rbp-{}], [rdp-{}]\n",
                            usize_to_keyword(var.size),
                            new_var.offset,
                            var.offset,
                        ));
                    }
                }
            }
            Expr::Return(ret) => {
                self.output.push_str("\n\t;move output into the a reg\n");
                match ret {
                    Pat::Ident(ident) => {
                        let var = self
                            .fn_meta
                            .get(self.current_fn.as_ref().unwrap())
                            .unwrap()
                            .frame
                            .frame
                            .get(&ident, SFrameLocality::Local)
                            .expect(&format!("unknown var: {}", ident.value()));

                        self.output.push_str(&format!(
                            "\tmov {}, [rbp-{}]\n\n",
                            usize_to_a_reg(var.size),
                            var.offset,
                        ));
                    }
                    Pat::Lit(lit) => self.output.push_str(&format!(
                        "\tmov {}, {}\n\n",
                        usize_to_a_reg(lit.ty.unwrap().size()),
                        lit.val
                    )),
                    _ => panic!(),
                }
                self.output.push_str("\t;clean stack\n");
                self.output.push_str("\tleave\n");
                self.output.push_str("\tret\n\n");
            }
            _ => panic!(),
        }
    }
}

fn usize_to_a_reg(size: usize) -> &'static str {
    match size {
        4 => "eax",
        8 => "rax",
        _ => panic!(),
    }
}

fn usize_to_keyword(size: usize) -> &'static str {
    match size {
        4 => "dword",
        8 => "qword",
        _ => panic!(),
    }
}
