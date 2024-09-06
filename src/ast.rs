use std::collections::HashMap;

#[derive(Debug)]
pub struct Fn {
    pub decl: FnDecl,
    pub body: Scope,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub ident: Ident,
    pub input: Vec<Param>,
    pub output: FnType,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub ident: Ident,
    pub ty: Type,
    // pub pat: Pat,
}

#[derive(Debug)]
pub enum Pat {
    Ident(Ident),
    Lit(Lit),
    Call(Call),
}

#[derive(Debug)]
pub struct Call {
    pub ident: Ident,
    pub input: Vec<Pat>,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum FnType {
    #[default]
    Void,
    Ty(Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Self::I32 => 4,
        }
    }

    pub fn keyword(&self) -> &'static str {
        match self {
            Self::I32 => "dword",
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum LitKind {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

#[derive(Debug, PartialEq, Eq)]
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
    pub ident: Ident,
    pub ty: Option<Type>,
    pub pat: Pat,
}

#[derive(Debug)]
pub enum Expr {
    Fn(Fn),
    Return(Pat),
    Lit(Lit),
    Let(Let),
}

#[derive(Debug)]
pub struct Scope {
    pub exprs: Vec<Expr>,
}

#[allow(unused)]
pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
    }

    fn visit_pat(&mut self, pat: &mut Pat) {
        walk_pat(self, pat);
    }

    fn visit_fn(&mut self, f: &mut Fn) {
        walk_fn(self, f);
    }

    fn visit_fn_body(&mut self, body: &mut Scope) {
        walk_fn_body(self, body);
    }

    fn visit_scope(&mut self, scope: &mut Scope) {
        walk_scope(self, scope);
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

    fn visit_return(&mut self, ret: &mut Pat) {
        walk_return(self, ret);
    }

    fn visit_let(&mut self, lt: &mut Let) {
        walk_let(self, lt);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &mut Expr) {
    match expr {
        Expr::Fn(f) => visitor.visit_fn(f),
        Expr::Return(ret) => visitor.visit_return(ret),
        Expr::Lit(lit) => visitor.visit_lit(lit),
        Expr::Let(lt) => visitor.visit_let(lt),
    }
}

pub fn walk_pat<V: Visitor>(visitor: &mut V, pat: &mut Pat) {
    // match pat {
    //     _ => panic!(),
    // }
}

pub fn walk_fn<V: Visitor>(visitor: &mut V, f: &mut Fn) {
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

pub fn walk_return<V: Visitor>(visitor: &mut V, ret: &mut Pat) {
    visitor.visit_pat(ret);
}

pub fn walk_scope<V: Visitor>(visitor: &mut V, scope: &mut Scope) {
    for expr in scope.exprs.iter_mut() {
        visitor.visit_expr(expr);
    }
}

pub fn walk_lit<V: Visitor>(visitor: &mut V, lit: &mut Lit) {
    // match lit.kind {
    //     LitKind::Int =>
    // }
}

pub fn walk_let<V: Visitor>(visitor: &mut V, lt: &mut Let) {}

#[derive(Debug, Default)]
struct VarVisitor {
    vars: Vec<(Ident, Option<Type>)>,
    fn_return_ty: FnType,
}

impl VarVisitor {
    pub fn remaining_unknown_types(&self) -> usize {
        self.vars.iter().filter(|(_, t)| t.is_none()).count()
    }
}

impl VarVisitor {
    pub fn set_var_ty(&mut self, ident: &Ident, ty: Type) {
        for (i, t) in self.vars.iter_mut() {
            if i == ident {
                *t = Some(ty);
            }
        }
    }
}

impl Visitor for VarVisitor {
    fn visit_fn_decl(&mut self, decl: &mut FnDecl) {
        self.fn_return_ty = decl.output;
    }

    fn visit_fn_param(&mut self, param: &mut Param) {
        self.vars.push((param.ident.clone(), Some(param.ty)));
    }

    fn visit_let(&mut self, lt: &mut Let) {
        let ty = if let Some(ty) = lt.ty {
            Some(ty)
        } else {
            match &lt.pat {
                Pat::Ident(ident) => {
                    if let Some(ty) = self
                        .vars
                        .iter()
                        .find_map(|(i, ty)| if i == ident { *ty } else { None })
                    {
                        Some(ty)
                    } else {
                        panic!("variable is not declared in the local scope");
                    }
                }
                Pat::Call(_) => None,
                Pat::Lit(_) => None,
            }
        };

        self.vars.push((lt.ident.clone(), ty))
    }
}

struct VarSetterVisitor {
    vars: Vec<(Ident, Option<Type>)>,
}

impl Visitor for VarSetterVisitor {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Let(lt) => {
                if let Some(ty) = self
                    .vars
                    .iter()
                    .find_map(|(i, ty)| if i == &lt.ident { *ty } else { None })
                {
                    lt.ty = Some(ty);
                } else {
                    println!("visiting wrong function");
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Default)]
struct TyInferVisitor {
    fn_decls: Vec<FnDecl>,
    vars: HashMap<Ident, VarVisitor>,
    passes: usize,
    is_setting: bool,
}

impl TyInferVisitor {
    pub fn more_to_infer(&mut self) -> bool {
        self.passes += 1;
        let remaining = self
            .vars
            .values()
            .map(|v| v.remaining_unknown_types())
            .sum::<usize>();

        if self.passes > 1000 {
            panic!("inference recursion limit reached, could not infer var");
        }

        remaining > 0 || self.passes == 1
    }

    pub fn set_inferred_types(&mut self, scope: &mut Scope) {
        self.is_setting = true;
        self.visit_scope(scope);
    }
}

impl Visitor for TyInferVisitor {
    fn visit_fn(&mut self, f: &mut Fn) {
        if self.is_setting {
            let var = self.vars.remove(&f.decl.ident).unwrap();
            let mut setter = VarSetterVisitor { vars: var.vars };
            setter.visit_fn(f);
        } else {
            if self.passes <= 1 {
                let mut visitor = VarVisitor::default();
                visitor.visit_fn(f);
                self.vars.insert(f.decl.ident.clone(), visitor);
            }

            self.fn_decls.push(f.decl.clone());
            self.visit_fn_body(&mut f.body);
        }
    }

    fn visit_return(&mut self, ret: &mut Pat) {
        if !self.is_setting {
            let fn_return_ty = self.fn_decls.last().unwrap().output;
            let fn_return_ty = match fn_return_ty {
                FnType::Ty(ty) => ty,
                FnType::Void => panic!("returning value in void fn"),
            };
            let fn_name = self.fn_decls.last().as_ref().unwrap().ident.clone();

            match ret {
                Pat::Ident(ident) => {
                    if let Some(var) = self.vars.get_mut(&fn_name) {
                        var.set_var_ty(ident, fn_return_ty);
                    }
                }
                Pat::Lit(lit) => {
                    lit.ty = Some(fn_return_ty);
                }
                Pat::Call(_) => {}
            }
        }
    }

    fn visit_let(&mut self, lt: &mut Let) {
        if !self.is_setting {
            let fn_return_ty = self.fn_decls.last().unwrap().output;
            let fn_return_ty = match fn_return_ty {
                FnType::Ty(ty) => ty,
                FnType::Void => panic!("returning value in void fn"),
            };
            let fn_name = self.fn_decls.last().as_ref().unwrap().ident.clone();

            match &lt.pat {
                Pat::Call(call) => {
                    if let Some(decl) = self.fn_decls.iter().find(|f| f.ident == call.ident) {
                        if let Some(var) = self.vars.get_mut(&fn_name) {
                            var.set_var_ty(
                                &lt.ident,
                                match decl.output {
                                    FnType::Ty(ty) => ty,
                                    FnType::Void => panic!("attemping to assign var to void fn"),
                                },
                            );
                        }
                    }
                }
                Pat::Ident(_) => {}
                Pat::Lit(_) => {}
            }
        }
    }
}

pub fn parse_ast(scope: &mut Scope) {
    let mut visitor = TyInferVisitor::default();
    visitor.visit_scope(scope);
    while visitor.more_to_infer() {
        visitor.visit_scope(scope);
    }

    visitor.set_inferred_types(scope);

    // panic!("{visitor:#?}\n{scope:#?}");
}
