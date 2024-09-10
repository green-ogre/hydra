use crate::ast::*;
use core::panic;
use std::{
    error::Error,
    fmt::{Debug, Display},
};

#[derive(Debug)]
pub struct TmpError;

impl Display for TmpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("temp error")
    }
}
impl Error for TmpError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    ty: TokenType,
}

impl Token {
    pub fn new(ty: TokenType) -> Self {
        Self { ty }
    }

    pub fn ident(ident: Ident) -> Self {
        Self {
            ty: TokenType::Ident(ident),
        }
    }

    pub fn lit(lit: Lit) -> Self {
        Self {
            ty: TokenType::Lit(lit),
        }
    }

    pub fn ty(t: Type) -> Self {
        Self {
            ty: TokenType::Type(t),
        }
    }

    pub fn extract_ident(self) -> Option<Ident> {
        match self.ty {
            TokenType::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn extract_lit(self) -> Option<Lit> {
        match self.ty {
            TokenType::Lit(lit) => Some(lit),
            _ => None,
        }
    }

    pub fn extract_ty(self) -> Option<Type> {
        match self.ty {
            TokenType::Type(ty) => Some(ty),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Plus,
    Let,
    Equals,
    Return,
    Fn,
    SemiColon,
    Colon,
    Comma,
    OpenDelim(Delim),
    CloseDelim(Delim),
    Lit(Lit),
    Type(Type),
    Ident(Ident),
    Ptr,
    InlineAsm(String),
}

#[derive(Debug)]
struct Tokenizer {
    tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();

        Self { tokens }
    }

    pub fn get(&self, index: usize) -> Option<&Token> {
        let index = (self.tokens.len() - 1).saturating_sub(index);
        if self.tokens.len() > 0 {
            Some(&self.tokens[index])
        } else {
            None
        }
    }

    fn slice_until(&self, ty: TokenType) -> Option<&[Token]> {
        if let Some(index) = self.find(ty) {
            let (_, until) = self.tokens.split_at(self.tokens.len() - index);
            // self.tokens = end.to_vec();
            Some(until)
        } else {
            None
        }
    }

    pub fn take_until(&mut self, ty: TokenType) -> Result<Tokenizer, Box<TmpError>> {
        if let Some(result) = self.slice_until(ty.clone()) {
            let result = result.to_vec();
            self.skip(self.find(ty).unwrap());
            Ok(Tokenizer { tokens: result })
        } else {
            Err(Box::new(TmpError))
        }
    }

    pub fn skip(&mut self, n: usize) {
        if n > self.tokens.len() {
            self.tokens.clear();
        } else {
            for _ in 0..n {
                let _ = self.next_token();
            }
        }
    }

    fn find(&self, ty: TokenType) -> Option<usize> {
        self.tokens
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, t)| if t.ty == ty { Some(i) } else { None })
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    pub fn peek_next(&self) -> Option<&Token> {
        if let Some(last) = self.tokens.last() {
            Some(last)
        } else {
            None
        }
    }

    pub fn expect(&mut self, ty: TokenType) -> Result<&mut Self, TmpError> {
        let token = self.tokens.pop();

        if token.map(|t| t.ty) == Some(ty) {
            Ok(self)
        } else {
            Err(TmpError)
        }
    }

    pub fn parse_while(
        &mut self,
        w: impl Fn(&Token) -> bool,
        mut f: impl FnMut(&mut Tokenizer) -> Result<(), Box<Err>>,
    ) -> Result<&mut Self, Box<Err>> {
        while let Some(t) = self.peek_next() {
            if w(t) {
                f(self)?;
            } else {
                break;
            }
        }

        Ok(self)
    }

    pub fn parse_expr(&mut self, ctx: &mut ScopeCtx) -> Result<Expr, Box<Err>> {
        parse_expr(self, ctx)
    }

    pub fn next_ident(&mut self) -> Result<Ident, TmpError> {
        self.tokens
            .pop()
            .and_then(|t| t.extract_ident())
            .ok_or_else(|| TmpError)
    }

    pub fn next_lit(&mut self) -> Result<Lit, TmpError> {
        self.tokens
            .pop()
            .and_then(|t| t.extract_lit())
            .ok_or_else(|| TmpError)
    }

    pub fn next_ty(&mut self) -> Result<Type, TmpError> {
        self.tokens
            .pop()
            .and_then(|t| t.extract_ty())
            .ok_or_else(|| TmpError)
    }

    pub fn is_next_then<O>(
        &mut self,
        ty: TokenType,
        mut f: impl FnMut(&mut Tokenizer) -> Result<O, Box<Err>>,
    ) -> Result<Option<O>, Box<Err>> {
        if self.is_next(ty) {
            Ok(Some(f(self)?))
        } else {
            Ok(None)
        }
    }

    pub fn is_next(&self, ty: TokenType) -> bool {
        if let Some(t) = self.peek_next() {
            t.ty == ty
        } else {
            false
        }
    }
}

pub type Err = dyn Error + 'static;

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Scope, Box<Err>> {
    let mut tokenizer = Tokenizer::new(tokens);
    parse_scope(&mut tokenizer, None)
}

fn parse_scope(tokenizer: &mut Tokenizer, ctx: Option<ScopeCtx>) -> Result<Scope, Box<Err>> {
    let mut stmts = Vec::new();
    let mut ctx = ctx.unwrap_or_default();

    let mut fn_tokenizer = Tokenizer {
        tokens: tokenizer.tokens.clone(),
    };

    let next_fn = fn_tokenizer.find(TokenType::Fn);
    if let Some(next_fn) = next_fn {
        fn_tokenizer.skip(next_fn);
        loop {
            match parse_fn_decl(&mut fn_tokenizer) {
                Ok(fn_decl) => {
                    // for input in fn_decl.input.iter() {
                    //     ctx.vars.push(Var {
                    //         ident: input.ident.clone(),
                    //         ty: input.ty,
                    //     })
                    // }
                    // TODO: wasting work here
                    if !ctx.fns.contains(&fn_decl) {
                        ctx.fns.push(fn_decl);
                    }

                    let next_fn = fn_tokenizer.find(TokenType::Fn);
                    if let Some(next_fn) = next_fn {
                        fn_tokenizer.skip(next_fn);
                    } else {
                        break;
                    }
                }
                Err(e) => panic!("{:?}", e),
            }
        }
    }

    tokenizer
        .expect(TokenType::OpenDelim(Delim::Curly))
        .unwrap();
    while let Some(token) = tokenizer.peek_next() {
        if token.ty == TokenType::CloseDelim(Delim::Curly) {
            break;
        }

        stmts.push(parse_stmt(tokenizer, &mut ctx).unwrap());
    }
    tokenizer
        .expect(TokenType::CloseDelim(Delim::Curly))
        .unwrap();

    Ok(Scope { stmts })
}

// let i = 12 + 1;
//
// let <pat>[: <ty>] = <epxr>
// let <Pat::Ident(i)> = <Expr::Add(Expr::Lit(12), Expr::Lit(1))>
//
// let i = add() + 24;
//
// let <Pat::Ident(i)> = <Expr::Add(Expr::Call( add {...} ), Expr::Lit(24))>
//
// let i = add() + other;
//
// let <Pat::Ident(i)> = <Expr::Add(Expr::Call( add {...} ), Expr::Ref(other))>

fn parse_call(
    tokenizer: &mut Tokenizer,
    ident: &Ident,
    ctx: &mut ScopeCtx,
) -> Result<Expr, Box<Err>> {
    let mut input = Vec::new();
    tokenizer
        .expect(TokenType::OpenDelim(Delim::Paren))?
        .parse_while(
            |token| token.ty != TokenType::CloseDelim(Delim::Paren),
            |tokenizer| {
                let is_ptr = if tokenizer.is_next(TokenType::Ptr) {
                    let _ = tokenizer.next_token();
                    true
                } else {
                    false
                };

                let ident = tokenizer.next_ident()?;

                let ty = if is_ptr {
                    Type::Ptr(Box::new(ctx.find_var(&ident).unwrap().ty.clone()))
                } else {
                    ctx.find_var(&ident).unwrap().ty.clone()
                };

                input.push(Param { ty, ident });

                if tokenizer.is_next(TokenType::Comma) {
                    let _ = tokenizer.next_token();
                }

                Ok(())
            },
        )?
        .expect(TokenType::CloseDelim(Delim::Paren))?;

    let mut call = ctx
        .find_fn(&ident)
        .expect(&format!("fn does not exist: {:?}", ident.value()))
        .call();

    for (fn_param, call_input) in call.input.iter().zip(input.iter()) {
        if fn_param.ty != call_input.ty {
            if !call_input.ty.can_coerce(&fn_param.ty) {
                println!("{:?}, {:?}", call_input.ty, fn_param.ty);
                panic!("call input type does not match fn signature");
            }
        }
    }

    if call.input.len() != input.len() {
        panic!(
            "parameter number mismatch: wanted {}, got {}",
            call.input.len(),
            input.len()
        );
    }

    call.input = input;

    Ok(Expr::Call(call))
}

fn parse_pat(tokenizer: &mut Tokenizer) -> Result<Pat, Box<Err>> {
    match tokenizer.next_token().unwrap().ty {
        TokenType::Ident(ident) => match tokenizer.peek_next().unwrap().ty {
            TokenType::SemiColon => Ok(Pat::Ident(ident)),
            _ => panic!(),
        },
        // TokenType::Lit(lit) => Ok(Pat::Lit(lit)),
        _ => panic!("invalid pattern"),
    }
}

fn parse_ty_maybe(tokenizer: &mut Tokenizer) -> Result<Option<Type>, Box<Err>> {
    match tokenizer.peek_next().unwrap().ty {
        TokenType::Colon => {
            let _ = tokenizer.next_token();
            match tokenizer.next_token().unwrap().ty {
                TokenType::Type(ty) => Ok(Some(ty)),
                _ => Err(Box::new(TmpError)),
            }
        }
        _ => Ok(None),
    }
}

fn parse_expr(tokenizer: &mut Tokenizer, ctx: &mut ScopeCtx) -> Result<Expr, Box<Err>> {
    println!("{tokenizer:#?}");
    match &tokenizer.peek_next().map(|t| &t.ty) {
        Some(TokenType::Return) => parse_return(tokenizer, ctx),
        // TODO: check for more here
        Some(TokenType::Lit(_)) => {
            let lit = tokenizer.next_token().unwrap().extract_lit().unwrap();

            match tokenizer.peek_next().map(|t| &t.ty) {
                Some(TokenType::Plus) => {
                    let rhs = tokenizer.expect(TokenType::Plus)?.next_lit()?;
                    Ok(Expr::Add(
                        Box::new(Expr::Lit(lit)),
                        Box::new(Expr::Lit(rhs)),
                    ))
                }
                None => Ok(Expr::Lit(lit)),
                _ => panic!(),
            }
        }
        Some(TokenType::Ident(_)) => {
            // let ident = tokenizer.next_token().unwrap().extract_ident().unwrap();
            // Ok(Expr::Ref(Ref { ident, ty: None }))

            let ident = tokenizer.next_token().unwrap().extract_ident().unwrap();
            println!("{ident:?}");
            match tokenizer.peek_next().map(|t| &t.ty) {
                Some(TokenType::OpenDelim(Delim::Paren)) => parse_call(tokenizer, &ident, ctx),
                // Some(TokenType::SemiColon => {
                //     // let _ = tokenizer.next_token();
                //     Ok(Expr::Ref(Ref {
                //         ident: ident.clone(),
                //         ty: None,
                //     }))
                // }
                Some(TokenType::Plus) => {
                    println!("{ctx:#?}");
                    let lhs = ctx
                        .find_var(&ident)
                        .expect(&format!("var does not exist: {:?}", ident.value()));
                    let rhs = ctx
                        .find_var(&ident)
                        .expect(&format!("var does not exist: {:?}", ident.value()));
                    let rhs_ident = tokenizer.expect(TokenType::Plus)?.next_ident()?;
                    Ok(Expr::Add(
                        Box::new(Expr::Ref(Ref {
                            ident: ident.clone(),
                            ty: lhs.ty.clone(),
                        })),
                        Box::new(Expr::Ref(Ref {
                            ident: rhs_ident.clone(),
                            ty: rhs.ty.clone(),
                        })),
                    ))
                }
                None => {
                    let var = ctx
                        .find_var(&ident)
                        .expect(&format!("var does not exist: {:?}", ident.value()));

                    Ok(Expr::Ref(Ref {
                        ident: ident.clone(),
                        ty: var.ty.clone(),
                    }))
                }
                _ => panic!("invalid expr"),
            }
        }
        Some(TokenType::SemiColon) => Ok(Expr::Empty),
        None => Ok(Expr::Empty),
        _ => panic!("invalid expr"),
    }
}

fn parse_stmt(tokenizer: &mut Tokenizer, ctx: &mut ScopeCtx) -> Result<Stmt, Box<Err>> {
    // println!("parsing stmt: {:#?}", tokenizer.peek_next());
    match &tokenizer.peek_next().unwrap().ty {
        TokenType::Return | TokenType::Lit(_) | TokenType::Ident(_) => {
            let stmt = Stmt::Expr(parse_expr(tokenizer, ctx)?);
            if tokenizer.is_next(TokenType::SemiColon) {
                let _ = tokenizer.next_token();
            } else {
                // panic!("expected semi: {stmt:#?}");
            }
            return Ok(stmt);
        }
        _ => {}
    }

    match &tokenizer.peek_next().unwrap().ty {
        TokenType::Let => parse_let(tokenizer, ctx),
        TokenType::Fn => Ok(Stmt::Item(Item::Fn(parse_fn(tokenizer, ctx).unwrap()))),
        TokenType::InlineAsm(asm) => {
            let asm = asm.clone();
            let _ = tokenizer.next_token();
            Ok(Stmt::InlineAsm(asm))
        }
        t => panic!("parse_stmt: {t:?}"),
    }
}

fn parse_param(tokenizer: &mut Tokenizer) -> Result<Param, Box<Err>> {
    let ident = tokenizer.next_ident()?;
    let ty = tokenizer.expect(TokenType::Colon)?.next_ty()?;
    if tokenizer.is_next(TokenType::Comma) {
        let _ = tokenizer.next_token();
    }

    Ok(Param { ident, ty })
}

fn parse_fn_decl(tokenizer: &mut Tokenizer) -> Result<FnDecl, Box<Err>> {
    let mut input = Vec::new();

    let ident = tokenizer.expect(TokenType::Fn)?.next_ident()?;
    tokenizer
        .expect(TokenType::OpenDelim(Delim::Paren))?
        .parse_while(
            |token| token.ty != TokenType::CloseDelim(Delim::Paren),
            |tokenizer| Ok(input.push(parse_param(tokenizer)?)),
        )?
        .expect(TokenType::CloseDelim(Delim::Paren))?;

    let output = tokenizer
        .is_next_then(TokenType::Colon, |tokenizer| {
            tokenizer
                .expect(TokenType::Colon)?
                .expect(TokenType::Colon)?;
            Ok(FnType::Ty(tokenizer.next_ty().unwrap()))
        })?
        .unwrap_or_else(|| FnType::Void);

    Ok(FnDecl {
        ident,
        input,
        output,
    })
}

fn parse_fn(tokenizer: &mut Tokenizer, ctx: &mut ScopeCtx) -> Result<Func, Box<Err>> {
    let mut new_ctx = ctx.clone();

    let decl = parse_fn_decl(tokenizer)?;
    for input in decl.input.iter() {
        new_ctx.vars.push(Var {
            ident: input.ident.clone(),
            ty: input.ty.clone(),
        })
    }

    let mut body = parse_scope(tokenizer, Some(new_ctx))?;

    if let Some(last) = body.stmts.last() {
        match last {
            Stmt::Expr(expr) => match expr {
                Expr::Ret(_) => {}
                _ => body.stmts.push(Stmt::Expr(Expr::Ret(None))),
            },
            _ => body.stmts.push(Stmt::Expr(Expr::Ret(None))),
        }
    } else {
        body.stmts.push(Stmt::Expr(Expr::Ret(None)));
    }

    Ok(Func { decl, body })
}

fn parse_return(tokenizer: &mut Tokenizer, ctx: &mut ScopeCtx) -> Result<Expr, Box<Err>> {
    let ret = match tokenizer
        .expect(TokenType::Return)?
        .take_until(TokenType::SemiColon)?
        .parse_expr(ctx)?
    {
        Expr::Empty => None,
        expr => Some(Box::new(expr)),
    };
    tokenizer.expect(TokenType::SemiColon)?;

    Ok(Expr::Ret(ret))
}

fn parse_let(tokenizer: &mut Tokenizer, ctx: &mut ScopeCtx) -> Result<Stmt, Box<Err>> {
    let lhs = tokenizer.expect(TokenType::Let)?.next_ident()?;
    let ty_def = parse_ty_maybe(tokenizer)?;

    match tokenizer.next_token().unwrap().ty {
        TokenType::SemiColon => {
            if ty_def.is_none() {
                panic!("uninitialized var must have type declaration: let <var>[: <type>]");
            }

            ctx.vars.push(Var {
                ident: lhs.clone(),
                ty: ty_def.as_ref().unwrap().clone(),
            });

            return Ok(Stmt::Let(Let {
                lhs,
                ty: ty_def.unwrap(),
                rhs: None,
            }));
        }
        TokenType::Equals => {}
        _ => panic!("let: expected either `=` or `;`"),
    }

    let rhs = tokenizer
        .take_until(TokenType::SemiColon)?
        .parse_expr(ctx)?;
    tokenizer.expect(TokenType::SemiColon)?;

    let evaluated = rhs.evaluate_type(ctx);

    if evaluated
        .clone()
        .is_some_and(|t| ty_def.clone().is_some_and(|td| t != td))
    {
        panic!("declared type does not align with rhs: [{:?}]", lhs.value());
    }

    let ty = ty_def.unwrap_or_else(|| {
        evaluated.unwrap_or_else(|| panic!("cannot infer type of [{:?}]", lhs.value()))
    });

    ctx.vars.push(Var {
        ident: lhs.clone(),
        ty: ty.clone(),
    });

    Ok(Stmt::Let(Let {
        lhs,
        ty,
        rhs: Some(rhs),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer() {
        let mut tokenizer = Tokenizer::new(vec![
            Token::new(TokenType::Plus),
            Token::new(TokenType::SemiColon),
            Token::new(TokenType::Equals),
        ]);
        let mut res = tokenizer.take_until(TokenType::SemiColon).unwrap();

        assert_next_tt(&mut tokenizer, TokenType::SemiColon);
        assert_next_tt(&mut tokenizer, TokenType::Equals);
        assert_next_tt(&mut res, TokenType::Plus);

        let tokenizer = Tokenizer::new(vec![
            Token::new(TokenType::Plus),
            Token::new(TokenType::SemiColon),
            Token::new(TokenType::Equals),
        ]);
        let res = tokenizer.slice_until(TokenType::Equals).unwrap();

        assert_eq!(
            res,
            &[
                Token::new(TokenType::SemiColon),
                Token::new(TokenType::Plus),
            ]
        );
    }

    fn assert_next_tt(tokenizer: &mut Tokenizer, ty: TokenType) {
        assert_eq!(tokenizer.next_token().unwrap().ty, ty);
    }
}
