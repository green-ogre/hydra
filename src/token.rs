use crate::ast::{Call, Delim, Expr, Fn, FnDecl, FnType, Ident, Let, Lit, Param, Pat, Scope, Type};
use std::{
    error::Error,
    fmt::{Debug, Display, Write},
};

#[derive(Debug)]
pub struct TmpError;

impl Display for TmpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("temp error")
    }
}
impl Error for TmpError {}

#[derive(Debug)]
pub struct Token {
    ty: TokenType,
    // val: Option<TokenVal>,
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

#[derive(Debug)]
pub enum TokenVal {
    Ident(Ident),
    Lit(Lit),
    Type(Type),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
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

    pub fn expect(&mut self, ty: TokenType) -> Result<(), TmpError> {
        let token = self.tokens.pop().unwrap();

        if token.ty == ty {
            Ok(())
        } else {
            Err(TmpError)
        }
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
    parse_scope(&mut tokenizer)
}

fn parse_scope(tokenizer: &mut Tokenizer) -> Result<Scope, Box<Err>> {
    let mut exprs = Vec::new();

    tokenizer.expect(TokenType::OpenDelim(Delim::Curly))?;
    // while !tokenizer.is_next(TokenType::CloseDelim(Delim::Curly)) {
    while let Some(next) = tokenizer.peek_next() {
        match &next.ty {
            // TokenType::Fn => {
            //     exprs.push(parse_function(tokenizer)?);
            // }
            TokenType::Fn | TokenType::Let | TokenType::Return => {
                exprs.push(parse_expression(tokenizer)?);
            }
            TokenType::CloseDelim(Delim::Curly) => {
                break;
            }
            t => {
                println!("{t:?}");
                let _ = tokenizer.next_token();
            }
        }
    }
    // }
    tokenizer.expect(TokenType::CloseDelim(Delim::Curly))?;

    Ok(Scope { exprs })
}

fn parse_expression(tokenizer: &mut Tokenizer) -> Result<Expr, Box<Err>> {
    if let Some(token) = tokenizer.peek_next() {
        match token.ty {
            TokenType::Fn => parse_function(tokenizer),
            TokenType::Return => parse_return(tokenizer),
            TokenType::Let => parse_let(tokenizer),
            _ => {
                println!("parse_expression: {token:?}");
                Err(Box::new(TmpError))
            }
        }
    } else {
        panic!();
        Err(Box::new(TmpError))
    }
}

fn parse_function(tokenizer: &mut Tokenizer) -> Result<Expr, Box<Err>> {
    tokenizer.expect(TokenType::Fn)?;
    let ident = tokenizer.next_ident()?;

    let mut input = Vec::new();
    tokenizer.expect(TokenType::OpenDelim(Delim::Paren))?;
    while !tokenizer.is_next(TokenType::CloseDelim(Delim::Paren)) {
        let ident = tokenizer.next_ident()?;
        tokenizer.expect(TokenType::Colon)?;
        let ty = tokenizer.next_ty()?;
        input.push(Param { ident, ty })
    }
    tokenizer.expect(TokenType::CloseDelim(Delim::Paren))?;

    let output = if tokenizer.is_next(TokenType::Colon) {
        tokenizer.expect(TokenType::Colon)?;
        tokenizer.expect(TokenType::Colon)?;
        FnType::Ty(tokenizer.next_ty()?)
    } else {
        FnType::Void
    };

    let body = parse_scope(tokenizer)?;

    Ok(Expr::Fn(Fn {
        decl: FnDecl {
            ident,
            input,
            output,
        },
        body,
    }))
}

fn parse_return(tokenizer: &mut Tokenizer) -> Result<Expr, Box<Err>> {
    tokenizer.expect(TokenType::Return)?;
    let Some(token) = tokenizer.next_token() else {
        return Err(Box::new(TmpError));
    };

    match token.ty {
        TokenType::Lit(lit) => Ok(Expr::Return(Pat::Lit(lit))),
        TokenType::Ident(ident) => Ok(Expr::Return(Pat::Ident(ident))),
        _ => {
            println!("parse_return: {token:?}");
            Err(Box::new(TmpError))
        }
    }
}

fn parse_let(tokenizer: &mut Tokenizer) -> Result<Expr, Box<Err>> {
    tokenizer.expect(TokenType::Let)?;
    let ident = tokenizer.next_ident()?;

    let ty = if tokenizer.is_next(TokenType::Colon) {
        tokenizer.expect(TokenType::Colon)?;
        Some(tokenizer.next_ty()?)
    } else {
        None
    };

    tokenizer.expect(TokenType::Equals)?;

    let Some(token) = tokenizer.next_token() else {
        return Err(Box::new(TmpError));
    };
    let pat = match token.ty {
        TokenType::Lit(lit) => Pat::Lit(lit),
        TokenType::Ident(ident) => {
            if tokenizer.is_next(TokenType::OpenDelim(Delim::Paren)) {
                let mut input = Vec::new();
                while !tokenizer.is_next(TokenType::CloseDelim(Delim::Paren)) {
                    // TODO: parse as expr
                    let _ = tokenizer.next_token();
                    // tokenizer.expect(TokenType::Colon)?;
                    // let ty = tokenizer.next_ty()?;
                    // input.push(Param { ident, ty })
                }
                tokenizer.expect(TokenType::CloseDelim(Delim::Paren))?;

                Pat::Call(Call {
                    ident: ident.clone(),
                    input,
                })
            } else {
                Pat::Ident(ident)
            }
        }
        _ => {
            println!("parse_let: {token:?}");
            return Err(Box::new(TmpError));
        }
    };

    tokenizer.expect(TokenType::SemiColon)?;

    Ok(Expr::Let(Let { ident, ty, pat }))
}
