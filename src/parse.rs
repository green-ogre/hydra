use crate::{
    ast::{Delim, Ident, Lit, Type},
    token::{Token, TokenType},
};
use colored::Colorize;

pub fn test_parse() {
    let raw = "{fn main(apple: i32) :: i32 {
            let val = 12;
            return val;
        }}";
    let tokens = parse_raw(raw.into());
    println!("{tokens:#?}");
    let mut scope = crate::token::parse_tokens(tokens).unwrap();
    println!("pre-parse: {scope:#?}");
    crate::ast::parse_ast(&mut scope);
    println!("post-parse: {scope:#?}");
    let output = crate::compiler::compile(scope);
    println!("{output}");
}

pub fn parse_raw(raw: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut buf = String::new();
    let mut is_comment = false;
    for (i, c) in raw.chars().enumerate() {
        if c.is_whitespace() {
            if !is_comment {
                interpret_buf(&mut buf, &mut tokens);
            } else if c == '\n' {
                println!("hello");
                is_comment = false;
                buf.clear();
            }
        } else if c == '/' {
            is_comment = true
        } else if c == ';'
            || c == ':'
            || c == '('
            || c == ')'
            || c == '{'
            || c == '}'
            || c == '='
            || c == ','
        {
            if !is_comment {
                interpret_buf(&mut buf, &mut tokens);
                match c {
                    ';' => tokens.push(Token::new(TokenType::SemiColon)),
                    ':' => tokens.push(Token::new(TokenType::Colon)),
                    '(' => tokens.push(Token::new(TokenType::OpenDelim(Delim::Paren))),
                    ')' => tokens.push(Token::new(TokenType::CloseDelim(Delim::Paren))),
                    '{' => tokens.push(Token::new(TokenType::OpenDelim(Delim::Curly))),
                    '}' => tokens.push(Token::new(TokenType::CloseDelim(Delim::Curly))),
                    '=' => tokens.push(Token::new(TokenType::Equals)),
                    ',' => tokens.push(Token::new(TokenType::Comma)),
                    _ => unreachable!(),
                }
            }
        } else if c.is_alphabetic() || c.is_numeric() {
            buf.push(c);
        } else {
            panic!(
                "invalid ascii in file:\n {}{}{}",
                &raw[0..i],
                // TODO: remove colored dependency
                &raw[i..i + 1].red(),
                &raw[i + 1..]
            );
        }
    }

    tokens
}

fn interpret_buf(buf: &mut String, tokens: &mut Vec<Token>) {
    if buf.trim().is_empty() {
        return;
    }

    match buf.trim() {
        "return" => {
            tokens.push(Token::new(TokenType::Return));
        }
        "let" => {
            tokens.push(Token::new(TokenType::Let));
        }
        "fn" => {
            tokens.push(Token::new(TokenType::Fn));
        }
        ident => {
            if buf.chars().all(|c| c.is_numeric()) {
                tokens.push(Token::lit(Lit::int(buf.clone())));
            } else if buf.chars().next().is_some_and(|c| c.is_alphabetic()) {
                let token = match ident {
                    "i32" => Token::ty(Type::I32),
                    ident => Token::ident(Ident::new(ident.into())),
                };

                tokens.push(token);
            } else {
                panic!(
                    "invalid ident in file:\n {}",
                    // TODO: remove colored dependency
                    buf.red(),
                );
            }
        }
    }

    buf.clear();
}
