use core::panic;

use crate::{
    ast::{ArrayTy, Delim, Ident, Lit, Type},
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
    let mut is_asm = false;

    for (i, c) in raw.chars().enumerate() {
        if c.is_whitespace() {
            if !is_comment && !is_asm {
                if buf == "@asm" {
                    is_asm = true;
                    buf.clear();
                }

                interpret_buf(&mut buf, &mut tokens);
            } else if c == '\n' && !is_asm {
                is_comment = false;
                buf.clear();
            } else if is_asm {
                buf.push(c);
            }
        } else if c == '/' {
            if !is_asm {
                is_comment = true
            }
        } else if c.is_alphabetic()
            || c.is_numeric()
            || c == '_'
            || c == '@'
            || c == '['
            || c == ']'
            || c == '&'
        {
            buf.push(c);
        } else {
            if is_asm {
                if c == '}' {
                    is_asm = false;
                    tokens.push(Token::new(TokenType::InlineAsm(buf.clone())));
                    buf.clear();
                } else if c != '{' {
                    buf.push(c);
                }
            } else if !is_comment {
                interpret_buf(&mut buf, &mut tokens);
                match c {
                    ';' => tokens.push(Token::new(TokenType::SemiColon)),
                    '=' => tokens.push(Token::new(TokenType::Equals)),
                    '+' => tokens.push(Token::new(TokenType::Plus)),
                    ':' => tokens.push(Token::new(TokenType::Colon)),
                    ',' => tokens.push(Token::new(TokenType::Comma)),
                    '{' => tokens.push(Token::new(TokenType::OpenDelim(Delim::Curly))),
                    '}' => tokens.push(Token::new(TokenType::CloseDelim(Delim::Curly))),
                    '(' => tokens.push(Token::new(TokenType::OpenDelim(Delim::Paren))),
                    ')' => tokens.push(Token::new(TokenType::CloseDelim(Delim::Paren))),
                    _ => panic!(
                        "unrecognized symbol:\n {}{}{}",
                        &raw[0..i],
                        // TODO: remove colored dependency
                        &raw[i..i + 1].red(),
                        &raw[i + 1..]
                    ),
                }
            }
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
            buf.clear();
        }
        "let" => {
            tokens.push(Token::new(TokenType::Let));
            buf.clear();
        }
        "fn" => {
            tokens.push(Token::new(TokenType::Fn));
            buf.clear();
        }
        ident => {
            if buf.chars().all(|c| c.is_numeric()) {
                tokens.push(Token::lit(Lit::int(buf.clone())));
                buf.clear();
            } else if buf.ends_with("i32") && *buf != "i32" {
                tokens.push(Token::lit(
                    Lit::int(buf.strip_suffix("i32").unwrap().into()).with_type(Type::I32),
                ));
                buf.clear();
            } else if buf
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
            {
                let token = match ident {
                    "i32" => Token::ty(Type::I32),
                    ident => Token::ident(Ident::new(ident.into())),
                };

                tokens.push(token);
                buf.clear();
            } else if buf.starts_with("&") {
                if buf.contains("[") && buf.contains("]") {
                    let end = buf.find("]").unwrap();
                    let ty = Type::from((&buf[2..end]).to_string());

                    tokens.push(Token::new(TokenType::Type(Type::Slice(Box::new(ty)))));

                    buf.clear();
                } else {
                    tokens.push(Token::new(TokenType::Ptr));
                    // TODO: valid ident
                    tokens.push(Token::new(TokenType::Ident(Ident::new(
                        buf[1..].to_string(),
                    ))));

                    buf.clear();
                }
            } else if buf.starts_with("[") {
                if !buf.ends_with("]") {
                    let len = buf
                        .chars()
                        .skip(1)
                        .take_while(|c| *c != ']')
                        .collect::<String>();
                    let len = len.parse::<usize>().expect("array size is not usize");
                    let ty = buf
                        .chars()
                        .skip(buf.find("]").unwrap() + 1)
                        .collect::<String>();

                    tokens.push(Token::new(TokenType::Type(Type::Array(ArrayTy {
                        len,
                        element: Box::new(Type::from(ty)),
                    }))));

                    buf.clear();
                }
            } else {
                panic!(
                    "invalid ident in file:\n {}",
                    // TODO: remove colored dependency
                    buf.red(),
                );
            }
        }
    }
}
