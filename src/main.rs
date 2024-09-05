use std::process::Command;

use colored::Colorize;

fn main() {
    let mut args = std::env::args();
    let _ex = args.next();
    let path = args.next().expect("file not provided: nova <file.hy>");
    let raw = std::fs::read_to_string(path).unwrap();
    let tokens = tokenize_file(raw);
    // println!("{tokens:#?}");

    let output = compile(tokens);
    std::fs::write("test.asm", output).unwrap();

    let output = Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg("-o")
        .arg("./build/hydra_asm.o")
        .arg("test.asm")
        .output()
        .unwrap();
    println!("{output:?}");
    let output = Command::new("ld")
        .arg("-o")
        .arg("./build/hydra_asm")
        .arg("./build/hydra_asm.o")
        .output()
        .unwrap();
    println!("{output:?}");
    println!(
        "exit code: {:?}",
        Command::new("./build/hydra_asm")
            .output()
            .unwrap()
            .status
            .code()
            .unwrap()
    );
}

#[derive(Debug)]
struct Token {
    ty: TokenType,
}

impl Token {
    pub fn new(ty: TokenType) -> Self {
        Self { ty }
    }
}

#[derive(Debug, Clone)]
enum TokenType {
    Let,
    Equals,
    Return,
    Fn,
    OpenParen,
    ClosedParen,
    OpenCurly,
    ClosedCurly,
    Numeric(String),
    SemiColon,
    Colon,
    Type(Type),
    Ident(String),
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl TokenType {
    pub fn extract_type(&self) -> Type {
        match self {
            TokenType::Type(ty) => *ty,
            _ => panic!(),
        }
    }

    pub fn extract_ident(&self) -> String {
        match self {
            TokenType::Ident(ident) => ident.clone(),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Type {
    // TODO: this to make code look nice elsewhere, but it doesn't make any sense here
    #[default]
    Undefined,
    I32,
}

fn tokenize_file(raw: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut buf = String::new();
    for (i, c) in raw.chars().enumerate() {
        if c.is_whitespace() {
            interpret_buf(&mut buf, &mut tokens);
        } else if c == ';' || c == ':' || c == '(' || c == ')' || c == '{' || c == '}' || c == '=' {
            interpret_buf(&mut buf, &mut tokens);
            match c {
                ';' => tokens.push(Token::new(TokenType::SemiColon)),
                ':' => tokens.push(Token::new(TokenType::Colon)),
                '(' => tokens.push(Token::new(TokenType::OpenParen)),
                ')' => tokens.push(Token::new(TokenType::ClosedParen)),
                '{' => tokens.push(Token::new(TokenType::OpenCurly)),
                '}' => tokens.push(Token::new(TokenType::ClosedCurly)),
                '=' => tokens.push(Token::new(TokenType::Equals)),
                _ => unreachable!(),
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
                tokens.push(Token::new(TokenType::Numeric(buf.clone())));
            } else if buf.chars().next().is_some_and(|c| c.is_alphabetic()) {
                let token = match ident {
                    "i32" => Token::new(TokenType::Type(Type::I32)),
                    ident => Token::new(TokenType::Ident(ident.into())),
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

fn compile(mut tokens: Vec<Token>) -> String {
    let mut output = String::new();

    output.push_str("global _start\n");
    output.push_str("section .text\n");
    output.push('\n');

    tokens.reverse();
    let ast = parse_file(&tokens);

    verify_functions(&ast);

    // print_ast(&ast, 0);
    println!("{ast:#?}");

    assert_eq!(ast.body, Syntax::File);

    for child in ast.children.iter() {
        assert_eq!(
            std::mem::discriminant(&child.body),
            std::mem::discriminant(&Syntax::Function {
                name: Ident(Default::default()),
                return_type: None
            })
        );

        match &child.body {
            Syntax::Function { name, return_type } => {
                output.push_str(&format!("{}:\n", name.0));
                let mut frame = StackFrame::default();

                for child in child.children.iter() {
                    match child.body {
                        Syntax::Scope => {
                            output.push_str(&build_scope(&child, &mut frame));
                        }
                        _ => panic!(),
                    }
                }
                output.push('\n');

                if &name.0 == "main" {
                    output.push_str("_start:\n");
                    output.push_str("\tcall main\n");
                    output.push_str("\tmov edi, eax\n");
                    output.push_str("\tmov rax, 60\n");
                    output.push_str("\tsyscall\n");
                }
            }
            _ => panic!(),
        }
    }

    println!("\n\n{output}\n\n");

    output
}

// fn propogate_types_for_variables(head: &mut Ast) {
//     for child in head.children.iter() {
//         match &child.body {
//             Syntax::Variable(var) => {
//                 propogate_types(head, var);
//             }
//             _ => recur_propogate_types_for_variables(child),
//         }
//     }
// }
//
// fn propogate_types(head: &Ast, var: &Variable) -> Option<Type> {
//     for child in head.children.iter() {
//         match &child.body {
//             Syntax::Assignment(ty) => {
//                 if var.ty.is_none() {
//                     return Some(*ty);
//                 } else {
//                     assert_eq!(*ty, var.ty.unwrap());
//                     return None;
//                 }
//             }
//             _ => {
//                 let val = propogate_types(child, var);
//                 if val.is_some() {
//                     return val;
//                 }
//             }
//         }
//     }
//
//     None
// }

fn verify_functions(head: &Ast) {
    for child in head.children.iter() {
        match &child.body {
            Syntax::Function { name, return_type } => {
                if let Some(ret) = retrieve_return_children(child) {
                    // println!("{ret:#?}");

                    for child in ret.iter() {
                        match &child.body {
                            Syntax::Variable(var) => {
                                // let var = retrieve_variable_from_ident(child, &var.ident);
                                // TODO: verify
                            }
                            Syntax::Literal(lit) => {
                                // TODO: verify
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => verify_functions(child),
        }
    }
}

fn retrieve_variable_from_ident<'a>(head: &'a Ast, ident: &Ident) -> Option<&'a [Box<Ast>]> {
    match &head.body {
        Syntax::Variable(var) => {
            if &var.ident == ident {
                return Some(&head.children);
            }
        }
        _ => {}
    }

    for child in head.children.iter() {
        let val = retrieve_variable_from_ident(child, ident);
        if val.is_some() {
            return val;
        }
    }

    None
}

fn retrieve_return_children(head: &Ast) -> Option<&[Box<Ast>]> {
    if head.body == Syntax::Return {
        return Some(&head.children);
    }

    for child in head.children.iter() {
        let val = retrieve_return_children(child);
        if val.is_some() {
            return val;
        }
    }

    None
}

#[derive(Debug, Default)]
struct StackFrame {
    variables: Vec<(Variable, usize)>,
}

impl StackFrame {
    pub fn push(&mut self, var: Variable) {
        let i = if let Some((var, prev_offset)) = self.variables.last() {
            prev_offset + var.size()
        } else {
            0
        };

        self.variables.push((var, i));
    }

    pub fn get(&self, var: &Variable) -> Option<usize> {
        self.variables
            .iter()
            .find_map(|(v, i)| if v.ident == var.ident { Some(*i) } else { None })
    }

    pub fn get_or_push(&mut self, var: Variable) -> usize {
        if let Some(offset) = self.get(&var) {
            offset
        } else {
            self.push(var);
            self.variables.last().unwrap().1
        }
    }

    pub fn size(&self) -> usize {
        if let Some((v, i)) = self.variables.last() {
            i + v.size()
        } else {
            0
        }
    }
}

fn build_scope(scope: &Ast, frame: &mut StackFrame) -> String {
    let mut output = String::new();
    let mut scope_output = String::new();

    // output.push_str("\tpush rbp\n");
    // output.push_str("\tmov rbp, rsp\n");

    for child in scope.children.iter() {
        match child.body {
            Syntax::Return => {
                // println!("{child:#?}");
                let val = &child.children[0];
                match &val.body {
                    Syntax::Literal(lit) => {
                        // if let Some(ty) = return_type {
                        // match ty {
                        //     Type::I32 => {
                        //         // let val = lit
                        //         //     .numeric()
                        //         //     // TODO: if this is
                        //         //     // verified in a
                        //         //     // previous step, then
                        //         //     // this step would be
                        //         //     // unnecessary since
                        //         //     // the value is already
                        //         //     // described by a
                        //         //     // String.
                        //         //     .parse::<i32>()
                        //         //     .unwrap();
                        //         //

                        scope_output.push_str(&format!("\tmov rax, {}\n", lit.as_string()));

                        //     }
                        //     _ => panic!(),
                        // }
                        // }
                    }
                    Syntax::Variable(var) => {
                        if let Some(offset) = frame.get(var) {
                            scope_output.push_str(&format!("\tmov rax, [rsp+{}]\n", offset))
                        }
                    }
                    _ => panic!(),
                }

                scope_output.push_str(&format!("\tadd rsp, {}\n", frame.size()));
                scope_output.push_str("\tret\n");
            }
            Syntax::Assignment(ty) => {
                println!("yes: {:#?}", child.children);
                let mut offset = 0;
                for ass_child in child.children.iter() {
                    match &ass_child.body {
                        Syntax::Variable(var) => {
                            offset = frame.get_or_push(var.clone());
                        }
                        Syntax::Literal(lit) => {
                            assert_eq!(child.children.len(), 2);
                            let val = lit.as_string();
                            scope_output
                                .push_str(&format!("\tmov dword [rsp+{}], {}\n", offset, val));
                        }
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        }
    }

    println!("{frame:?}");

    // allocate stack
    output.push_str(&format!("\tsub rsp, {}\n", frame.size()));
    output.push_str(&scope_output);

    output
}

fn build_exit(exit_code: i32) -> String {
    let mut output = String::new();
    output.push_str("\tmov rax, 60\n");
    output.push_str(&format!("\tmov rdi, {}\n", exit_code));
    output.push_str("\tsyscall\n");

    output
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Ident(String);

#[derive(Debug)]
struct Literal(TokenType);

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        if std::mem::discriminant(&self.0) == std::mem::discriminant(&other.0) {
            match &self.0 {
                TokenType::Numeric(val1) => match &other.0 {
                    TokenType::Numeric(val2) => val1 == val2,
                    _ => false,
                },
                _ => unreachable!(),
            }
        } else {
            false
        }
    }
}

impl Literal {
    pub fn as_string(&self) -> String {
        match &self.0 {
            TokenType::Numeric(val) => val.clone(),
            _ => panic!(),
        }
    }

    pub fn numeric(&self) -> String {
        match &self.0 {
            TokenType::Numeric(val) => val.clone(),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    ident: Ident,
    ty: Type,
}

impl Variable {
    pub fn from_ident(ident: Ident) -> Self {
        Self {
            ident,
            ty: Type::Undefined,
        }
    }

    pub fn size(&self) -> usize {
        match self.ty {
            Type::I32 => 4,
            Type::Undefined => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Syntax {
    Variable(Variable),
    Assignment(Type),
    Literal(Literal),
    File,
    Function {
        name: Ident,
        return_type: Option<Type>,
    },
    Scope,
    Expression,
    Return,
    While,
}

#[derive(Debug)]
struct Ast {
    body: Syntax,
    children: Vec<Box<Ast>>,
}

impl Ast {
    pub fn variable(var: Variable) -> Self {
        Self {
            body: Syntax::Variable(var),
            children: Vec::new(),
        }
    }

    pub fn literal(lit: Literal) -> Self {
        Self {
            body: Syntax::Literal(lit),
            children: Vec::new(),
        }
    }
}

fn parse_file(tokens: &[Token]) -> Ast {
    let mut children = Vec::new();
    let mut tokenizer = Tokenizer {
        tokens: tokens.iter().by_ref().collect::<Vec<_>>(),
    };

    // Let's assume that the first token will be the start of a function...
    let mut skip = 0;
    while let Some(token) = tokenizer.next_token() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        match &token.ty {
            TokenType::Fn => {
                // println!("{:#?}", tokenizer.tokens);
                let f = collect_tokens_in_scope(&tokenizer.tokens);
                children.push(Box::new(parse_function(&f)));
                skip += f.len();
                // println!("{skip}");
            }
            _ => {
                panic!("expected function");
            }
        }
    }

    Ast {
        body: Syntax::File,
        children,
    }
}

fn collect_tokens_in_scope<'a>(tokens: &[&'a Token]) -> Vec<&'a Token> {
    let mut open_curly = 0;
    let mut closed_curly = 0;
    let mut tokens = tokens
        .iter()
        .rev()
        .take_while(|t| {
            let should_collect = !(open_curly > 0 && open_curly == closed_curly);

            match t.ty {
                TokenType::OpenCurly => open_curly += 1,
                TokenType::ClosedCurly => closed_curly += 1,
                _ => {}
            }

            should_collect
        })
        .map(|t| *t)
        .collect::<Vec<_>>();
    tokens.reverse();
    tokens
}

struct Tokenizer<'a> {
    tokens: Vec<&'a Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn next_token<'s>(&'s mut self) -> Option<&'a Token> {
        self.tokens.pop()
    }

    pub fn expect(&mut self, ty: TokenType) -> Result<&TokenType, ()> {
        let token = self.tokens.pop().unwrap();

        if token.ty == ty {
            Ok(&token.ty)
        } else {
            Err(())
        }
    }

    pub fn is_next(&self, ty: TokenType) -> bool {
        self.tokens[self.tokens.len() - 1].ty == ty
    }
}

fn parse_function(tokens: &[&Token]) -> Ast {
    // println!("Function: {tokens:#?}");

    let mut children = Vec::new();

    let tokens = tokens.to_vec();
    let mut tokenizer = Tokenizer { tokens };
    // tokenizer.expect(TokenType::Fn).unwrap();
    let name = Ident(
        tokenizer
            .expect(TokenType::Ident(Default::default()))
            .unwrap()
            .extract_ident(),
    );
    tokenizer.expect(TokenType::OpenParen).unwrap();
    tokenizer.expect(TokenType::ClosedParen).unwrap();

    // println!("{:?}", tokenizer.tokens);
    let return_type = if tokenizer.is_next(TokenType::Colon) {
        tokenizer.expect(TokenType::Colon).unwrap();
        tokenizer.expect(TokenType::Colon).unwrap();
        Some(
            tokenizer
                .expect(TokenType::Type(Default::default()))
                .unwrap()
                .extract_type(),
        )
    } else {
        None
    };

    tokenizer.expect(TokenType::OpenCurly).unwrap();
    children.push(Box::new(parse_scope(&tokenizer.tokens)));

    Ast {
        body: Syntax::Function { name, return_type },
        children,
    }
}

fn parse_scope(tokens: &[&Token]) -> Ast {
    let mut children = Vec::new();
    let tokens = tokens.to_vec();
    let mut tokenizer = Tokenizer { tokens };

    let mut expr = Vec::new();
    let mut skip = 0;
    while let Some(t) = tokenizer.next_token() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        match &t.ty {
            TokenType::OpenCurly => {
                let tokens = collect_tokens_in_scope(&tokenizer.tokens);
                children.push(Box::new(parse_scope(&tokens)));
                skip += tokens.len();
            }
            TokenType::ClosedCurly => {
                break;
            }
            TokenType::SemiColon => {
                children.push(Box::new(parse_expr(&expr)));
                expr.clear();
            }
            _ => expr.push(t),
        }
    }

    Ast {
        body: Syntax::Scope,
        children,
    }
}

fn parse_expr(expr: &[&Token]) -> Ast {
    // let mut children = Vec::new();
    let mut tokens = expr.to_vec();
    tokens.reverse();
    let mut tokenizer = Tokenizer { tokens };

    match &tokenizer.next_token().unwrap().ty {
        TokenType::Return => {
            // let child = Box::new(parse_return(tokenizer));
            // children.push(child);
            parse_return(tokenizer)
        }
        TokenType::Let => {
            // let child = Box::new(parse_assignment(tokenizer));
            // children.push(child);
            parse_assignment(tokenizer)
        }
        val => panic!("{:?}", val),
    }

    // Ast {
    //     body: Syntax::Expression,
    //     children,
    // }
}

fn parse_return(mut tokenizer: Tokenizer) -> Ast {
    let mut children = Vec::new();

    while let Some(t) = tokenizer.next_token() {
        match &t.ty {
            TokenType::Numeric(_) => children.push(Box::new(Ast::literal(Literal(t.ty.clone())))),
            TokenType::Ident(ident) => children.push(Box::new(Ast::variable(
                Variable::from_ident(Ident(ident.clone())),
            ))),
            _ => panic!(),
        }
    }

    Ast {
        body: Syntax::Return,
        children,
    }
}

fn parse_assignment(mut tokenizer: Tokenizer) -> Ast {
    let mut children = Vec::new();

    let ident = Ident(
        tokenizer
            .expect(TokenType::Ident(Default::default()))
            .unwrap()
            .extract_ident(),
    );
    // TODO: type inference
    tokenizer.expect(TokenType::Colon).unwrap();
    let ty = tokenizer
        .expect(TokenType::Type(Default::default()))
        .unwrap()
        .extract_type();
    tokenizer.expect(TokenType::Equals).unwrap();

    children.push(Box::new(Ast::variable(Variable { ident, ty })));

    while let Some(t) = tokenizer.next_token() {
        match t.ty {
            TokenType::Numeric(_) => children.push(Box::new(Ast::literal(Literal(t.ty.clone())))),
            _ => panic!(),
        }
    }

    Ast {
        body: Syntax::Assignment(ty),
        children,
    }
}

fn print_ast(head: &Ast, i: usize) {
    println!(
        "{}body: {:?}",
        (0..i).map(|_| "\t").collect::<String>(),
        head.body
    );
    for children in head.children.iter() {
        print_ast(&children, i + 1);
    }
}
