use colored::Colorize;
use core::panic;
use std::{os::unix::process::CommandExt, path::PathBuf, process::Command, str::FromStr};

mod ast;
mod compiler;
mod emulator;
mod parse;
mod token;

fn main() {
    // riscv32-unknown-elf-gcc -nostdlib -Ttext=0x80000000 -o emu.txt ./emu.S
    let output = Command::new("riscv32-unknown-elf-gcc")
        .arg("-nostdlib")
        .arg("-Ttext=0x80000000")
        .arg("-oemu.txt")
        .arg("./emu.S")
        .output()
        .unwrap();
    if !output.stdout.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stderr));
    }
    // riscv32-unknown-elf-objcopy -O binary emu.bin
    let output = Command::new("riscv32-unknown-elf-objcopy")
        .arg("-O")
        .arg("binary")
        .arg("emu.txt")
        .output()
        .unwrap();
    if !output.stdout.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stderr));
    }

    let raw = std::fs::read("./emu.txt").unwrap();

    // std::fs::remove_file("./emu.txt").unwrap();

    emulator::emulate(&raw);

    // let mut args = std::env::args();
    // let _ex = args.next();
    // let path = args.next().expect("file not provided: nova <file.hy>");
    // let compile = !args.next().is_some_and(|a| a == "--compile-asm");
    //
    // let mut raw = std::fs::read_to_string(&path).unwrap();
    // raw.push_str(
    //     "fn print(str: &[char]) {
    //         @asm {
    //             ; rsi -- [] char
    //             ; rdx -- buf_len
    //
    //             mov rsi, [rsp+48]
    //             mov rdx, [rsp+56]
    //
    //             mov rax, 1          ; write
    //             mov rdi, 1          ; to stdout
    //             syscall
    //         }
    //     }",
    // );
    // let path = PathBuf::from_str(&path).unwrap();
    // let path = path.file_stem().unwrap().to_str().unwrap();
    //
    // if compile {
    //     for file in std::fs::read_dir("./build").unwrap() {
    //         std::fs::remove_file(file.unwrap().path()).unwrap();
    //     }
    //
    //     raw.insert(0, '{');
    //     raw.push('}');
    //
    //     let tokens = parse::parse_raw(raw.into());
    //     println!("{tokens:#?}");
    //     let mut scope = token::parse_tokens(tokens).unwrap();
    //     println!("{scope:#?}");
    //     ast::parse_ast(&mut scope);
    //     println!("{scope:#?}");
    //     let output = crate::compiler::compile(scope);
    //     println!("{output}");
    //
    //     std::fs::write(format!("./build/{}.asm", path), output).unwrap();
    // }
    //
    // let output = Command::new("nasm")
    //     .arg("-f")
    //     .arg("elf64")
    //     .arg("-o")
    //     .arg(format!("./build/{}.o", &path))
    //     .arg(format!("./build/{}.asm", path))
    //     .output()
    //     .unwrap();
    // if !output.stdout.is_empty() {
    //     println!("nasm stdout: {}", String::from_utf8(output.stdout).unwrap());
    // }
    // if !output.stderr.is_empty() {
    //     println!("nasm stderr: {}", String::from_utf8(output.stderr).unwrap());
    // }
    // let output = Command::new("gcc")
    //     .arg("-no-pie")
    //     .arg("-o")
    //     .arg(format!("./build/{}", &path))
    //     .arg(format!("./build/{}.o", &path))
    //     .output()
    //     .unwrap();
    // if !output.stdout.is_empty() {
    //     println!(
    //         "linker stdout: {}",
    //         String::from_utf8(output.stdout).unwrap()
    //     );
    // }
    // if !output.stderr.is_empty() {
    //     println!(
    //         "linker stderr: {}",
    //         String::from_utf8(output.stderr).unwrap()
    //     );
    // }
    // let output = Command::new(format!("./build/{}", &path)).output().unwrap();
    // if !output.stdout.is_empty() {
    //     println!("stout: {}", String::from_utf8(output.stdout).unwrap());
    // }
    // if !output.stderr.is_empty() {
    //     println!("sterr: {:?}", String::from_utf8(output.stderr).unwrap());
    // }
    // println!("exit code: {}", output.status.code().unwrap());
}

// impl PartialEq for TokenType {
//     fn eq(&self, other: &Self) -> bool {
//         std::mem::discriminant(self) == std::mem::discriminant(other)
//     }
// }
//
// impl TokenType {
//     pub fn extract_type(&self) -> Type {
//         match self {
//             TokenType::Type(ty) => *ty,
//             _ => panic!(),
//         }
//     }
//
//     pub fn extract_ident(&self) -> String {
//         match self {
//             TokenType::Ident(ident) => ident.clone(),
//             _ => panic!(),
//         }
//     }
// }
//
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// enum Type {
//     #[default]
//     Void,
//     I32,
//     I64,
//     U32,
//     U64,
// }
//
// impl Type {
//     pub fn keyword(&self) -> String {
//         let keyword = match self {
//             Self::I32 => "dword",
//             Self::I64 => "qword",
//             Self::U32 => "dword",
//             Self::U64 => "qword",
//             Self::Void => panic!(),
//         };
//         keyword.into()
//     }
//
//     pub fn size(&self) -> usize {
//         match self {
//             Type::I32 => 4,
//             Type::U32 => 4,
//             Type::I64 => 8,
//             Type::U64 => 8,
//             Type::Void => panic!(),
//         }
//     }
// }
//
// fn compile(mut tokens: Vec<Token>) -> String {
//     let mut output = String::new();
//
//     output.push_str("global _start\n");
//     output.push_str("section .text\n");
//     output.push('\n');
//
//     tokens.reverse();
//     println!("{tokens:#?}");
//     let mut ast = parse_file(&tokens);
//
//     propogate_types_for_variables(&mut ast);
//     verify_functions(&ast);
//
//     // print_ast(&ast, 0);
//     println!("{ast:#?}");
//
//     assert_eq!(ast.body, Syntax::File);
//
//     for child in ast.children.iter() {
//         assert_eq!(
//             std::mem::discriminant(&child.body),
//             std::mem::discriminant(&Syntax::Function {
//                 name: Ident(Default::default()),
//                 return_type: Default::default(),
//             })
//         );
//
//         match &child.body {
//             Syntax::Function { name, .. } => {
//                 output.push_str(&format!("{}:\n", name.0));
//                 let mut frame = StackFrame::default();
//
//                 for child in child.children.iter() {
//                     match &child.body {
//                         Syntax::Scope {
//                             variables,
//                             return_type,
//                         } => {
//                             output.push_str(&build_scope(
//                                 &child,
//                                 &mut frame,
//                                 &variables,
//                                 &return_type,
//                             ));
//                         }
//                         _ => panic!(),
//                     }
//                 }
//                 output.push('\n');
//
//                 if &name.0 == "main" {
//                     output.push_str("_start:\n");
//                     output.push_str("\tcall main\n");
//                     output.push_str("\tmov rdi, rax\n");
//                     output.push_str("\tmov rax, 60\n");
//                     output.push_str("\tsyscall\n");
//                 }
//             }
//             _ => panic!(),
//         }
//     }
//
//     println!("\n\n{output}\n\n");
//
//     output
// }
//
// // TODO: There should be some sort of precedence for type propogation, like back to front.
// // This would allow for types to be inferred by the return type of the function, and have that back
// // propgate through the ast.
// //
// //
// fn propogate_types_for_variables(head: &mut Ast) {
//     let mut inferences = Vec::new();
//     recur_propogate_types_for_variables(head, &mut inferences);
//     for (var_ident, ty) in inferences.into_iter() {
//         assign_type_to_variable(head, &var_ident, &ty);
//     }
// }
//
// fn assign_type_to_variable(head: &mut Ast, var: &Ident, ty: &Type) {
//     match &mut head.body {
//         Syntax::Variable(v) => {
//             if &v.ident == var {
//                 v.ty = Some(*ty);
//             }
//         }
//         _ => {}
//     }
//
//     for child in head.children.iter_mut() {
//         assign_type_to_variable(child, var, ty);
//     }
// }
//
// fn recur_propogate_types_for_variables(head: &Ast, buf: &mut Vec<(Ident, Type)>) {
//     for child in head.children.iter() {
//         match &child.body {
//             Syntax::Variable(var) => {
//                 if let Some(ty) = propogate_types(head, var) {
//                     buf.push((var.ident.clone(), ty));
//                 }
//             }
//             _ => recur_propogate_types_for_variables(child, buf),
//         }
//     }
// }
//
// fn propogate_types(head: &Ast, var: &Variable) -> Option<Type> {
//     if head
//         .children
//         .iter()
//         .find(|a| match &a.body {
//             Syntax::Variable(v) => v.ident == var.ident,
//             _ => false,
//         })
//         .is_some()
//     {
//         match &head.body {
//             Syntax::Return(ty) => {
//                 return Some(*ty);
//                 // for child in head.children.iter() {
//                 // match &child.body {
//                 //     Syntax::Literal(_) => return None,
//                 //     // TODO: error on void function
//                 //     Syntax::Function { return_type, .. } => return *return_type,
//                 //     // Syntax::Scope => retrieve_return_children(child)
//                 //     Syntax::Variable(_) => {}
//                 //     _ => panic!(),
//                 // }
//                 // }
//             }
//             Syntax::Assignment => {
//                 for child in head.children.iter() {
//                     match &child.body {
//                         Syntax::Literal(_) => return None,
//                         // TODO: error on void function
//                         Syntax::Function { return_type, .. } => return Some(*return_type),
//                         // Syntax::Scope => retrieve_return_children(child)
//                         Syntax::Variable(_) => {}
//                         // Syntax::Call { name } =>
//                         _ => panic!(),
//                     }
//                 }
//             }
//             _ => {}
//         }
//     } else {
//         for child in head.children.iter() {
//             let val = propogate_types(child, var);
//             if val.is_some() {
//                 return val;
//             }
//         }
//     }
//
//     None
// }
//
// fn verify_functions(head: &Ast) {
//     for child in head.children.iter() {
//         match &child.body {
//             Syntax::Function { name, return_type } => {
//                 if let Some(ret) = retrieve_return_children(child) {
//                     // println!("{ret:#?}");
//
//                     for child in ret.iter() {
//                         match &child.body {
//                             Syntax::Variable(var) => {
//                                 // let var = retrieve_variable_from_ident(child, &var.ident);
//                                 // TODO: verify
//                             }
//                             Syntax::Literal(lit) => {
//                                 // TODO: verify
//                             }
//                             _ => {}
//                         }
//                     }
//                 }
//             }
//             _ => verify_functions(child),
//         }
//     }
// }
//
// fn retrieve_variable_from_ident<'a>(head: &'a Ast, ident: &Ident) -> Option<&'a [Box<Ast>]> {
//     match &head.body {
//         Syntax::Variable(var) => {
//             if &var.ident == ident {
//                 return Some(&head.children);
//             }
//         }
//         _ => {}
//     }
//
//     for child in head.children.iter() {
//         let val = retrieve_variable_from_ident(child, ident);
//         if val.is_some() {
//             return val;
//         }
//     }
//
//     None
// }
//
// fn retrieve_variables_in_scope(scope: &Ast) -> Vec<Variable> {
//     match &scope.body {
//         Syntax::Scope { .. } => {}
//         _ => panic!(),
//     }
//
//     let mut buf = Vec::new();
//
//     for child in scope.children.iter() {
//         recur_retrieve_variables_in_scope(child, &mut buf);
//     }
//
//     buf
// }
//
// fn recur_retrieve_variables_in_scope(scope: &Ast, buf: &mut Vec<Variable>) {
//     match &scope.body {
//         Syntax::Variable(var) => {
//             if !buf.contains(&var) {
//                 buf.push(var.clone());
//             }
//         }
//         _ => {}
//     }
//
//     for child in scope.children.iter() {
//         recur_retrieve_variables_in_scope(child, buf);
//     }
// }
//
// fn retrieve_return_children(head: &Ast) -> Option<&[Box<Ast>]> {
//     match &head.body {
//         Syntax::Return(_) => return Some(&head.children),
//         _ => {}
//     }
//
//     for child in head.children.iter() {
//         let val = retrieve_return_children(child);
//         if val.is_some() {
//             return val;
//         }
//     }
//
//     None
// }
//
// fn retrieve_return(head: &Ast) -> Option<&Ast> {
//     match &head.body {
//         Syntax::Return(_) => return Some(head),
//         _ => {}
//     }
//
//     for child in head.children.iter() {
//         let val = retrieve_return(child);
//         if val.is_some() {
//             return val;
//         }
//     }
//
//     None
// }
//
// #[derive(Debug, Default)]
// struct StackFrame {
//     variables: Vec<(Variable, usize)>,
// }
//
// impl StackFrame {
//     pub fn push(&mut self, var: Variable) {
//         let i = if let Some((var, prev_offset)) = self.variables.last() {
//             prev_offset + var.size()
//         } else {
//             0
//         };
//
//         self.variables.push((var, i));
//     }
//
//     pub fn get(&self, var: &Variable) -> Option<usize> {
//         self.variables
//             .iter()
//             .find_map(|(v, i)| if v.ident == var.ident { Some(*i) } else { None })
//     }
//
//     pub fn get_or_push(&mut self, var: Variable) -> usize {
//         if let Some(offset) = self.get(&var) {
//             offset
//         } else {
//             self.push(var);
//             self.variables.last().unwrap().1
//         }
//     }
//
//     pub fn size(&self) -> usize {
//         if let Some((v, i)) = self.variables.last() {
//             i + v.size()
//         } else {
//             0
//         }
//     }
//
//     pub fn size_aligned(&self) -> usize {
//         let size = self.size();
//         let padding = 16 % 12;
//         size + padding
//     }
// }
//
// fn build_scope(
//     scope: &Ast,
//     frame: &mut StackFrame,
//     variables: &[Variable],
//     return_type: &Type,
// ) -> String {
//     let mut output = String::new();
//     let mut scope_output = String::new();
//
//     output.push_str("\t;stack frame\n");
//     output.push_str("\tpush rbp\n");
//     output.push_str("\tmov rbp, rsp\n");
//     scope_output.push_str("\n\t;body\n");
//
//     for child in scope.children.iter() {
//         match child.body {
//             Syntax::Return(ty) => {
//                 // println!("{child:#?}");
//                 let val = &child.children[0];
//                 match &val.body {
//                     Syntax::Literal(lit) => {
//                         // if let Some(ty) = return_type {
//                         // match ty {
//                         //     Type::I32 => {
//                         //         // let val = lit
//                         //         //     .numeric()
//                         //         //     // TODO: if this is
//                         //         //     // verified in a
//                         //         //     // previous step, then
//                         //         //     // this step would be
//                         //         //     // unnecessary since
//                         //         //     // the value is already
//                         //         //     // described by a
//                         //         //     // String.
//                         //         //     .parse::<i32>()
//                         //         //     .unwrap();
//                         //         //
//
//                         scope_output.push_str(&format!("\tmov rax, {}\n", lit.as_string()));
//
//                         //     }
//                         //     _ => panic!(),
//                         // }
//                         // }
//                     }
//                     Syntax::Variable(var) => {
//                         if let Some(offset) = frame.get(var) {
//                             scope_output.push_str(&format!("\tmov rax, [rbp-{}]\n", offset))
//                         }
//                     }
//                     _ => panic!(),
//                 }
//
//                 // scope_output.push_str(&format!("\tadd rsp, {}\n", frame.size()));
//                 // scope_output.push_str("\tret\n");
//
//                 output.push_str("\n\t;allocating local stack (must maintain 16 byte alignment)\n");
//                 output.push_str(&format!("\tsub rsp, {}\n", frame.size_aligned()));
//                 output.push_str(&scope_output);
//
//                 // output.push_str("\tmov rsp, rbp\n");
//                 // output.push_str("\tpop rbp\n");
//                 break;
//             }
//             Syntax::Assignment => {
//                 let mut offset = 0;
//                 let mut var = None;
//                 for ass_child in child.children.iter() {
//                     match &ass_child.body {
//                         Syntax::Variable(v) => {
//                             offset = frame.get_or_push(v.clone());
//                             var = Some(v);
//                         }
//                         Syntax::Literal(lit) => {
//                             assert_eq!(child.children.len(), 2);
//                             let val = lit.as_string();
//                             // TODO: literal sizing
//                             if let Some(ty) = &var.and_then(|v| v.ty) {
//                                 scope_output.push_str(&format!(
//                                     "\tmov {} [rbp-{}], {}\n",
//                                     ty.keyword(),
//                                     offset,
//                                     val
//                                 ));
//                             } else {
//                                 panic!();
//                             }
//                         }
//                         Syntax::Call { name } => {
//                             // TODO: function parameters
//                             if let Some(var) = var {
//                                 scope_output.push_str(&format!("\tcall {}\n", name.0));
//                                 scope_output.push_str(&format!(
//                                     "\tmov {} [rbp-{}], rax\n",
//                                     var.ty.unwrap().keyword(),
//                                     frame.get_or_push(var.clone())
//                                 ));
//                             } else {
//                                 panic!();
//                             }
//                         }
//                         _ => panic!(),
//                     }
//                 }
//             }
//             _ => panic!(),
//         }
//     }
//
//     output.push_str("\n\t;clean stack frame\n");
//     output.push_str("\tleave\n");
//     output.push_str("\tret\n");
//
//     println!("{frame:#?}");
//     output
// }
//
// #[derive(Debug, Clone, PartialEq, Eq)]
// struct Ident(String);
//
// #[derive(Debug)]
// struct Literal(TokenType);
//
// impl PartialEq for Literal {
//     fn eq(&self, other: &Self) -> bool {
//         if std::mem::discriminant(&self.0) == std::mem::discriminant(&other.0) {
//             match &self.0 {
//                 TokenType::Numeric(val1) => match &other.0 {
//                     TokenType::Numeric(val2) => val1 == val2,
//                     _ => false,
//                 },
//                 _ => unreachable!(),
//             }
//         } else {
//             false
//         }
//     }
// }
//
// impl Literal {
//     pub fn as_string(&self) -> String {
//         match &self.0 {
//             TokenType::Numeric(val) => val.clone(),
//             _ => panic!(),
//         }
//     }
//
//     pub fn numeric(&self) -> String {
//         match &self.0 {
//             TokenType::Numeric(val) => val.clone(),
//             _ => panic!(),
//         }
//     }
// }
//
// #[derive(Debug, Clone, PartialEq)]
// struct Variable {
//     ident: Ident,
//     ty: Option<Type>,
// }
//
// impl Variable {
//     pub fn from_ident(ident: Ident) -> Self {
//         Self { ident, ty: None }
//     }
//
//     pub fn size(&self) -> usize {
//         if let Some(ty) = self.ty {
//             ty.size()
//         } else {
//             panic!()
//         }
//     }
// }
//
// #[derive(Debug, PartialEq)]
// enum Syntax {
//     Variable(Variable),
//     Assignment,
//     Literal(Literal),
//     File,
//     Function {
//         name: Ident,
//         return_type: Type,
//     },
//     Call {
//         name: Ident,
//     },
//     Scope {
//         variables: Vec<Variable>,
//         return_type: Type,
//     },
//     Expression,
//     Return(Type),
//     While,
// }
//
// #[derive(Debug)]
// struct Ast {
//     body: Syntax,
//     children: Vec<Box<Ast>>,
// }
//
// impl Ast {
//     pub fn variable(var: Variable) -> Self {
//         Self {
//             body: Syntax::Variable(var),
//             children: Vec::new(),
//         }
//     }
//
//     pub fn literal(lit: Literal) -> Self {
//         Self {
//             body: Syntax::Literal(lit),
//             children: Vec::new(),
//         }
//     }
//
//     pub fn call(name: Ident) -> Self {
//         Self {
//             body: Syntax::Call { name },
//             children: Vec::new(),
//         }
//     }
// }
//
// fn parse_file(tokens: &[Token]) -> Ast {
//     let mut children = Vec::new();
//     let mut tokenizer = Tokenizer {
//         tokens: tokens.iter().by_ref().collect::<Vec<_>>(),
//     };
//
//     // Let's assume that the first token will be the start of a function...
//     let mut skip = 0;
//     while let Some(token) = tokenizer.next_token() {
//         if skip > 0 {
//             skip -= 1;
//             continue;
//         }
//
//         match &token.ty {
//             TokenType::Fn => {
//                 // println!("{:#?}", tokenizer.tokens);
//                 let f = collect_tokens_in_scope(&tokenizer.tokens);
//                 children.push(Box::new(parse_function(&f)));
//                 skip += f.len();
//                 // println!("{skip}");
//             }
//             _ => {
//                 panic!("expected function");
//             }
//         }
//     }
//
//     Ast {
//         body: Syntax::File,
//         children,
//     }
// }
//
// fn collect_tokens_in_scope<'a>(tokens: &[&'a Token]) -> Vec<&'a Token> {
//     let mut open_curly = 0;
//     let mut closed_curly = 0;
//     let mut tokens = tokens
//         .iter()
//         .rev()
//         .take_while(|t| {
//             let should_collect = !(open_curly > 0 && open_curly == closed_curly);
//
//             match t.ty {
//                 TokenType::OpenCurly => open_curly += 1,
//                 TokenType::ClosedCurly => closed_curly += 1,
//                 _ => {}
//             }
//
//             should_collect
//         })
//         .map(|t| *t)
//         .collect::<Vec<_>>();
//     tokens.reverse();
//     tokens
// }
//
// #[derive(Debug)]
// struct Tokenizer<'a> {
//     tokens: Vec<&'a Token>,
// }
//
// impl<'a> Tokenizer<'a> {
//     pub fn next_token<'s>(&'s mut self) -> Option<&'a Token> {
//         self.tokens.pop()
//     }
//
//     pub fn expect(&mut self, ty: TokenType) -> Result<&TokenType, ()> {
//         let token = self.tokens.pop().unwrap();
//
//         if token.ty == ty {
//             Ok(&token.ty)
//         } else {
//             Err(())
//         }
//     }
//
//     pub fn is_next(&self, ty: TokenType) -> bool {
//         self.tokens[self.tokens.len() - 1].ty == ty
//     }
// }
//
// fn parse_function(tokens: &[&Token]) -> Ast {
//     // println!("Function: {tokens:#?}");
//
//     let mut children = Vec::new();
//
//     let tokens = tokens.to_vec();
//     let mut tokenizer = Tokenizer { tokens };
//     // tokenizer.expect(TokenType::Fn).unwrap();
//     let name = Ident(
//         tokenizer
//             .expect(TokenType::Ident(Default::default()))
//             .unwrap()
//             .extract_ident(),
//     );
//     tokenizer.expect(TokenType::OpenParen).unwrap();
//     tokenizer.expect(TokenType::ClosedParen).unwrap();
//
//     // println!("{:?}", tokenizer.tokens);
//     let return_type = if tokenizer.is_next(TokenType::Colon) {
//         tokenizer.expect(TokenType::Colon).unwrap();
//         tokenizer.expect(TokenType::Colon).unwrap();
//         tokenizer
//             .expect(TokenType::Type(Default::default()))
//             .unwrap()
//             .extract_type()
//     } else {
//         Type::Void
//     };
//
//     tokenizer.expect(TokenType::OpenCurly).unwrap();
//     children.push(Box::new(parse_scope(&tokenizer.tokens, return_type)));
//
//     Ast {
//         body: Syntax::Function { name, return_type },
//         children,
//     }
// }
//
// fn parse_scope(tokens: &[&Token], return_type: Type) -> Ast {
//     let mut children = Vec::new();
//     let tokens = tokens.to_vec();
//     let mut tokenizer = Tokenizer { tokens };
//
//     let mut expr = Vec::new();
//     let mut skip = 0;
//     while let Some(t) = tokenizer.next_token() {
//         if skip > 0 {
//             skip -= 1;
//             continue;
//         }
//
//         match &t.ty {
//             TokenType::OpenCurly => {
//                 let tokens = collect_tokens_in_scope(&tokenizer.tokens);
//                 children.push(Box::new(parse_scope(&tokens, return_type)));
//                 skip += tokens.len();
//             }
//             TokenType::ClosedCurly => {
//                 break;
//             }
//             TokenType::SemiColon => {
//                 children.push(Box::new(parse_expr(&expr, return_type)));
//                 expr.clear();
//             }
//             _ => expr.push(t),
//         }
//     }
//
//     let mut return_type = Type::Void;
//     for child in children.iter() {
//         if let Some(ret) = retrieve_return(child) {
//             match &ret.body {
//                 Syntax::Return(ty) => return_type = *ty,
//                 _ => unreachable!(),
//             }
//         }
//     }
//
//     let mut scope = Ast {
//         body: Syntax::Scope {
//             variables: Vec::new(),
//             return_type,
//         },
//         children,
//     };
//     let vars = retrieve_variables_in_scope(&scope);
//     println!("vars: {vars:#?}");
//     match &mut scope.body {
//         Syntax::Scope { variables, .. } => {
//             *variables = vars;
//         }
//         _ => unreachable!(),
//     }
//
//     scope
// }
//
// fn parse_expr(expr: &[&Token], return_type: Type) -> Ast {
//     // let mut children = Vec::new();
//     let mut tokens = expr.to_vec();
//     tokens.reverse();
//     let mut tokenizer = Tokenizer { tokens };
//
//     match &tokenizer.next_token().unwrap().ty {
//         TokenType::Return => {
//             // let child = Box::new(parse_return(tokenizer));
//             // children.push(child);
//             parse_return(tokenizer, return_type)
//         }
//         TokenType::Let => {
//             // let child = Box::new(parse_assignment(tokenizer));
//             // children.push(child);
//             parse_assignment(tokenizer)
//         }
//         val => panic!("{:?}", val),
//     }
//
//     // Ast {
//     //     body: Syntax::Expression,
//     //     children,
//     // }
// }
//
// fn parse_return(mut tokenizer: Tokenizer, return_type: Type) -> Ast {
//     let mut children = Vec::new();
//
//     while let Some(t) = tokenizer.next_token() {
//         match &t.ty {
//             TokenType::Numeric(_) => children.push(Box::new(Ast::literal(Literal(t.ty.clone())))),
//             TokenType::Ident(ident) => children.push(Box::new(Ast::variable(
//                 Variable::from_ident(Ident(ident.clone())),
//             ))),
//             _ => panic!(),
//         }
//     }
//
//     Ast {
//         body: Syntax::Return(return_type),
//         children,
//     }
// }
//
// fn parse_assignment(mut tokenizer: Tokenizer) -> Ast {
//     let mut children = Vec::new();
//
//     let ident = Ident(
//         tokenizer
//             .expect(TokenType::Ident(Default::default()))
//             .unwrap()
//             .extract_ident(),
//     );
//     // TODO: type inference
//     let ty = if tokenizer.is_next(TokenType::Colon) {
//         tokenizer.expect(TokenType::Colon).unwrap();
//         Some(
//             tokenizer
//                 .expect(TokenType::Type(Default::default()))
//                 .unwrap()
//                 .extract_type(),
//         )
//     } else {
//         None
//     };
//     tokenizer.expect(TokenType::Equals).unwrap();
//
//     children.push(Box::new(Ast::variable(Variable { ident, ty })));
//
//     while let Some(t) = tokenizer.next_token() {
//         match &t.ty {
//             TokenType::Numeric(_) => children.push(Box::new(Ast::literal(Literal(t.ty.clone())))),
//             TokenType::Ident(ident) => {
//                 let ident = Ident(t.ty.clone().extract_ident());
//
//                 if tokenizer.is_next(TokenType::OpenParen) {
//                     children.push(Box::new(Ast::call(ident)));
//                     // TODO: parameters
//                     tokenizer.expect(TokenType::OpenParen).unwrap();
//                     tokenizer.expect(TokenType::ClosedParen).unwrap();
//                 } else {
//                     children.push(Box::new(Ast::variable(Variable::from_ident(ident))));
//                 }
//             }
//             _ => panic!(),
//         }
//     }
//
//     Ast {
//         body: Syntax::Assignment,
//         children,
//     }
// }
//
// fn print_ast(head: &Ast, i: usize) {
//     println!(
//         "{}body: {:?}",
//         (0..i).map(|_| "\t").collect::<String>(),
//         head.body
//     );
//     for children in head.children.iter() {
//         print_ast(&children, i + 1);
//     }
// }
