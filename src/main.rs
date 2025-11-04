mod expressions;
mod lexer;
mod parser;
mod semantics;
mod statements;
use std::fs::read_to_string;

use lexer::{Lexer, Token, TokenType};

use crate::semantics::SemanticAnalyzer;

fn main() {
    //let str_1 = "-(1.5 + 2) * 3 - 4 / 2 + (5 > 3 && 2.0 != 2 || 4 <= 6) & 3 | 1";

    let fs = read_to_string("test.txt").unwrap();

    let mut lex = Lexer::new(fs);
    let tk = lex.lexe();

    let mut parser = parser::Parser::new(tk);
    let program = parser.parse_program().unwrap();
    let mut analyzer = SemanticAnalyzer::new();
    if let Err(e) = analyzer.analyze(&program) {
        for i in e {
            println!("{}", i)
        }
    }
    //println!("{:#?}", parser.parse_program().unwrap());
}
