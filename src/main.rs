mod lexer;
mod parser;
mod statements;
mod expressions;
use std::fs::read_to_string;

use lexer::{Lexer, Token, TokenType};
fn main() {
    let str_1 = "-(1.5 + 2) * 3 - 4 / 2 + (5 > 3 && 2.0 != 2 || 4 <= 6) & 3 | 1";
    let fs = read_to_string("test.txt").unwrap();
    let mut lex = Lexer::new(fs);
    let tk = lex.lexe();
    let mut parser = parser::Parser::new(tk);
    println!("{:#?}", parser.parse_program().unwrap());
}
