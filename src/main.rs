#![allow(warnings)]
mod cfgbuilder;
mod codegen;
mod expressions;
mod lexer;
mod parser;
mod semantics;
mod statements;
use std::{
    fs::{File, read_to_string},
    io::Write,
};

use inkwell::{
    context::Context,
    passes::PassBuilderOptions,
    targets::{Target, TargetMachine},
};
use lexer::{Lexer, Token, TokenType};

use crate::{codegen::Codegen, semantics::SemanticAnalyzer};

fn main() {
    //let str_1 = "-(1.5 + 2) * 3 - 4 / 2 + (5 > 3 && 2.0 != 2 || 4 <= 6) & 3 | 1";

    let fs = read_to_string("test.txt").unwrap();

    let mut lex = Lexer::new(fs);
    let tk = lex.lexe();

    let mut parser = parser::Parser::new(tk);
    let program = parser.parse_program().unwrap();
    println!("{:#?}", program);

    let mut analyzer = SemanticAnalyzer::new();
    if let Err(e) = analyzer.analyze(&program) {
        for i in e {
            println!("semantics error :- {}", i)
        }
        return;
    }
    let ctx = Context::create();

    let mut codegen = Codegen::new(&ctx, "mod_rs".to_string());
    codegen.generate(&program);

    Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let target_tripple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_tripple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_tripple,
            "generic",
            "",
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    /*let passes = "mem2reg,instcombine,gvn,simplifycfg";
    codegen
        .module
        .run_passes(passes, &target_machine, PassBuilderOptions::create())
        .unwrap();*/
    // codegen.module.run_passes("mem2reg", &target_machine, PassBuilderOptions::create()).unwrap();
    target_machine
        .write_to_file(
            &codegen.module,
            inkwell::targets::FileType::Object,
            "out.o".as_ref(),
        )
        .unwrap();
    println!("{}", codegen.module.print_to_string().to_string());
    //println!("{:#?}", parser.parse_program().unwrap());
    //let mut file = File::create("test.ll").unwrap();
    //file.write( codegen.module.print_to_string().to_string().as_bytes()).unwrap();
}
