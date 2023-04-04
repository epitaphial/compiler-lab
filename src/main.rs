mod asmgen;
mod ast;
mod parser;

use std::{env, error::Error, fs};

#[macro_use]
extern crate lalrpop_util;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input_name = &args[2];
    let output_mode = &args[1];
    let output_name = &args[4];
    let source_code = fs::read_to_string(input_name)?;
    fs::File::create(output_name)?;
    match output_mode.as_str() {
        "-koopa"=>{
            let program = parser::parse(&source_code)?;
            parser::gen_ir(program, output_name)?;
        },
        "-riscv"=>{
            let program = parser::parse(&source_code)?;
            asmgen::gen_asm(&program, output_name);
        },
        _=>{},
    }
    Ok(())
}
