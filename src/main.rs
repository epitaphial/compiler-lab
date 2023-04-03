use std::{env, fs, error::Error};

#[macro_use]
extern crate lalrpop_util;

pub mod ast;
lalrpop_mod!(pub sysy);

fn main() -> Result<(),Box<dyn Error>> {
    let args:Vec<String> = env::args().collect();
    let input_name = &args[2];
    //let output_name = &args[4];
    let source_code = fs::read_to_string(input_name)?;
    let final_ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
    //let mut _f_output = fs::File::create(output_name)?;
    //write!(f_output,final_ast)?;
    print!("{}",final_ast);
    Ok(())
}
