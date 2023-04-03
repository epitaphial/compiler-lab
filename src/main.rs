use koopa::{
    self,
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
        FunctionData, Program, Type,
    },
};
use std::{env, error::Error, fs};

#[macro_use]
extern crate lalrpop_util;

pub mod ast;
lalrpop_mod!(pub sysy);

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input_name = &args[2];
    let output_name = &args[4];
    let source_code = fs::read_to_string(input_name)?;
    let final_ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
    let mut _f_output = fs::File::create(output_name)?;
    //write!(f_output,final_ast)?;
    //print!("{}",final_ast);

    let mut program = Program::new();
    let fun_main = program.new_func(FunctionData::new(
        "@main".into(),
        Vec::new(),
        Type::get_i32(),
    ));
    let main_data = program.func_mut(fun_main);
    let bb_entry = main_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    main_data.layout_mut().bbs_mut().extend([bb_entry]);

    // instructions for entry basic block
    let ret_num = main_data
        .dfg_mut()
        .new_value()
        .integer(final_ast.func_def.block.ret_stmt.ret_num);
    let ret_ret = main_data.dfg_mut().new_value().ret(Some(ret_num));
    main_data
        .layout_mut()
        .bb_mut(bb_entry)
        .insts_mut()
        .push_key_back(ret_ret)
        .unwrap();
    let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
    koopa_gen.generate_on(&program)?;
    Ok(())
}
