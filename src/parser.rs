use std::error::Error;

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
        FunctionData, Program, Type,
    },
};

lalrpop_mod!(pub sysy);

pub fn parse(source_code: &String) -> Result<Program, Box<dyn Error>> {
    let final_ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
    let program = Program::new();
    println!("{:#?}",final_ast);
    // let fun_main = program.new_func(FunctionData::new(
    //     "@main".into(),
    //     Vec::new(),
    //     Type::get_i32(),
    // ));
    // let main_data = program.func_mut(fun_main);
    // let bb_entry = main_data
    //     .dfg_mut()
    //     .new_bb()
    //     .basic_block(Some("%entry".into()));
    // main_data.layout_mut().bbs_mut().extend([bb_entry]);

    // // instructions for entry basic block
    // let ret_num = main_data
    //     .dfg_mut()
    //     .new_value()
    //     .integer(final_ast.func_def.block.ret_stmt.ret_num);
    // let ret_ret = main_data.dfg_mut().new_value().ret(Some(ret_num));
    // main_data
    //     .layout_mut()
    //     .bb_mut(bb_entry)
    //     .insts_mut()
    //     .push_key_back(ret_ret).unwrap();
    Ok(program)
}

pub fn gen_ir(program:Program, output_name: &String)-> Result<(), Box<dyn Error>> {
    let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
    koopa_gen.generate_on(&program).unwrap();
    Ok(())
}