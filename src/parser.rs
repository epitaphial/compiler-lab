use std::error::Error;

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
        Function, FunctionData, Program, Type,
    },
};

use crate::ast;

lalrpop_mod!(pub sysy);

pub struct Parser {
    ast: ast::CompUnit,
    program: Program,
}

impl ast::Visitor<i64> for Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit) -> i64 {
        match comp_unit {
            ast::CompUnit::FuncDef(func_def) => {
                self.visit_func_def(func_def);
            }
        }
        0
    }
    fn visit_func_def(&mut self, func_def: &crate::ast::FuncDef) -> i64 {
        let function = match func_def.func_type {
            ast::FuncType::TypeInt => self.program.new_func(FunctionData::new(
                func_def.func_ident.clone(),
                Vec::new(),
                Type::get_i32(),
            )),
            ast::FuncType::TypeVoid => self.program.new_func(FunctionData::new(
                func_def.func_ident.clone(),
                Vec::new(),
                Type::get_unit(),
            )),
        };
        let func_data = self.program.func_mut(function);
        let entry_bb = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        self.visit_block(&func_def.block, &function);
        0
    }
    fn visit_block(&mut self, block: &crate::ast::Block, function: &Function) -> i64 {
        match block {
            ast::Block::BlockItems(block_items) =>{
                for block_item in block_items{
                    self.visit_block_item(block_item,function);
                }
            }
            ast::Block::Void=>{}
        }
        0
    }
    fn visit_block_item(&mut self, block_item: &crate::ast::BlockItem, function: &Function) -> i64 {
        0
    }
    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt, function: &Function) -> i64 {
        0
    }
    fn visit_ret_stmt(&mut self, ret_stmt: &crate::ast::RetStmt) -> i64 {
        0
    }
    fn visit_exp(&mut self, exp: &crate::ast::Exp) -> i64 {
        0
    }
}

impl Parser {
    pub fn new(source_code: &String) -> Parser {
        Parser {
            ast: sysy::CompUnitParser::new().parse(&source_code).unwrap(),
            program: Program::new(),
        }
    }
    pub fn parse(&mut self) -> Result<&Program, Box<dyn Error>> {
        println!("{:#?}", self.ast);

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
        Ok(&self.program)
    }

    pub fn gen_ir(&mut self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
