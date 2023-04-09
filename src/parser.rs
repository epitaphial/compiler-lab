use std::error::Error;

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder},
        BasicBlock, Function, FunctionData, Program, Type,
    },
};

use crate::ast::{self, BlockItem, Stmt, RetStmt, Visitor};

lalrpop_mod!(pub sysy);

pub struct Parser {
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
        let mut sym_func_name = func_def.func_ident.clone();
        sym_func_name.insert(0, '@');
        let function = match func_def.func_type {
            ast::FuncType::TypeInt => self.program.new_func(FunctionData::new(
                sym_func_name,
                Vec::new(),
                Type::get_i32(),
            )),
            ast::FuncType::TypeVoid => self.program.new_func(FunctionData::new(
                sym_func_name,
                Vec::new(),
                Type::get_unit(),
            )),
        };
        let func_data = self.program.func_mut(function);
        // insert the %entry block for function
        let entry_bb = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        self.visit_block(&func_def.block, &function, &entry_bb);
        0
    }
    fn visit_block(
        &mut self,
        block: &crate::ast::Block,
        function: &Function,
        bb: &BasicBlock,
    ) -> i64 {
        match block {
            ast::Block::BlockItems(block_items) => {
                for block_item in block_items {
                    self.visit_block_item(block_item, function, bb);
                }
            }
            ast::Block::Void => {}
        }
        0
    }
    fn visit_block_item(
        &mut self,
        block_item: &crate::ast::BlockItem,
        function: &Function,
        bb: &BasicBlock,
    ) -> i64 {
        match block_item {
            BlockItem::Stmt(stmt) => {
                self.visit_stmt(stmt, function, bb);
            }
        }
        0
    }
    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt, function: &Function, bb: &BasicBlock) -> i64 {
        match stmt {
            Stmt::RetStmt(ret_stmt) => {
                self.visit_ret_stmt(ret_stmt,function,bb);
            }
        }
        0
    }
    fn visit_ret_stmt(&mut self, ret_stmt: &crate::ast::RetStmt, function: &Function, bb: &BasicBlock) -> i64 {
        match ret_stmt {
            RetStmt::Exp(exp) => {
                self.visit_exp(exp, function, bb);
            }
            RetStmt::Void => {
                let func_data = self.program.func_mut(*function);
                let ret = func_data.dfg_mut().new_value().ret(None);
                func_data.layout_mut().bb_mut(*bb).insts_mut().push_key_back(ret).unwrap();
            }
        }
        0
    }
    fn visit_exp(&mut self, _exp: &crate::ast::Exp, _function: &Function, _bb: &BasicBlock) -> i64 {
        
        0
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            program: Program::new(),
        }
    }
    pub fn parse(&mut self,source_code: &String) -> Result<&Program, Box<dyn Error>> {
        let ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
        self.visit_comp_unit(&ast);
        Ok(&self.program)
    }

    pub fn gen_ir(&self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
