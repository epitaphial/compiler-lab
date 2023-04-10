use std::error::Error;

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder},
        BasicBlock, Function, FunctionData, Program, Type, Value,
    },
};

use crate::ast::{self, BlockItem, RetStmt, Stmt, Visitor, LOrExp, LAndExp, EqExp, RelExp, AddExp, MulExp, UnaryExp, Operator};

lalrpop_mod!(pub sysy);

pub struct Parser {
    program: Program,
}

enum VisitRetType {
    Value(Value),
    None,
}

impl ast::Visitor<VisitRetType> for Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit) -> VisitRetType {
        match comp_unit {
            ast::CompUnit::FuncDef(func_def) => {
                self.visit_func_def(func_def);
            }
        }
        VisitRetType::None
    }
    fn visit_func_def(&mut self, func_def: &crate::ast::FuncDef) -> VisitRetType {
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
        VisitRetType::None
    }
    fn visit_block(
        &mut self,
        block: &crate::ast::Block,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match block {
            ast::Block::BlockItems(block_items) => {
                for block_item in block_items {
                    self.visit_block_item(block_item, function, bb);
                }
            }
            ast::Block::Void => {}
        }
        VisitRetType::None
    }
    fn visit_block_item(
        &mut self,
        block_item: &crate::ast::BlockItem,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match block_item {
            BlockItem::Stmt(stmt) => {
                self.visit_stmt(stmt, function, bb);
            }
        }
        VisitRetType::None
    }
    fn visit_stmt(
        &mut self,
        stmt: &crate::ast::Stmt,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match stmt {
            Stmt::RetStmt(ret_stmt) => {
                self.visit_ret_stmt(ret_stmt, function, bb);
            }
        }
        VisitRetType::None
    }
    fn visit_ret_stmt(
        &mut self,
        ret_stmt: &crate::ast::RetStmt,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match ret_stmt {
            RetStmt::Exp(exp) => match self.visit_exp(exp, function, bb) {
                VisitRetType::Value(value) => {
                    let func_data = self.program.func_mut(*function);
                    let ret = func_data.dfg_mut().new_value().ret(Some(value));
                    func_data
                        .layout_mut()
                        .bb_mut(*bb)
                        .insts_mut()
                        .push_key_back(ret)
                        .unwrap();
                }
                VisitRetType::None => {
                    panic!("Exp can not be this type")
                }
            },
            RetStmt::Void => {
                let func_data = self.program.func_mut(*function);
                let ret = func_data.dfg_mut().new_value().ret(None);
                func_data
                    .layout_mut()
                    .bb_mut(*bb)
                    .insts_mut()
                    .push_key_back(ret)
                    .unwrap();
            }
        }
        VisitRetType::None
    }
    fn visit_exp(
        &mut self,
        exp: &crate::ast::Exp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        self.visit_l_or_exp(&exp.l_or_exp, function, bb)
    }
    fn visit_l_or_exp(
        &mut self,
        l_or_exp: &ast::LOrExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match l_or_exp  {
            LOrExp::LAndExp(l_and_exp) =>{
                self.visit_l_and_exp(l_and_exp, function, bb)
            }
            LOrExp::BinaryOp(l_or_exp,l_and_exp )=>{
                unimplemented!()
                // match self.visit_l_and_exp(l_and_exp, function, bb) {
                //     VisitRetType::Value(value) => {
                //         VisitRetType::None
                //     }   
                //     VisitRetType::None =>{
                //         panic!("LOrExp can not be this type")
                //     }                 
                // }
            }
        }
    }
    fn visit_l_and_exp(
        &mut self,
        l_and_exp: &ast::LAndExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match l_and_exp {
            LAndExp::EqExp(eq_exp) =>{
                self.visit_eq_exp(eq_exp, function, bb)
            }
            LAndExp::BinaryOp(l_and_exp,eq_exp )=>{
                unimplemented!()
            }
        }
    }
    fn visit_eq_exp(
        &mut self,
        eq_exp: &ast::EqExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match eq_exp {
            EqExp::RelExp(rel_exp) => {
                self.visit_rel_exp(rel_exp, function, bb)
            }
            EqExp::BinaryOp(eq_exp,op ,rel_exp )=>{
                unimplemented!()
            }
        }
    }
    fn visit_rel_exp(
        &mut self,
        rel_exp: &ast::RelExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match rel_exp {
            RelExp::AddExp(add_exp) => {
                self.visit_add_exp(add_exp, function, bb)
            }
            RelExp::BinaryOp(rel_exp,op ,add_exp )=>{
                unimplemented!()
            }
        }
    }
    fn visit_add_exp(
        &mut self,
        add_exp: &ast::AddExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match add_exp {
            AddExp::MulExp(mul_exp) => {
                self.visit_mul_exp(mul_exp, function, bb)
            }
            AddExp::BinaryOp(add_exp,op ,mul_exp )=>{
                unimplemented!()
            }
        }
    }
    fn visit_mul_exp(
        &mut self,
        mul_exp: &ast::MulExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match mul_exp {
            MulExp::UnaryExp(unary_exp) =>{
                self.visit_unary_exp(unary_exp, function, bb)
            }
            MulExp::BinaryOp(mul_exp,op ,unary_exp )=>{
                unimplemented!()
            }
        }
    }

    fn visit_unary_exp(
        &mut self,
        unary_exp: &ast::UnaryExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        match unary_exp {
            UnaryExp::PrimaryExp(primary_exp) =>{
                self.visit_primary_exp(primary_exp, function, bb)
            }
            UnaryExp::UnaryOp(op ,unary_exp )=>{
                unimplemented!()
                // match op {
                //     Operator::Add => {

                //     }
                //     Operator::Divide => {

                //     }
                //     Operator::Not => {

                //     }
                //     _=>{
                //         panic!()
                //     }
                // }
            }
        }
    }
    fn visit_primary_exp(
        &mut self,
        primary_exp: &ast::PrimaryExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        VisitRetType::None
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            program: Program::new(),
        }
    }
    pub fn parse(&mut self, source_code: &String) -> Result<&Program, Box<dyn Error>> {
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
