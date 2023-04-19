use std::error::Error;

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
        BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind,
    },
};

use crate::ast::{
    self, AddExp, BlockItem, EqExp, LAndExp, LOrExp, MulExp, Number, Operator, PrimaryExp, RelExp,
    RetStmt, Stmt, UnaryExp, Visitor,
};

lalrpop_mod!(pub sysy);

pub struct Parser {
    pub program: Program,
}

enum VisitRetType {
    Value(Value),
    None,
}

enum VisitType <'a>{
    Global,
    Local(&'a Function,&'a BasicBlock),
}

macro_rules! funcdata {
    ($self:ident,$func:expr) => {
        $self.program.func_mut($func)
    };
}

macro_rules! basicblock {
    ($func:ident,$bbname:expr) => {
        $func.dfg_mut().new_bb().basic_block(Some($bbname))
    };
}

macro_rules! insertbb {
    ($func:ident,$bb:expr) => {
        $func.layout_mut().bbs_mut().extend($bb)
    };
}

macro_rules! value {
    ($func:ident,$op:ident,$($value:expr),+)=>{
        $func.dfg_mut().new_value().$op($($value),+)
    };
}

macro_rules! globalvalue {
    ($program:expr,$op:ident,$($value:expr),+)=>{
        $program.new_value().$op($($value),+)
    };
}

macro_rules! insertvalue {
    ($func:ident,$bb:expr,$values:expr) => {
        $func.layout_mut().bb_mut($bb).insts_mut().extend($values)
    };
}

macro_rules! getvaluedata {
    ($func:ident,$value:expr) => {
        $func.dfg().value($value)
    };
}

macro_rules! removevalue {
    ($func:ident,$value:expr) => {
        $func.dfg().remove_value($value)
    };
}

impl ast::Visitor<VisitRetType, VisitType> for Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit) -> VisitRetType {
        match comp_unit {
            ast::CompUnit::Decl(comp_unit, decl) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit);
                }
                self.visit_global_decl(decl);
            }
            ast::CompUnit::FuncDef(comp_unit, func_def) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit);
                }
                self.visit_func_def(func_def);
            }
        }
        VisitRetType::None
    }

    fn visit_decl(&mut self, decl: &ast::Decl, visit_type: VisitType) -> VisitRetType {
        match decl {
            ast::Decl::ConstDecl(const_decl) => {
                self.visit_local_const_decl(const_decl, function, bb);
            }
            ast::Decl::VarDecl(var_decl) => {
                self.visit_local_var_decl(var_decl, function, bb);
            }
        }
        VisitRetType::None
    }

    fn visit_const_decl(
        &mut self,
        const_decl: &ast::ConstDecl,
        visit_type: VisitType,
    ) -> VisitRetType {
        for const_def in &const_decl.const_defs {
            self.visit_local_const_def(const_def, function, bb);
        }
        VisitRetType::None
    }

    fn visit_const_def(
        &mut self,
        const_def: &ast::ConstDef,
        visit_type: VisitType,
    ) -> VisitRetType {
        for const_exp in &const_def.const_exps {
            self.visit_const_exp(const_exp, function, bb);
        }
        VisitRetType::None
    }

    fn visit_const_init_val(
        &mut self,
        cons_init_val: &ast::ConstInitVal,
        visit_type: VisitType,
    ) -> VisitRetType {
        unimplemented!()
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl, visit_type: VisitType) -> VisitRetType {
        unimplemented!()
    }

    fn visit_var_def(&mut self, var_def: &ast::VarDef, visit_type: VisitType) -> VisitRetType {
        unimplemented!()
    }

    fn visit_init_val(&mut self, init_val: &ast::InitVal, visit_type: VisitType) -> VisitRetType {
        unimplemented!()
    }

    fn visit_func_def(&mut self, func_def: &crate::ast::FuncDef) -> VisitRetType {
        let mut sym_func_name = func_def.func_ident.clone();
        sym_func_name.insert(0, '@');
        let function = match func_def.func_type {
            ast::Type::Int => self.program.new_func(FunctionData::new(
                sym_func_name,
                Vec::new(),
                Type::get_i32(),
            )),
            ast::Type::Void => self.program.new_func(FunctionData::new(
                sym_func_name,
                Vec::new(),
                Type::get_unit(),
            )),
        };
        let func_data = funcdata!(self, function);
        let entry_bb = basicblock!(func_data, "%entry".into());
        insertbb!(func_data, [entry_bb]);
        self.visit_block(&func_def.block, &function, &entry_bb);
        VisitRetType::None
    }

    fn visit_block(
        &mut self,
        block: &crate::ast::Block,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        for block_item in &block.block_items {
            self.visit_block_item(block_item, function, bb);
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
            BlockItem::Decl(decl) => {}
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
            Stmt::AssignStmt(ass_stmt) => {}
        }
        VisitRetType::None
    }

    fn visit_ret_stmt(
        &mut self,
        ret_stmt: &crate::ast::RetStmt,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        unimplemented!();
        // match ret_stmt {
        //     RetStmt::Exp(exp) => match self.visit_exp(exp, function, bb) {
        //         VisitRetType::Value(value) => {
        //             let func_data = funcdata!(self, *function);
        //             let ret = value!(func_data, ret, Some(value));
        //             insertvalue!(func_data, *bb, [ret]);
        //         }
        //         VisitRetType::None => {
        //             panic!("Exp can not be this type")
        //         }
        //     },
        //     RetStmt::Void => {
        //         let func_data = funcdata!(self, *function);
        //         let ret = value!(func_data, ret, None);
        //         insertvalue!(func_data, *bb, [ret]);
        //     }
        // }
        VisitRetType::None
    }

    fn visit_ass_stmt(
        &mut self,
        ass_stmt: &ast::AssignStmt,
        function: &Function,
        bb: &BasicBlock,
    ) -> VisitRetType {
        unimplemented!();
        VisitRetType::None
    }

    fn visit_l_val(&mut self, l_val: &ast::LVal, visit_type: VisitType) -> VisitRetType {
        unimplemented!();
        VisitRetType::None
    }

    fn visit_const_exp(
        &mut self,
        const_exp: &ast::ConstExp,
        visit_type: VisitType,
    ) -> VisitRetType {
        unimplemented!();
    }

    fn visit_exp(&mut self, exp: &crate::ast::Exp, visit_type: VisitType) -> VisitRetType {
        self.visit_l_or_exp(&exp.l_or_exp, function, bb)
    }

    fn visit_l_or_exp(&mut self, l_or_exp: &ast::LOrExp, visit_type: VisitType) -> VisitRetType {
        match l_or_exp {
            LOrExp::LAndExp(l_and_exp) => self.visit_l_and_exp(l_and_exp, function, bb),
            LOrExp::BinaryOp(l_or_exp, l_and_exp) => {
                if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                    self.visit_l_or_exp(l_or_exp, function, bb),
                    self.visit_l_and_exp(l_and_exp, function, bb),
                ) {
                    let func_data = funcdata!(self, *function);
                    let or = value!(func_data, binary, BinaryOp::Or, l_value, r_value);
                    let zero_value = value!(func_data, integer, 0);
                    let eq = value!(func_data, binary, BinaryOp::NotEq, or, zero_value);
                    insertvalue!(func_data, *bb, [or, eq]);
                    VisitRetType::Value(eq)
                } else {
                    panic!()
                }
            }
        }
    }

    fn visit_l_and_exp(&mut self, l_and_exp: &ast::LAndExp, visit_type: VisitType) -> VisitRetType {
        match l_and_exp {
            LAndExp::EqExp(eq_exp) => self.visit_eq_exp(eq_exp, function, bb),
            LAndExp::BinaryOp(l_and_exp, eq_exp) => {
                if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                    self.visit_l_and_exp(l_and_exp, function, bb),
                    self.visit_eq_exp(eq_exp, function, bb),
                ) {
                    let func_data = funcdata!(self, *function);
                    let zero_value = value!(func_data, integer, 0);
                    let neq_l = value!(func_data, binary, BinaryOp::NotEq, l_value, zero_value);
                    let neq_r = value!(func_data, binary, BinaryOp::NotEq, r_value, zero_value);
                    let and = value!(func_data, binary, BinaryOp::And, neq_l, neq_r);
                    let neq = value!(func_data, binary, BinaryOp::NotEq, and, zero_value);
                    insertvalue!(func_data, *bb, [neq_l, neq_r, and, neq]);
                    VisitRetType::Value(neq)
                } else {
                    panic!()
                }
            }
        }
    }

    fn visit_eq_exp(&mut self, eq_exp: &ast::EqExp, visit_type: VisitType) -> VisitRetType {
        match eq_exp {
            EqExp::RelExp(rel_exp) => self.visit_rel_exp(rel_exp, function, bb),
            EqExp::BinaryOp(eq_exp, op, rel_exp) => match op {
                Operator::Equal => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_eq_exp(eq_exp, function, bb),
                        self.visit_rel_exp(rel_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let eq = value!(func_data, binary, BinaryOp::Eq, l_value, r_value);
                        insertvalue!(func_data, *bb, [eq]);
                        VisitRetType::Value(eq)
                    } else {
                        panic!()
                    }
                }
                Operator::NotEqual => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_eq_exp(eq_exp, function, bb),
                        self.visit_rel_exp(rel_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let not_eq = value!(func_data, binary, BinaryOp::NotEq, l_value, r_value);
                        insertvalue!(func_data, *bb, [not_eq]);
                        VisitRetType::Value(not_eq)
                    } else {
                        panic!()
                    }
                }
                _ => {
                    panic!()
                }
            },
        }
    }

    fn visit_rel_exp(&mut self, rel_exp: &ast::RelExp, visit_type: VisitType) -> VisitRetType {
        match rel_exp {
            RelExp::AddExp(add_exp) => self.visit_add_exp(add_exp, function, bb),
            RelExp::BinaryOp(rel_exp, op, add_exp) => match op {
                Operator::LessThan => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_rel_exp(rel_exp, function, bb),
                        self.visit_add_exp(add_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let lt = value!(func_data, binary, BinaryOp::Lt, l_value, r_value);
                        insertvalue!(func_data, *bb, [lt]);
                        VisitRetType::Value(lt)
                    } else {
                        panic!()
                    }
                }
                Operator::MoreThan => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_rel_exp(rel_exp, function, bb),
                        self.visit_add_exp(add_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let mt = value!(func_data, binary, BinaryOp::Gt, l_value, r_value);
                        insertvalue!(func_data, *bb, [mt]);
                        VisitRetType::Value(mt)
                    } else {
                        panic!()
                    }
                }
                Operator::LessOrEqualThan => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_rel_exp(rel_exp, function, bb),
                        self.visit_add_exp(add_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let le = value!(func_data, binary, BinaryOp::Le, l_value, r_value);
                        insertvalue!(func_data, *bb, [le]);
                        VisitRetType::Value(le)
                    } else {
                        panic!()
                    }
                }
                Operator::MoreOrEqualThan => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_rel_exp(rel_exp, function, bb),
                        self.visit_add_exp(add_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let me = value!(func_data, binary, BinaryOp::Ge, l_value, r_value);
                        insertvalue!(func_data, *bb, [me]);
                        VisitRetType::Value(me)
                    } else {
                        panic!()
                    }
                }
                _ => {
                    panic!()
                }
            },
        }
    }

    fn visit_add_exp(&mut self, add_exp: &ast::AddExp, visit_type: VisitType) -> VisitRetType {
        match add_exp {
            AddExp::MulExp(mul_exp) => self.visit_mul_exp(mul_exp, function, bb),
            AddExp::BinaryOp(add_exp, op, mul_exp) => match op {
                Operator::Add => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_add_exp(add_exp, function, bb),
                        self.visit_mul_exp(mul_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let add = value!(func_data, binary, BinaryOp::Add, l_value, r_value);
                        insertvalue!(func_data, *bb, [add]);
                        VisitRetType::Value(add)
                    } else {
                        panic!()
                    }
                }
                Operator::Subtract => {
                    if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                        self.visit_add_exp(add_exp, function, bb),
                        self.visit_mul_exp(mul_exp, function, bb),
                    ) {
                        let func_data = funcdata!(self, *function);
                        let sub = value!(func_data, binary, BinaryOp::Sub, l_value, r_value);
                        insertvalue!(func_data, *bb, [sub]);
                        VisitRetType::Value(sub)
                    } else {
                        panic!()
                    }
                }
                _ => {
                    panic!()
                }
            },
        }
    }

    fn visit_mul_exp(&mut self, mul_exp: &ast::MulExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global=>{
                unimplemented!()
            }
            VisitType::Local(function,bb )=>{
                match mul_exp {
                    MulExp::UnaryExp(unary_exp) => self.visit_unary_exp(unary_exp, visit_type),
                    MulExp::BinaryOp(mul_exp, op, unary_exp) => match op {
                        Operator::Multiply => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, visit_type),
                                self.visit_unary_exp(unary_exp, visit_type),
                            ) {
                                let func_data = funcdata!(self, *function);
                                let mul = value!(func_data, binary, BinaryOp::Mul, l_value, r_value);
                                insertvalue!(func_data, *bb, [mul]);
                                VisitRetType::Value(mul)
                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        }
                        Operator::Divide => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, function, bb),
                                self.visit_unary_exp(unary_exp, function, bb),
                            ) {
                                let func_data = funcdata!(self, *function);
                                let div = value!(func_data, binary, BinaryOp::Div, l_value, r_value);
                                insertvalue!(func_data, *bb, [div]);
                                VisitRetType::Value(div)
                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        }
                        Operator::GetRemainder => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, function, bb),
                                self.visit_unary_exp(unary_exp, function, bb),
                            ) {
                                let func_data = funcdata!(self, *function);
                                let mod_value = value!(func_data, binary, BinaryOp::Mod, l_value, r_value);
                                insertvalue!(func_data, *bb, [mod_value]);
                                VisitRetType::Value(mod_value)
                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!",op)
                        }
                    },
                }
            }
        }
    }

    fn visit_unary_exp(
        &mut self,
        unary_exp: &ast::UnaryExp,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type {
            VisitType::Global =>{
                unimplemented!()
            }
            VisitType::Local(function,bb)=>{
                match unary_exp {
                    UnaryExp::PrimaryExp(primary_exp) => self.visit_primary_exp(primary_exp, visit_type),
                    UnaryExp::UnaryOp(op, unary_exp) => match op {
                        Operator::Add => self.visit_unary_exp(unary_exp, visit_type),
                        Operator::Subtract => {
                            if let VisitRetType::Value(value) =
                                self.visit_unary_exp(unary_exp, visit_type)
                            {
                                // check if value is a const
                                let func_data = funcdata!(self, *function);
                                let value_data = getvaluedata!(func_data,value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer) =>{
                                        let number_value = value!(func_data,integer,-integer.value());
                                        removevalue!(func_data,value);
                                        VisitRetType::Value(number_value)
                                    }
                                    _=>{
                                        let zero_value = value!(func_data, integer, 0);
                                        let sub = value!(func_data, binary, BinaryOp::Sub, zero_value, value);
                                        insertvalue!(func_data, *bb, [sub]);
                                        VisitRetType::Value(sub)
                                    }
                                }
                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        }
                        Operator::Not => {
                            if let VisitRetType::Value(value) =
                                self.visit_unary_exp(unary_exp, visit_type)
                            {
                                // check if value is a const
                                let func_data = funcdata!(self, *function);
                                let value_data = getvaluedata!(func_data,value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer)=>{
                                        let number_value = value!(func_data,integer,!integer.value());
                                        removevalue!(func_data,value);
                                        VisitRetType::Value(number_value) 
                                    }
                                    _=>{
                                        let zero_value = value!(func_data, integer, 0);
                                        let eq = value!(func_data, binary, BinaryOp::Eq, zero_value, value);
                                        insertvalue!(func_data, *bb, [eq]);
                                        VisitRetType::Value(eq)
                                    }
                                }

                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!",op)
                        }
                    },
                }
            }
        }
    }

    fn visit_primary_exp(
        &mut self,
        primary_exp: &ast::PrimaryExp,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type {
            VisitType::Global => {
                unimplemented!()
                // match primary_exp {
                //     PrimaryExp::Exp(exp) => self.visit_exp(exp, visit_type),
                //     PrimaryExp::Number(number) => match number {
                //         Number::IntConst(int_const) => {
                //             let number_value = globalvalue!(self.program, integer, *int_const);
                //             VisitRetType::Value(number_value)
                //         }
                //     },
                //     PrimaryExp::LVal(lval) => {
                //         // look up symbol table...
                //         unimplemented!()
                //     }
                // }
            },
            VisitType::Local(function,_) => match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, visit_type),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        let func_data = funcdata!(self,*function);
                        let number_value = value!(func_data, integer, *int_const);
                        VisitRetType::Value(number_value)
                    }
                },
                PrimaryExp::LVal(lval) => {
                    // look up symbol table...
                    unimplemented!()
                }
            },
        }
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
        println!("{:#?}", ast);
        self.visit_comp_unit(&ast);
        Ok(&self.program)
    }

    pub fn gen_ir(&self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
