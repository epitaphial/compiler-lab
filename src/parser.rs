use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    rc::{Rc, Weak},
};

use koopa::{
    back::KoopaGenerator,
    ir::{
        builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
        BasicBlock, BinaryOp, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind,
    },
};

use crate::ast::{
    self, AddExp, BlockItem, EqExp, LAndExp, LOrExp, MulExp, Number, Operator, PrimaryExp, RelExp,
    Stmt, UnaryExp, Visitor,
};

lalrpop_mod!(pub sysy);

// scope design
/*
global:[
    func1:{
        block1-1:{
            block1-1-1{

            }
        }
    }
    func2:{
        block2-1:{

        }
    }
    ...
]
 */

#[derive(Debug, Default)]
struct BlockNode {
    // symbol name and symbol value
    pub symbols: RefCell<HashMap<String, Value>>,
    pub parent: RefCell<Option<Weak<BlockNode>>>,
    pub _childs: RefCell<Vec<Rc<BlockNode>>>,
}

impl BlockNode {
    pub fn new() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn insert_symbol(&self, name: String, value: Value) {
        if self.symbols.borrow().get(&name) == None {
            (*self.symbols.borrow_mut()).insert(name, value);
        } else {
            panic!("Redefined symbol: {}", &name);
        }
    }

    pub fn lookup_symbol(&self, name: &String) -> Value {
        if let Some(value) = self.symbols.borrow().get(name) {
            *value
        } else {
            let mut op_parent = (*self.parent.borrow()).clone();
            while let Some(parent) = &op_parent {
                // find symbols
                if let Some(value) = (*parent.upgrade().unwrap()).symbols.borrow().get(name) {
                    return *value;
                } else {
                    op_parent = (*parent.clone().upgrade().unwrap()).parent.borrow().clone();
                }
            }
            panic!("Undefined symbol:{}", &name);
        }
    }

    // pub fn insert_block(parent: Rc<BlockNode>, child: Rc<BlockNode>) {
    //     parent.childs.borrow_mut().push(child.clone());
    //     *child.parent.borrow_mut() = Some(Rc::downgrade(&parent));
    // }
}

// pub fn testtest() {
//     let child = BlockNode::new();
//     let parent = BlockNode::new();
//     BlockNode::insert_block(parent.clone(), child.clone());
//     println!("{:#?}", &parent.parent.borrow());
//     println!("{:#?}", child.parent.borrow().clone().unwrap().upgrade().unwrap().symbols);

//     for chi in parent.childs.borrow().iter() {
//         println!("{:#?}", chi.symbols);
//     }
// }

pub struct Parser {
    pub program: Program,
}

enum VisitRetType {
    Value(Value),
    None,
}

#[derive(Debug, Clone)]
enum VisitType {
    Global,
    Local(Function, BasicBlock, Rc<BlockNode>),
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

// macro_rules! globalvalue {
//     ($program:expr,$op:ident,$($value:expr),+)=>{
//         $program.new_value().$op($($value),+)
//     };
// }

macro_rules! insertvalue {
    ($func:ident,$bb:expr,$values:expr) => {
        $func.layout_mut().bb_mut($bb).insts_mut().extend($values)
    };
}

macro_rules! getvaluedata {
    ($func:ident,$value:expr) => {
        $func.dfg().value($value).clone()
    };
}

// macro_rules! removevalue {
//     ($func:ident,$value:expr) => {
//         $func.dfg_mut().remove_value($value)
//     };
// }

impl ast::Visitor<VisitRetType, VisitType> for Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit) -> VisitRetType {
        match comp_unit {
            ast::CompUnit::Decl(comp_unit, decl) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit);
                }
                self.visit_decl(decl, VisitType::Global);
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
                self.visit_const_decl(const_decl, visit_type);
            }
            ast::Decl::VarDecl(var_decl) => {
                self.visit_var_decl(var_decl, visit_type);
            }
        }
        VisitRetType::None
    }

    // global decl's lvalue must be a const
    fn visit_const_decl(
        &mut self,
        const_decl: &ast::ConstDecl,
        visit_type: VisitType,
    ) -> VisitRetType {
        // do some type check here...?
        for const_def in &const_decl.const_defs {
            println!("{:#?}", const_def.const_ident);
            self.visit_const_def(const_def, visit_type.clone());
        }
        VisitRetType::None
    }

    fn visit_const_def(
        &mut self,
        const_def: &ast::ConstDef,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => {
                unimplemented!()
            }
            VisitType::Local(_function, _bb, bn) => {
                if const_def.const_exps.len() == 0 {
                    if let VisitRetType::Value(value) =
                        self.visit_const_init_val(&const_def.const_init_val, visit_type)
                    {
                        bn.insert_symbol(const_def.const_ident.clone(), value);
                        VisitRetType::Value(value)
                    } else {
                        panic!("Value can not be VisitRetType::None!")
                    }
                } else {
                    unimplemented!()
                }
            }
        }
    }

    fn visit_const_init_val(
        &mut self,
        cons_init_val: &ast::ConstInitVal,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => unimplemented!(),
            VisitType::Local(_function, _bb, _bn) => match cons_init_val {
                ast::ConstInitVal::ConstExp(const_exp) => {
                    self.visit_const_exp(const_exp, visit_type)
                }
                ast::ConstInitVal::ConstInitVal(_const_init_val) => {
                    unimplemented!()
                }
            },
        }
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl, visit_type: VisitType) -> VisitRetType {
        // do some type check here...?
        for var_def in &var_decl.var_defs {
            self.visit_var_def(var_def, visit_type.clone());
        }
        VisitRetType::None
    }

    fn visit_var_def(&mut self, var_def: &ast::VarDef, visit_type: VisitType) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, bn) => {
                match var_def {
                    // with no initial vals
                    ast::VarDef::VarDef(var_ident, const_exps) => {
                        if const_exps.len() == 0 {
                            let func_data = funcdata!(self, function);
                            let alloc = value!(func_data, alloc, Type::get(TypeKind::Int32));
                            insertvalue!(func_data, bb, [alloc]);
                            bn.insert_symbol(var_ident.clone(), alloc);
                            VisitRetType::Value(alloc)
                        } else {
                            unimplemented!()
                        }
                    }
                    // with initial vals...
                    ast::VarDef::InitVal(var_ident, const_exps, init_val) => {
                        if const_exps.len() == 0 {
                            if let VisitRetType::Value(value) =
                                self.visit_init_val(init_val, visit_type)
                            {
                                let func_data = funcdata!(self, function);
                                let alloc = value!(func_data, alloc, Type::get(TypeKind::Int32));
                                let store = value!(func_data, store, value, alloc);
                                insertvalue!(func_data, bb, [alloc, store]);
                                bn.insert_symbol(var_ident.clone(), alloc);
                                VisitRetType::Value(alloc)
                            } else {
                                panic!("Value can not be VisitRetType::None!")
                            }
                        } else {
                            unimplemented!()
                        }
                    }
                }
            }
        }
    }

    fn visit_init_val(&mut self, init_val: &ast::InitVal, visit_type: VisitType) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => unimplemented!(),
            VisitType::Local(_function, _bb, _bn) => match init_val {
                ast::InitVal::Exp(exp) => self.visit_exp(exp, visit_type),
                ast::InitVal::InitVal(_init_val) => unimplemented!(),
            },
        }
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
        let block_node = BlockNode::new(); // maybe consider param symbols...
        self.visit_block(
            &func_def.block,
            VisitType::Local(function, entry_bb, block_node),
        );
        VisitRetType::None
    }

    fn visit_block(&mut self, block: &crate::ast::Block, visit_type: VisitType) -> VisitRetType {
        for block_item in &block.block_items {
            self.visit_block_item(block_item, visit_type.clone());
        }
        VisitRetType::None
    }

    fn visit_block_item(
        &mut self,
        block_item: &crate::ast::BlockItem,
        visit_type: VisitType,
    ) -> VisitRetType {
        match block_item {
            BlockItem::Stmt(stmt) => {
                self.visit_stmt(stmt, visit_type);
            }
            BlockItem::Decl(decl) => {
                self.visit_decl(decl, visit_type);
            }
        }
        VisitRetType::None
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt, visit_type: VisitType) -> VisitRetType {
        match stmt {
            Stmt::RetStmt(ret_stmt) => {
                self.visit_ret_stmt(ret_stmt, visit_type);
            }
            Stmt::AssignStmt(_ass_stmt) => {}
        }
        VisitRetType::None
    }

    fn visit_ret_stmt(
        &mut self,
        ret_stmt: &crate::ast::RetStmt,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => panic!("Value can not be {:#?}", visit_type),
            VisitType::Local(function, bb, _bn) => {
                if let Some(exp) = &ret_stmt.exp {
                    match self.visit_exp(exp, visit_type) {
                        VisitRetType::Value(value) => {
                            let func_data = funcdata!(self, function);
                            let ret = value!(func_data, ret, Some(value));
                            insertvalue!(func_data, bb, [ret]);
                        }
                        VisitRetType::None => {
                            panic!("Return value can not be VisitRetType::None")
                        }
                    }
                } else {
                    let func_data = funcdata!(self, function);
                    let ret = value!(func_data, ret, None);
                    insertvalue!(func_data, bb, [ret]);
                }
            }
        }
        VisitRetType::None
    }

    fn visit_ass_stmt(
        &mut self,
        _ass_stmt: &ast::AssignStmt,
        _visit_type: VisitType,
    ) -> VisitRetType {
        unimplemented!();
    }

    fn visit_l_val(&mut self, l_val: &ast::LVal, visit_type: VisitType) -> VisitRetType {
        // look up symbol table...
        match visit_type {
            VisitType::Global => {
                unimplemented!()
            }
            VisitType::Local(function, bb, bn) => {
                let value = bn.lookup_symbol(&l_val.ident);
                let func_data = funcdata!(self,function);
                let value_data = getvaluedata!(func_data,value);
                if matches!(value_data.kind(),ValueKind::Alloc(_)){
                    let load = value!(func_data,load,value);
                    insertvalue!(func_data, bb, [load]);
                    VisitRetType::Value(load)
                }else{
                    VisitRetType::Value(value)
                }
            }
        }
    }

    fn visit_const_exp(
        &mut self,
        const_exp: &ast::ConstExp,
        visit_type: VisitType,
    ) -> VisitRetType {
        // should check if it is a const...
        match visit_type.clone() {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, _bb, _bn) => {
                let const_value = self.visit_exp(&const_exp.exp, visit_type.clone());
                if let VisitRetType::Value(value) = const_value {
                    let func_data = funcdata!(self, function);
                    let val_data = getvaluedata!(func_data, value);
                    if let ValueKind::Integer(_) = val_data.kind() {
                        const_value
                    } else {
                        panic!(
                            "Const expression's rvalue must be a constant, but got: {:#?}",
                            val_data.kind()
                        )
                    }
                } else {
                    panic!("Value can not be VisitRetType::None!")
                }
            }
        }
    }

    fn visit_exp(&mut self, exp: &crate::ast::Exp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(_function, _bb, _) => self.visit_l_or_exp(&exp.l_or_exp, visit_type),
        }
    }

    fn visit_l_or_exp(&mut self, l_or_exp: &ast::LOrExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _) => {
                match l_or_exp {
                    LOrExp::LAndExp(l_and_exp) => self.visit_l_and_exp(l_and_exp, visit_type),
                    LOrExp::BinaryOp(l_or_exp, l_and_exp) => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_l_or_exp(l_or_exp, visit_type.clone()),
                            self.visit_l_and_exp(l_and_exp, visit_type.clone()),
                        ) {
                            let func_data = funcdata!(self, function);
                            // check if both lval and rval are const
                            let l_val_data = getvaluedata!(func_data, l_value);
                            let r_val_data = getvaluedata!(func_data, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value = value!(
                                    func_data,
                                    integer,
                                    (l_num.value() != 0 || r_num.value() != 0) as i32
                                );
                                //removevalue!(func_data, l_value);
                                //removevalue!(func_data, r_value);
                                VisitRetType::Value(number_value)
                            } else {
                                let or = value!(func_data, binary, BinaryOp::Or, l_value, r_value);
                                let zero_value = value!(func_data, integer, 0);
                                let eq = value!(func_data, binary, BinaryOp::NotEq, or, zero_value);
                                insertvalue!(func_data, bb, [or, eq]);
                                VisitRetType::Value(eq)
                            }
                        } else {
                            panic!("Value can not be {:#?}", visit_type)
                        }
                    }
                }
            }
        }
    }

    fn visit_l_and_exp(&mut self, l_and_exp: &ast::LAndExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _) => {
                match l_and_exp {
                    LAndExp::EqExp(eq_exp) => self.visit_eq_exp(eq_exp, visit_type),
                    LAndExp::BinaryOp(l_and_exp, eq_exp) => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_l_and_exp(l_and_exp, visit_type.clone()),
                            self.visit_eq_exp(eq_exp, visit_type.clone()),
                        ) {
                            let func_data = funcdata!(self, function);
                            // check if both lval and rval are const
                            let l_val_data = getvaluedata!(func_data, l_value);
                            let r_val_data = getvaluedata!(func_data, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value = value!(
                                    func_data,
                                    integer,
                                    (l_num.value() != 0 && r_num.value() != 0) as i32
                                );
                                //removevalue!(func_data, l_value);
                                //removevalue!(func_data, r_value);
                                VisitRetType::Value(number_value)
                            } else {
                                let zero_value = value!(func_data, integer, 0);
                                let neq_l =
                                    value!(func_data, binary, BinaryOp::NotEq, l_value, zero_value);
                                let neq_r =
                                    value!(func_data, binary, BinaryOp::NotEq, r_value, zero_value);
                                let and = value!(func_data, binary, BinaryOp::And, neq_l, neq_r);
                                let neq =
                                    value!(func_data, binary, BinaryOp::NotEq, and, zero_value);
                                insertvalue!(func_data, bb, [neq_l, neq_r, and, neq]);
                                VisitRetType::Value(neq)
                            }
                        } else {
                            panic!("Value can not be {:#?}", visit_type)
                        }
                    }
                }
            }
        }
    }

    fn visit_eq_exp(&mut self, eq_exp: &ast::EqExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _) => {
                match eq_exp {
                    EqExp::RelExp(rel_exp) => self.visit_rel_exp(rel_exp, visit_type),
                    EqExp::BinaryOp(eq_exp, op, rel_exp) => match op {
                        Operator::Equal => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_eq_exp(eq_exp, visit_type.clone()),
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() == r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let eq =
                                        value!(func_data, binary, BinaryOp::Eq, l_value, r_value);
                                    insertvalue!(func_data, bb, [eq]);
                                    VisitRetType::Value(eq)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::NotEqual => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_eq_exp(eq_exp, visit_type.clone()),
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() != r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let not_eq = value!(
                                        func_data,
                                        binary,
                                        BinaryOp::NotEq,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(func_data, bb, [not_eq]);
                                    VisitRetType::Value(not_eq)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!", op)
                        }
                    },
                }
            }
        }
    }

    fn visit_rel_exp(&mut self, rel_exp: &ast::RelExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _) => {
                match rel_exp {
                    RelExp::AddExp(add_exp) => self.visit_add_exp(add_exp, visit_type),
                    RelExp::BinaryOp(rel_exp, op, add_exp) => match op {
                        Operator::LessThan => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                                self.visit_add_exp(add_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() < r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let lt =
                                        value!(func_data, binary, BinaryOp::Lt, l_value, r_value);
                                    insertvalue!(func_data, bb, [lt]);
                                    VisitRetType::Value(lt)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::MoreThan => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                                self.visit_add_exp(add_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() > r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mt =
                                        value!(func_data, binary, BinaryOp::Gt, l_value, r_value);
                                    insertvalue!(func_data, bb, [mt]);
                                    VisitRetType::Value(mt)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::LessOrEqualThan => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                                self.visit_add_exp(add_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() <= r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let le =
                                        value!(func_data, binary, BinaryOp::Le, l_value, r_value);
                                    insertvalue!(func_data, bb, [le]);
                                    VisitRetType::Value(le)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::MoreOrEqualThan => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                                self.visit_add_exp(add_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        func_data,
                                        integer,
                                        (l_num.value() >= r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let me =
                                        value!(func_data, binary, BinaryOp::Ge, l_value, r_value);
                                    insertvalue!(func_data, bb, [me]);
                                    VisitRetType::Value(me)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!", op)
                        }
                    },
                }
            }
        }
    }

    fn visit_add_exp(&mut self, add_exp: &ast::AddExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _) => match add_exp {
                AddExp::MulExp(mul_exp) => self.visit_mul_exp(mul_exp, visit_type),
                AddExp::BinaryOp(add_exp, op, mul_exp) => match op {
                    Operator::Add => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_add_exp(add_exp, visit_type.clone()),
                            self.visit_mul_exp(mul_exp, visit_type.clone()),
                        ) {
                            let func_data = funcdata!(self, function);
                            // check if both lval and rval are const
                            let l_val_data = getvaluedata!(func_data, l_value);
                            let r_val_data = getvaluedata!(func_data, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value =
                                    value!(func_data, integer, l_num.value() + r_num.value());
                                //removevalue!(func_data, l_value);
                                //removevalue!(func_data, r_value);
                                VisitRetType::Value(number_value)
                            } else {
                                let add =
                                    value!(func_data, binary, BinaryOp::Add, l_value, r_value);
                                insertvalue!(func_data, bb, [add]);
                                VisitRetType::Value(add)
                            }
                        } else {
                            panic!("Value can not be {:#?}", visit_type)
                        }
                    }
                    Operator::Subtract => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_add_exp(add_exp, visit_type.clone()),
                            self.visit_mul_exp(mul_exp, visit_type.clone()),
                        ) {
                            let func_data = funcdata!(self, function);
                            // check if both lval and rval are const
                            let l_val_data = getvaluedata!(func_data, l_value);
                            let r_val_data = getvaluedata!(func_data, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value =
                                    value!(func_data, integer, l_num.value() - r_num.value());
                                //removevalue!(func_data, l_value);
                                //removevalue!(func_data, r_value);
                                VisitRetType::Value(number_value)
                            } else {
                                let sub =
                                    value!(func_data, binary, BinaryOp::Sub, l_value, r_value);
                                insertvalue!(func_data, bb, [sub]);
                                VisitRetType::Value(sub)
                            }
                        } else {
                            panic!("Value can not be {:#?}", visit_type)
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            },
        }
    }

    fn visit_mul_exp(&mut self, mul_exp: &ast::MulExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => {
                unimplemented!()
            }
            VisitType::Local(function, bb, _) => {
                match mul_exp {
                    MulExp::UnaryExp(unary_exp) => self.visit_unary_exp(unary_exp, visit_type),
                    MulExp::BinaryOp(mul_exp, op, unary_exp) => match op {
                        Operator::Multiply => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, visit_type.clone()),
                                self.visit_unary_exp(unary_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value =
                                        value!(func_data, integer, l_num.value() * r_num.value());
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mul =
                                        value!(func_data, binary, BinaryOp::Mul, l_value, r_value);
                                    insertvalue!(func_data, bb, [mul]);
                                    VisitRetType::Value(mul)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::Divide => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, visit_type.clone()),
                                self.visit_unary_exp(unary_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value =
                                        value!(func_data, integer, l_num.value() / r_num.value());
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let div =
                                        value!(func_data, binary, BinaryOp::Div, l_value, r_value);
                                    insertvalue!(func_data, bb, [div]);
                                    VisitRetType::Value(div)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::GetRemainder => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, visit_type.clone()),
                                self.visit_unary_exp(unary_exp, visit_type.clone()),
                            ) {
                                let func_data = funcdata!(self, function);
                                // check if both lval and rval are const
                                let l_val_data = getvaluedata!(func_data, l_value);
                                let r_val_data = getvaluedata!(func_data, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value =
                                        value!(func_data, integer, l_num.value() % r_num.value());
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mod_value =
                                        value!(func_data, binary, BinaryOp::Mod, l_value, r_value);
                                    insertvalue!(func_data, bb, [mod_value]);
                                    VisitRetType::Value(mod_value)
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!", op)
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
            VisitType::Global => {
                unimplemented!()
            }
            VisitType::Local(function, bb, _) => {
                match unary_exp {
                    UnaryExp::PrimaryExp(primary_exp) => {
                        self.visit_primary_exp(primary_exp, visit_type)
                    }
                    UnaryExp::UnaryOp(op, unary_exp) => match op {
                        Operator::Add => self.visit_unary_exp(unary_exp, visit_type),
                        Operator::Subtract => {
                            if let VisitRetType::Value(value) =
                                self.visit_unary_exp(unary_exp, visit_type.clone())
                            {
                                // check if value is a const
                                let func_data = funcdata!(self, function);
                                let value_data = getvaluedata!(func_data, value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer) => {
                                        let number_value =
                                            value!(func_data, integer, -integer.value());
                                        //removevalue!(func_data, value);
                                        VisitRetType::Value(number_value)
                                    }
                                    _ => {
                                        let zero_value = value!(func_data, integer, 0);
                                        let sub = value!(
                                            func_data,
                                            binary,
                                            BinaryOp::Sub,
                                            zero_value,
                                            value
                                        );
                                        insertvalue!(func_data, bb, [sub]);
                                        VisitRetType::Value(sub)
                                    }
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        Operator::Not => {
                            if let VisitRetType::Value(value) =
                                self.visit_unary_exp(unary_exp, visit_type.clone())
                            {
                                // check if value is a const
                                let func_data = funcdata!(self, function);
                                let value_data = getvaluedata!(func_data, value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer) => {
                                        let number_value = value!(
                                            func_data,
                                            integer,
                                            (integer.value() == 0) as i32
                                        );
                                        //removevalue!(func_data, value);
                                        VisitRetType::Value(number_value)
                                    }
                                    _ => {
                                        let zero_value = value!(func_data, integer, 0);
                                        let eq = value!(
                                            func_data,
                                            binary,
                                            BinaryOp::Eq,
                                            zero_value,
                                            value
                                        );
                                        insertvalue!(func_data, bb, [eq]);
                                        VisitRetType::Value(eq)
                                    }
                                }
                            } else {
                                panic!("Value can not be {:#?}", visit_type)
                            }
                        }
                        _ => {
                            panic!("Illegal Operator: {:#?}!", op)
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
            }
            VisitType::Local(function, _, _) => match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, visit_type),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        let func_data = funcdata!(self, function);
                        let number_value = value!(func_data, integer, *int_const);
                        VisitRetType::Value(number_value)
                    }
                },
                PrimaryExp::LVal(l_val) => self.visit_l_val(l_val, visit_type),
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
        // testtest();
        self.visit_comp_unit(&ast);
        Ok(&self.program)
    }

    pub fn gen_ir(&self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
