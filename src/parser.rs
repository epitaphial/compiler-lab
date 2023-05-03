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
    pub childs: RefCell<Vec<Rc<BlockNode>>>,
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

    pub fn insert_block(parent: Rc<BlockNode>, child: Rc<BlockNode>) {
        parent.childs.borrow_mut().push(child.clone());
        *child.parent.borrow_mut() = Some(Rc::downgrade(&parent));
    }
}

#[derive(Debug)]
enum BBFlowStatus {
    PushEnable(Option<(Function, BasicBlock)>), // Need to jump back to original bb?
    PushDisable(Option<Function>),              // Need to jump to the return point
}

pub struct Parser {
    program: Program,
    bb_number: HashMap<Function, u32>,
    bb_flow_change: HashMap<BasicBlock, BBFlowStatus>,
    return_value: HashMap<Function, (BasicBlock, Value)>, // the last basicblock of function and the value store the return value.
    return_in_entry: HashMap<Function, bool>, // If already return in entry, we won't generate new basicblock...
}

#[derive(Debug, Clone, Copy)]
enum VisitRetType {
    Value(Value),
    BasicBlock(BasicBlock),
    None,
}

#[derive(Debug, Clone)]
enum VisitType {
    Global,
    // the second BasicBlock is last if's END BasicBlock
    Local(Function, BasicBlock, Rc<BlockNode>, Option<BasicBlock>),
}

macro_rules! funcdata {
    ($self:ident,$func:expr) => {
        $self.program.func_mut($func)
    };
}

macro_rules! imfuncdata {
    ($self:ident,$func:expr) => {
        $self.program.func($func)
    };
}

macro_rules! bbdata {
    ($self:ident,$func:expr,$bb:expr) => {
        funcdata!($self, $func).dfg_mut().bb_mut($bb)
    };
}

macro_rules! imbbdata {
    ($self:ident,$func:expr,$bb:expr) => {
        imfuncdata!($self, $func).dfg().bb($bb)
    };
}

macro_rules! basicblock {
    ($self:ident,$func:ident,$bbname:expr) => {
        funcdata!($self, $func)
            .dfg_mut()
            .new_bb()
            .basic_block($bbname)
    };
}

macro_rules! insertbb {
    ($self:ident,$func:ident,$bb:expr) => {
        if matches!($self.return_in_entry.get(&$func), None) {
            funcdata!($self, $func).layout_mut().bbs_mut().extend($bb)
        }
        //$func.layout_mut().bbs_mut().extend($bb)
    };
}

macro_rules! value {
    ($self:ident,$func:expr,$op:ident,$($value:expr),+)=>{
        funcdata!($self,$func).dfg_mut().new_value().$op($($value),+)
    };
}

// macro_rules! globalvalue {
//     ($program:expr,$op:ident,$($value:expr),+)=>{
//         $program.new_value().$op($($value),+)
//     };
// }

macro_rules! insertvalue {
    ($self:ident,$func:expr,$bb:expr,$values:expr) => {
        // Eliminate redundant instructions after the ret instruction
        if matches!(
            $self.bb_flow_change.get(&$bb).unwrap(),
            BBFlowStatus::PushEnable(_)
        ) && matches!($self.return_in_entry.get(&$func), None)
        {
            funcdata!($self, $func)
                .layout_mut()
                .bb_mut($bb)
                .insts_mut()
                .extend($values)
        }
    };
}

macro_rules! insertvalueforce {
    ($self:ident,$func:expr,$bb:expr,$values:expr) => {
        funcdata!($self, $func)
            .layout_mut()
            .bb_mut($bb)
            .insts_mut()
            .extend($values)
    };
}

macro_rules! isbbenable {
    ($self:ident,$bb:expr) => {
        // Eliminate redundant instructions after the ret instruction
        if matches!(
            $self.bb_flow_change.get(&$bb).unwrap(),
            BBFlowStatus::PushDisable(_)
        ) {
            false
        } else {
            true
        }
    };
}

macro_rules! imvaluedata {
    ($self:ident,$func:expr,$value:expr) => {
        imfuncdata!($self, $func).dfg().value($value).clone()
    };
}

macro_rules! removevalue {
    ($self:ident,$func:expr,$value:expr) => {
        funcdata!($self, $func).dfg_mut().remove_value($value)
    };
}

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
            VisitType::Local(_function, _bb, bn, _) => {
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
            VisitType::Local(_function, _bb, _bn, _) => match cons_init_val {
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
            VisitType::Local(function, bb, bn, _) => {
                match var_def {
                    // with no initial vals
                    ast::VarDef::VarDef(var_ident, const_exps) => {
                        if const_exps.len() == 0 {
                            let alloc = value!(self, function, alloc, Type::get(TypeKind::Int32));
                            insertvalue!(self, function, bb, [alloc]);
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
                                let alloc =
                                    value!(self, function, alloc, Type::get(TypeKind::Int32));
                                let store = value!(self, function, store, value, alloc);
                                insertvalue!(self, function, bb, [alloc, store]);
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
            VisitType::Local(_function, _bb, _bn, _) => match init_val {
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
        self.bb_number.insert(function, 0);
        let entry_bb = basicblock!(self, function, Some("%entry".into()));
        self.bb_flow_change
            .insert(entry_bb, BBFlowStatus::PushEnable(None));
        insertbb!(self, function, [entry_bb]);

        // alloc value to store return value...
        if matches!(func_def.func_type, ast::Type::Int) {
            let alloc = value!(self, function, alloc, Type::get(TypeKind::Int32));
            insertvalue!(self, function, entry_bb, [alloc]);
            self.return_value.insert(function, (entry_bb, alloc));
        }

        let block_node = BlockNode::new(); // maybe consider param symbols...
                                           /*let visit_ret_type = */
        self.visit_block(
            &func_def.block,
            VisitType::Local(function, entry_bb, block_node, None),
        );
        // if let VisitRetType::BasicBlock(bb) = visit_ret_type{
        //     println!("{}",bbdata!(self,function,bb).name().clone().unwrap());
        // }
        VisitRetType::None
    }

    fn visit_block(&mut self, block: &crate::ast::Block, visit_type: VisitType) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => {
                panic!("Can not in global scope!")
            }
            VisitType::Local(function, _bb, bn, last_bb) => {
                let mut visit_type = visit_type.clone();
                for block_item in &block.block_items {
                    let visit_ret_type = self.visit_block_item(block_item, visit_type.clone());
                    // change the bb to if end
                    if let VisitRetType::BasicBlock(end_bb) = visit_ret_type {
                        visit_type = VisitType::Local(function, end_bb, bn.clone(), last_bb);
                    }
                }
                // if let VisitType::Local(function, end_bb, _, _) = visit_type {
                //     VisitRetType::BasicBlock(end_bb)
                // } else {
                //     VisitRetType::None
                // }
                VisitRetType::None
            }
        }
    }

    fn visit_block_item(
        &mut self,
        block_item: &crate::ast::BlockItem,
        visit_type: VisitType,
    ) -> VisitRetType {
        match block_item {
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt, visit_type),
            BlockItem::Decl(decl) => self.visit_decl(decl, visit_type),
        }
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt, visit_type: VisitType) -> VisitRetType {
        match stmt {
            Stmt::RetStmt(ret_stmt) => self.visit_ret_stmt(ret_stmt, visit_type),
            Stmt::AssignStmt(ass_stmt) => self.visit_ass_stmt(ass_stmt, visit_type),
            Stmt::ExpStmt(exp_stmt) => self.visit_exp_stmt(exp_stmt, visit_type),
            Stmt::BlockStmt(block_stmt) => self.visit_block_stmt(block_stmt, visit_type),
            Stmt::IfStmt(if_stmt) => self.visit_if_stmt(if_stmt, visit_type),
        }
    }

    fn visit_if_stmt(&mut self, if_stmt: &ast::IfStmt, visit_type: VisitType) -> VisitRetType {
        // 1. deal with exp; 2. new bb for then; 3. new bb for else; 4. new bb for end; 5. add jmp from then and else to end, and dont forget to change folowing block item's bb, and jump back to original bb.
        match visit_type.clone() {
            VisitType::Global => {
                panic!("Can not be {:#?}", visit_type)
            }
            VisitType::Local(function, bb, bn, last_bb) => {
                if let VisitRetType::Value(exp_value) =
                    self.visit_exp(&if_stmt.cond_exp, visit_type.clone())
                {
                    if let Some(else_stmt) = &if_stmt.else_stmt {
                        // with else branch
                        let then_bb_lable = self.get_bb_number(function);
                        let else_bb_lable = self.get_bb_number(function);
                        let then_bb = basicblock!(self, function, Some(then_bb_lable));
                        self.bb_flow_change
                            .insert(then_bb, BBFlowStatus::PushEnable(None));
                        let else_bb = basicblock!(self, function, Some(else_bb_lable));
                        self.bb_flow_change
                            .insert(else_bb, BBFlowStatus::PushEnable(None));
                        let end_bb = basicblock!(self, function, None);
                        if let Some(last_bb) = last_bb {
                            self.bb_flow_change.insert(
                                end_bb,
                                BBFlowStatus::PushEnable(Some((function, last_bb))),
                            );
                        } else {
                            self.bb_flow_change
                                .insert(end_bb, BBFlowStatus::PushEnable(None));
                        }
                        let then_visit_type =
                            VisitType::Local(function, then_bb, bn.clone(), Some(end_bb));
                        let else_visit_type =
                            VisitType::Local(function, else_bb, bn.clone(), Some(end_bb));
                        insertbb!(self, function, [then_bb, else_bb]);

                        let br_value = value!(self, function, branch, exp_value, then_bb, else_bb);
                        insertvalue!(self, function, bb, [br_value]);
                        self.bb_flow_change
                            .insert(bb, BBFlowStatus::PushDisable(None));

                        self.visit_stmt(&if_stmt.then_stmt, then_visit_type);
                        self.visit_stmt(else_stmt, else_visit_type);

                        // insert jump to end_bb
                        let end_bb_lable = self.get_bb_number(function);

                        if isbbenable!(self, then_bb) {
                            let then_jmp_value = value!(self, function, jump, end_bb);
                            insertvalue!(self, function, then_bb, [then_jmp_value]);
                            self.bb_flow_change
                                .insert(then_bb, BBFlowStatus::PushDisable(None));
                        }

                        if isbbenable!(self, else_bb) {
                            let else_jmp_value = value!(self, function, jump, end_bb);
                            insertvalue!(self, function, else_bb, [else_jmp_value]);
                            self.bb_flow_change
                                .insert(else_bb, BBFlowStatus::PushDisable(None));
                        }

                        let bb_data = bbdata!(self, function, end_bb);
                        bb_data.set_name(Some(end_bb_lable));
                        insertbb!(self, function, [end_bb]);
                        VisitRetType::BasicBlock(end_bb)
                    } else {
                        // without else branch
                        let then_bb_lable = self.get_bb_number(function);

                        let then_bb = basicblock!(self, function, Some(then_bb_lable));
                        self.bb_flow_change
                            .insert(then_bb, BBFlowStatus::PushEnable(None));
                        let end_bb = basicblock!(self, function, None);
                        if let Some(last_bb) = last_bb {
                            self.bb_flow_change.insert(
                                end_bb,
                                BBFlowStatus::PushEnable(Some((function, last_bb))),
                            );
                        } else {
                            self.bb_flow_change
                                .insert(end_bb, BBFlowStatus::PushEnable(None));
                        }

                        let then_visit_type =
                            VisitType::Local(function, then_bb, bn.clone(), Some(end_bb));
                        insertbb!(self, function, [then_bb]);

                        self.visit_stmt(&if_stmt.then_stmt, then_visit_type);

                        // insert jump to end_bb
                        let end_bb_lable = self.get_bb_number(function);

                        if isbbenable!(self, bb) {
                            let br_value =
                                value!(self, function, branch, exp_value, then_bb, end_bb);
                            insertvalue!(self, function, bb, [br_value]);
                            self.bb_flow_change
                                .insert(bb, BBFlowStatus::PushDisable(None));
                        }

                        if isbbenable!(self, then_bb) {
                            let then_jmp_value = value!(self, function, jump, end_bb);
                            insertvalue!(self, function, then_bb, [then_jmp_value]);
                            self.bb_flow_change
                                .insert(then_bb, BBFlowStatus::PushDisable(None));
                        }

                        let bb_data = bbdata!(self, function, end_bb);
                        bb_data.set_name(Some(end_bb_lable));
                        insertbb!(self, function, [end_bb]);
                        VisitRetType::BasicBlock(end_bb)
                    }
                } else {
                    panic!("Condition must be a expresson")
                }
            }
        }
    }

    fn visit_block_stmt(
        &mut self,
        block_stmt: &ast::BlockStmt,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => {
                panic!("Can not be {:#?}", visit_type)
            }
            VisitType::Local(function, bb, bn, last_bb) => {
                let new_node = BlockNode::new();
                BlockNode::insert_block(bn.clone(), new_node.clone());
                let visit_type = VisitType::Local(function, bb, new_node, last_bb);
                self.visit_block(&block_stmt.block, visit_type)
            }
        }
    }

    fn visit_ret_stmt(
        &mut self,
        ret_stmt: &crate::ast::RetStmt,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => panic!("Value can not be {:#?}", visit_type),
            VisitType::Local(function, bb, _bn, _) => {
                if let Some(exp) = &ret_stmt.exp {
                    match self.visit_exp(exp, visit_type) {
                        VisitRetType::Value(value) => {
                            if bbdata!(self, function, bb).name().clone().unwrap() == "%entry" {
                                let ret = value!(self, function, ret, Some(value));
                                insertvalue!(self, function, bb, [ret]);
                                self.return_in_entry.insert(function, true);
                                self.bb_flow_change
                                    .insert(bb, BBFlowStatus::PushDisable(None));
                            } else {
                                // new an end block and jump to it...
                                let return_value = self.return_value.get(&function).unwrap();
                                let store = value!(self, function, store, value, return_value.1);
                                insertvalue!(self, function, bb, [store]);
                                self.bb_flow_change
                                    .insert(bb, BBFlowStatus::PushDisable(Some(function)));
                            }
                        }
                        VisitRetType::None => {
                            panic!("Return exp value can not be VisitRetType::None!")
                        }
                        VisitRetType::BasicBlock(_) => {
                            panic!("Return exp value can not be VisitRetType::BasicBlock!")
                        }
                    }
                } else {
                    // void function
                    unimplemented!();
                    // let func_data = funcdata!(self, function);
                    // let ret = value!(func_data, ret, None);
                    // insertvalue!(self, func_data, bb, [ret]);
                }
            }
        }
        VisitRetType::None
    }

    fn visit_exp_stmt(&mut self, exp_stmt: &ast::ExpStmt, visit_type: VisitType) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => {
                panic!("Can not be {:#?}", visit_type)
            }
            VisitType::Local(_, _, _, _) => {
                if let Some(exp) = &exp_stmt.exp {
                    self.visit_exp(exp, visit_type);
                }
                VisitRetType::None
            }
        }
    }

    fn visit_ass_stmt(
        &mut self,
        ass_stmt: &ast::AssignStmt,
        visit_type: VisitType,
    ) -> VisitRetType {
        match visit_type.clone() {
            VisitType::Global => {
                panic!("Value can not be {:#?}", visit_type)
            }
            VisitType::Local(function, bb, bn, _) => {
                if let VisitRetType::Value(value) = self.visit_exp(&ass_stmt.exp, visit_type) {
                    let ass_l_value = bn.lookup_symbol(&ass_stmt.l_val.ident);
                    // must do some type check, because we can't assign to a const!
                    let store = value!(self, function, store, value, ass_l_value);
                    insertvalue!(self, function, bb, [store]);
                } else {
                    panic!()
                }
            }
        }
        VisitRetType::None
    }

    fn visit_l_val(&mut self, l_val: &ast::LVal, visit_type: VisitType) -> VisitRetType {
        // look up symbol table...
        match visit_type {
            VisitType::Global => {
                unimplemented!()
            }
            VisitType::Local(function, bb, bn, _) => {
                let value = bn.lookup_symbol(&l_val.ident);
                let value_data = imvaluedata!(self, function, value);
                if matches!(value_data.kind(), ValueKind::Alloc(_)) {
                    let load = value!(self, function, load, value);
                    insertvalue!(self, function, bb, [load]);
                    VisitRetType::Value(load)
                } else {
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
            VisitType::Local(function, _bb, _bn, _) => {
                let const_value = self.visit_exp(&const_exp.exp, visit_type.clone());
                if let VisitRetType::Value(value) = const_value {
                    let val_data = imvaluedata!(self, function, value);
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
            VisitType::Local(_function, _bb, _, _) => {
                self.visit_l_or_exp(&exp.l_or_exp, visit_type)
            }
        }
    }

    fn visit_l_or_exp(&mut self, l_or_exp: &ast::LOrExp, visit_type: VisitType) -> VisitRetType {
        match visit_type {
            VisitType::Global => unimplemented!(),
            VisitType::Local(function, bb, _, _) => {
                match l_or_exp {
                    LOrExp::LAndExp(l_and_exp) => self.visit_l_and_exp(l_and_exp, visit_type),
                    LOrExp::BinaryOp(l_or_exp, l_and_exp) => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_l_or_exp(l_or_exp, visit_type.clone()),
                            self.visit_l_and_exp(l_and_exp, visit_type.clone()),
                        ) {
                            // check if both lval or rval are const
                            let l_val_data = imvaluedata!(self, function, l_value);
                            let r_val_data = imvaluedata!(self, function, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value = value!(
                                    self,
                                    function,
                                    integer,
                                    (l_num.value() != 0 || r_num.value() != 0) as i32
                                );
                                //removevalue!(func_data, l_value);
                                //removevalue!(func_data, r_value);
                                VisitRetType::Value(number_value)
                            } else {
                                let or =
                                    value!(self, function, binary, BinaryOp::Or, l_value, r_value);
                                let zero_value = value!(self, function, integer, 0);
                                let eq =
                                    value!(self, function, binary, BinaryOp::NotEq, or, zero_value);
                                insertvalue!(self, function, bb, [or, eq]);
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
            VisitType::Local(function, bb, _, _) => {
                match l_and_exp {
                    LAndExp::EqExp(eq_exp) => self.visit_eq_exp(eq_exp, visit_type),
                    LAndExp::BinaryOp(l_and_exp, eq_exp) => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_l_and_exp(l_and_exp, visit_type.clone()),
                            self.visit_eq_exp(eq_exp, visit_type.clone()),
                        ) {
                            // check if both lval and rval are const
                            let l_val_data = imvaluedata!(self, function, l_value);
                            let r_val_data = imvaluedata!(self, function, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value = value!(
                                    self,
                                    function,
                                    integer,
                                    (l_num.value() != 0 && r_num.value() != 0) as i32
                                );
                                VisitRetType::Value(number_value)
                            } else {
                                let zero_value = value!(self, function, integer, 0);
                                let neq_l = value!(
                                    self,
                                    function,
                                    binary,
                                    BinaryOp::NotEq,
                                    l_value,
                                    zero_value
                                );
                                let neq_r = value!(
                                    self,
                                    function,
                                    binary,
                                    BinaryOp::NotEq,
                                    r_value,
                                    zero_value
                                );
                                let and =
                                    value!(self, function, binary, BinaryOp::And, neq_l, neq_r);
                                let neq = value!(
                                    self,
                                    function,
                                    binary,
                                    BinaryOp::NotEq,
                                    and,
                                    zero_value
                                );
                                insertvalue!(self, function, bb, [neq_l, neq_r, and, neq]);
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
            VisitType::Local(function, bb, _, _) => {
                match eq_exp {
                    EqExp::RelExp(rel_exp) => self.visit_rel_exp(rel_exp, visit_type),
                    EqExp::BinaryOp(eq_exp, op, rel_exp) => match op {
                        Operator::Equal => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_eq_exp(eq_exp, visit_type.clone()),
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                            ) {
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() == r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let eq = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Eq,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [eq]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() != r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let not_eq = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::NotEq,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [not_eq]);
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
            VisitType::Local(function, bb, _, _) => {
                match rel_exp {
                    RelExp::AddExp(add_exp) => self.visit_add_exp(add_exp, visit_type),
                    RelExp::BinaryOp(rel_exp, op, add_exp) => match op {
                        Operator::LessThan => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_rel_exp(rel_exp, visit_type.clone()),
                                self.visit_add_exp(add_exp, visit_type.clone()),
                            ) {
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() < r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let lt = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Lt,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [lt]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() > r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mt = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Gt,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [mt]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() <= r_num.value()) as i32
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let le = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Le,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [le]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        (l_num.value() >= r_num.value()) as i32
                                    );
                                    VisitRetType::Value(number_value)
                                } else {
                                    let me = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Ge,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [me]);
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
            VisitType::Local(function, bb, _, _) => match add_exp {
                AddExp::MulExp(mul_exp) => self.visit_mul_exp(mul_exp, visit_type),
                AddExp::BinaryOp(add_exp, op, mul_exp) => match op {
                    Operator::Add => {
                        if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                            self.visit_add_exp(add_exp, visit_type.clone()),
                            self.visit_mul_exp(mul_exp, visit_type.clone()),
                        ) {
                            // check if both lval and rval are const
                            let l_val_data = imvaluedata!(self, function, l_value);
                            let r_val_data = imvaluedata!(self, function, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value =
                                    value!(self, function, integer, l_num.value() + r_num.value());
                                VisitRetType::Value(number_value)
                            } else {
                                let add =
                                    value!(self, function, binary, BinaryOp::Add, l_value, r_value);
                                insertvalue!(self, function, bb, [add]);
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
                            // check if both lval and rval are const
                            let l_val_data = imvaluedata!(self, function, l_value);
                            let r_val_data = imvaluedata!(self, function, r_value);
                            if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                (l_val_data.kind(), r_val_data.kind())
                            {
                                let number_value =
                                    value!(self, function, integer, l_num.value() - r_num.value());
                                VisitRetType::Value(number_value)
                            } else {
                                let sub =
                                    value!(self, function, binary, BinaryOp::Sub, l_value, r_value);
                                insertvalue!(self, function, bb, [sub]);
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
            VisitType::Local(function, bb, _, _) => {
                match mul_exp {
                    MulExp::UnaryExp(unary_exp) => self.visit_unary_exp(unary_exp, visit_type),
                    MulExp::BinaryOp(mul_exp, op, unary_exp) => match op {
                        Operator::Multiply => {
                            if let (VisitRetType::Value(l_value), VisitRetType::Value(r_value)) = (
                                self.visit_mul_exp(mul_exp, visit_type.clone()),
                                self.visit_unary_exp(unary_exp, visit_type.clone()),
                            ) {
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        l_num.value() * r_num.value()
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mul = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Mul,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [mul]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        l_num.value() / r_num.value()
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let div = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Div,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [div]);
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
                                // check if both lval and rval are const
                                let l_val_data = imvaluedata!(self, function, l_value);
                                let r_val_data = imvaluedata!(self, function, r_value);
                                if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                                    (l_val_data.kind(), r_val_data.kind())
                                {
                                    let number_value = value!(
                                        self,
                                        function,
                                        integer,
                                        l_num.value() % r_num.value()
                                    );
                                    //removevalue!(func_data, l_value);
                                    //removevalue!(func_data, r_value);
                                    VisitRetType::Value(number_value)
                                } else {
                                    let mod_value = value!(
                                        self,
                                        function,
                                        binary,
                                        BinaryOp::Mod,
                                        l_value,
                                        r_value
                                    );
                                    insertvalue!(self, function, bb, [mod_value]);
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
            VisitType::Local(function, bb, _, _) => {
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
                                let value_data = imvaluedata!(self, function, value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer) => {
                                        let number_value =
                                            value!(self, function, integer, -integer.value());
                                        //removevalue!(func_data, value);
                                        VisitRetType::Value(number_value)
                                    }
                                    _ => {
                                        let zero_value = value!(self, function, integer, 0);
                                        let sub = value!(
                                            self,
                                            function,
                                            binary,
                                            BinaryOp::Sub,
                                            zero_value,
                                            value
                                        );
                                        insertvalue!(self, function, bb, [sub]);
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
                                let value_data = imvaluedata!(self, function, value);
                                match value_data.kind() {
                                    ValueKind::Integer(integer) => {
                                        let number_value = value!(
                                            self,
                                            function,
                                            integer,
                                            (integer.value() == 0) as i32
                                        );
                                        //removevalue!(func_data, value);
                                        VisitRetType::Value(number_value)
                                    }
                                    _ => {
                                        let zero_value = value!(self, function, integer, 0);
                                        let eq = value!(
                                            self,
                                            function,
                                            binary,
                                            BinaryOp::Eq,
                                            zero_value,
                                            value
                                        );
                                        insertvalue!(self, function, bb, [eq]);
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
            VisitType::Local(function, _, _, _) => match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, visit_type),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        let number_value = value!(self, function, integer, *int_const);
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
            bb_number: HashMap::new(),
            bb_flow_change: HashMap::new(),
            return_value: HashMap::new(),
            return_in_entry: HashMap::new(),
        }
    }

    pub fn get_bb_number(&mut self, function: Function) -> String {
        self.bb_number
            .insert(function, *self.bb_number.get(&function).unwrap() + 1);
        format!("%bb{}", *self.bb_number.get(&function).unwrap() - 1)
    }

    pub fn parse(&mut self, source_code: &String) -> Result<&Program, Box<dyn Error>> {
        let ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
        //println!("{:#?}", ast);
        self.visit_comp_unit(&ast);
        self.add_end_jump_to_bb();
        self.remove_unused_bb();
        // add jmp to return
        self.add_jmp_to_return();
        Ok(&self.program)
    }

    // instead of generating a ret directly, we choose to store the value in virtual reg %0 and jump to end.
    fn add_jmp_to_return(&mut self) {
        let mut func_vec: Vec<Function> = vec![];
        for (function, _) in self.program.funcs() {
            func_vec.push(*function);
        }
        for function in func_vec {
            if matches!(self.return_in_entry.get(&function), None) {
                let end_bb = basicblock!(self, function, Some("%end_bb".to_string()));
                insertbb!(self, function, [end_bb]);
                if let Some((_, value)) = self.return_value.get(&function) {
                    let load_value = value!(self, function, load, *value);
                    let ret_value = value!(self, function, ret, Some(load_value));

                    insertvalueforce!(self, function, end_bb, [load_value, ret_value]);
                    let mut bbs: Vec<BasicBlock> = vec![];
                    for (bb, _) in imfuncdata!(self, function).layout().bbs() {
                        if let Some(BBFlowStatus::PushDisable(Some(_func))) =
                            self.bb_flow_change.get(&bb)
                        {
                            bbs.push(*bb);
                        }
                    }
                    for bb in bbs {
                        let jmp_value = value!(self, function, jump, end_bb);
                        insertvalueforce!(self, function, bb, [jmp_value]);
                    }
                } else {
                    panic!();
                }
            }
        }
    }

    fn remove_unused_bb(&mut self) {
        loop {
            let mut all_bbs: Vec<(Function, BasicBlock)> = vec![];
            for (function, func_data) in self.program.funcs() {
                for (bb, _bb_data) in func_data.dfg().bbs() {
                    all_bbs.push((*function, *bb));
                }
            }
            let all_bbs_num = all_bbs.len();
            for (func, bb) in &all_bbs {
                let bb_data = imbbdata!(self, *func, *bb);
                if bb_data.used_by().is_empty() && bb_data.name().clone().unwrap() != "%entry" {
                    // remove all values in the bb
                    let mut all_values: Vec<Value> = vec![];
                    for (bb_now, bb_node) in imfuncdata!(self, *func).layout().bbs() {
                        if bb_now == bb {
                            for (value, _) in bb_node.insts() {
                                if imvaluedata!(self, *func, *value).used_by().is_empty() {
                                    all_values.push(*value);
                                }
                            }
                        }
                    }
                    for value in all_values {
                        removevalue!(self, *func, value);
                    }
                    self.program
                        .func_mut(*func)
                        .layout_mut()
                        .bbs_mut()
                        .remove(&bb);
                    self.program.func_mut(*func).dfg_mut().remove_bb(*bb);
                }
            }
            if all_bbs.len() == all_bbs_num {
                break;
            }
        }
    }

    // every end bb should jump to last if's end bb...
    fn add_end_jump_to_bb(&mut self) {
        for (bb, bb_status) in &self.bb_flow_change {
            if let BBFlowStatus::PushEnable(ori_bb) = bb_status {
                if let Some((function, ori_bb)) = ori_bb {
                    let jump_value = value!(self, *function, jump, *ori_bb);
                    insertvalue!(self, *function, *bb, [jump_value]);
                }
            }
        }
    }

    pub fn gen_ir(&self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
