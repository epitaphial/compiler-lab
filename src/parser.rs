use core::panic;
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
        BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind,
    },
};

use crate::ast::{
    self, AddExp, BlockItem, EqExp, LAndExp, LOrExp, MulExp, Number, Operator, PrimaryExp, RelExp,
    Stmt, UnaryExp,
};

lalrpop_mod!(pub sysy);

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
enum BBStatus {
    Pushable,         // Current bb is pushable
    Branched, // Current bb already branched or jumped or returned, and can not push more value
    JumpToLastNested, // Current bb is pushable, but can only push value before the jump value. In the end of an "end branch" bb of if statement or whilestatement, we need to jump back to last if's "end branch" basicblock or last while's "while entry branch" basicblock, we need this(ugly design...), but if current end bb already has an branch value(ret br jump...), we dont need this field.
}

pub struct Parser {
    program: Program,
    bb_number: HashMap<Function, u32>, // the basic block number which start from 0
    bbs_status: HashMap<BasicBlock, BBStatus>, // the status of basicblock
    end_bbs: HashMap<Function, (BasicBlock, Value, bool)>, // The hashmap's value is the end basic block and return value of the key function, exists only has multi return statement.The bool stand for there is already has alloc return value.
}

#[derive(Debug, Clone, Default)]
struct Scope {
    is_global: bool,                                // is in global scope?
    function: Option<Function>,                     // current function
    basic_block: Option<BasicBlock>,                // current basicblock
    block_node: Rc<BlockNode>,                      // current blocknode, to determine the scope
    last_nested_bb: Option<BasicBlock>, // 1."end" basic block of last ifstatement, when in a if or while statement's end bb, we need to jump to last ifstatement's end bb. For example: if(a)if(b); 2."while entry" basic block of last while statement, when in a if or while statement's end bb, we need to jump to last whilestatement's while entry bb.
    last_end_exp_bb: Option<BasicBlock>, // "end_exp" basic block of logic exp, when in a multi-layer logic expression bb, we need to jump to last exp's end_exp bb. For example: a||b&&c
    curr_loop_bb: Option<(BasicBlock, BasicBlock)>, // current loop's while entry and while end, used for build continue and break.
}

impl Scope {
    fn new(is_global: bool) -> Self {
        Scope {
            is_global,
            ..Self::default()
        }
    }
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
    ($self:ident,$func:expr,$bbname:expr) => {
        funcdata!($self, $func)
            .dfg_mut()
            .new_bb()
            .basic_block($bbname)
    };
}

macro_rules! isentrybb {
    ($self:ident,$func:expr,$bb:expr) => {
        imfuncdata!($self, $func).layout().entry_bb() == Some($bb)
    };
}

macro_rules! entrybb {
    ($self:ident,$func:expr) => {
        imfuncdata!($self, $func).layout().entry_bb()
    };
}

macro_rules! insertbb {
    ($self:ident,$func:expr,$bb:expr) => {
        funcdata!($self, $func).layout_mut().bbs_mut().extend($bb);
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
        match $self.bbs_status.get(&$bb).unwrap() {
            BBStatus::Pushable => funcdata!($self, $func)
                .layout_mut()
                .bb_mut($bb)
                .insts_mut()
                .extend($values),
            BBStatus::JumpToLastNested => {
                for value in $values {
                    let mut has_branch = false;
                    if matches!(imvaluedata!($self, $func, value).kind(), ValueKind::Jump(_))
                        || matches!(
                            imvaluedata!($self, $func, value).kind(),
                            ValueKind::Branch(_)
                        )
                        || matches!(
                            imvaluedata!($self, $func, value).kind(),
                            ValueKind::Return(_)
                        )
                    {
                        has_branch = true;
                    }
                    let jmp_value = funcdata!($self, $func)
                        .layout_mut()
                        .bb_mut($bb)
                        .insts_mut()
                        .pop_back()
                        .unwrap()
                        .0;
                    funcdata!($self, $func)
                        .layout_mut()
                        .bb_mut($bb)
                        .insts_mut()
                        .extend($values);
                    if !has_branch {
                        funcdata!($self, $func)
                            .layout_mut()
                            .bb_mut($bb)
                            .insts_mut()
                            .extend([jmp_value]);
                    } else {
                        removevalue!($self, $func, jmp_value);
                    }
                }
            }
            _ => {}
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

impl Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit) {
        match comp_unit {
            ast::CompUnit::Decl(comp_unit, decl) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit);
                }
                self.visit_decl(decl, &mut Scope::new(true));
            }
            ast::CompUnit::FuncDef(comp_unit, func_def) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit);
                }
                self.visit_func_def(func_def);
            }
        }
    }

    fn visit_decl(&mut self, decl: &ast::Decl, scope: &mut Scope) {
        match decl {
            ast::Decl::ConstDecl(const_decl) => {
                self.visit_const_decl(const_decl, scope);
            }
            ast::Decl::VarDecl(var_decl) => {
                self.visit_var_decl(var_decl, scope);
            }
        }
    }

    // global decl's lvalue must be a const
    fn visit_const_decl(&mut self, const_decl: &ast::ConstDecl, scope: &mut Scope) {
        // do some type check here...?
        for const_def in &const_decl.const_defs {
            self.visit_const_def(const_def, scope);
        }
    }

    fn visit_const_def(&mut self, const_def: &ast::ConstDef, scope: &mut Scope) {
        if scope.is_global {
            unimplemented!()
        } else {
            if const_def.const_exps.len() == 0 {
                // define a const value
                let value = self.visit_const_init_val(&const_def.const_init_val, scope);
                scope
                    .block_node
                    .insert_symbol(const_def.const_ident.clone(), value);
            } else {
                // define a const array
                unimplemented!()
            }
        }
    }

    fn visit_const_init_val(
        &mut self,
        cons_init_val: &ast::ConstInitVal,
        scope: &mut Scope,
    ) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match cons_init_val {
                ast::ConstInitVal::ConstExp(const_exp) => self.visit_const_exp(const_exp, scope),
                ast::ConstInitVal::ConstInitVal(_const_init_val) => {
                    // const array init values...
                    unimplemented!()
                }
            }
        }
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl, scope: &mut Scope) {
        // do some type check here...?
        for var_def in &var_decl.var_defs {
            self.visit_var_def(var_def, scope);
        }
    }

    fn visit_var_def(&mut self, var_def: &ast::VarDef, scope: &mut Scope) {
        if scope.is_global {
            unimplemented!()
        } else {
            match var_def {
                // with no initial vals
                ast::VarDef::VarDef(var_ident, const_exps) => {
                    if const_exps.len() == 0 {
                        let alloc = value!(self, scope.function.unwrap(), alloc, Type::get_i32());
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc]
                        );
                        scope.block_node.insert_symbol(var_ident.clone(), alloc);
                    } else {
                        // arrray
                        unimplemented!()
                    }
                }
                // with initial vals...
                ast::VarDef::InitVal(var_ident, const_exps, init_val) => {
                    if const_exps.len() == 0 {
                        let value = self.visit_init_val(init_val, scope);
                        let alloc = value!(self, scope.function.unwrap(), alloc, Type::get_i32());
                        let store = value!(self, scope.function.unwrap(), store, value, alloc);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc, store]
                        );
                        scope.block_node.insert_symbol(var_ident.clone(), alloc);
                    } else {
                        // array
                        unimplemented!()
                    }
                }
            }
        }
    }

    fn visit_init_val(&mut self, init_val: &ast::InitVal, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match init_val {
                ast::InitVal::Exp(exp) => self.visit_exp(exp, scope),
                ast::InitVal::InitVal(_init_val) => unimplemented!(),
            }
        }
    }

    fn visit_func_def(&mut self, func_def: &crate::ast::FuncDef) {
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

        self.bb_number.insert(function, 0); // init bb number for current function

        // new entry bb for current function
        let entry_bb = basicblock!(self, function, Some("%entry".into()));
        self.bbs_status.insert(entry_bb, BBStatus::Pushable);
        insertbb!(self, function, [entry_bb]);

        // If current function has return value
        if !matches!(func_def.func_type, ast::Type::Void) {
            // alloc value to store return value
            let alloc = value!(self, function, alloc, Type::get_i32());
            //insertvalue!(self, function, entry_bb, [alloc]);

            // add an end block to load return value and return
            let end_bb = basicblock!(self, function, Some("%end_bb".into()));
            self.bbs_status.insert(end_bb, BBStatus::Pushable);
            insertbb!(self, function, [end_bb]);

            let load = value!(self, function, load, alloc);
            let ret = value!(self, function, ret, Some(load));
            insertvalue!(self, function, end_bb, [load, ret]);
            self.end_bbs.insert(function, (end_bb, alloc, false));
        }

        let block_node = BlockNode::new(); // maybe consider param symbols...

        let mut scope = Scope {
            is_global: false,
            function: Some(function),
            basic_block: Some(entry_bb),
            block_node: block_node,
            last_nested_bb: None,
            last_end_exp_bb: None,
            curr_loop_bb: None,
        };
        self.visit_block(&func_def.block, &mut scope);
    }

    fn visit_block(&mut self, block: &crate::ast::Block, scope: &mut Scope) {
        if scope.is_global {
            panic!("Block can not in a global scope")
        } else {
            for block_item in &block.block_items {
                self.visit_block_item(block_item, scope);
            }
        }
    }

    fn visit_block_item(&mut self, block_item: &crate::ast::BlockItem, scope: &mut Scope) {
        if scope.is_global {
            panic!("BlockItem can not in a global scope")
        } else {
            match block_item {
                BlockItem::Stmt(stmt) => self.visit_stmt(stmt, scope),
                BlockItem::Decl(decl) => self.visit_decl(decl, scope),
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt, scope: &mut Scope) {
        if scope.is_global {
            panic!("Statement can not in a global scope")
        } else {
            match stmt {
                Stmt::RetStmt(ret_stmt) => self.visit_ret_stmt(ret_stmt, scope),
                Stmt::AssignStmt(ass_stmt) => self.visit_ass_stmt(ass_stmt, scope),
                Stmt::ExpStmt(exp_stmt) => self.visit_exp_stmt(exp_stmt, scope),
                Stmt::BlockStmt(block_stmt) => self.visit_block_stmt(block_stmt, scope),
                Stmt::IfStmt(if_stmt) => self.visit_if_stmt(if_stmt, scope),
                Stmt::WhileStmt(while_stmt) => self.visit_while_stmt(while_stmt, scope),
                Stmt::BreakStmt => self.visit_break_stmt(scope),
                Stmt::ContinueStmt => self.visit_continue_stmt(scope),
            }
        }
    }

    fn visit_break_stmt(&mut self, scope: &mut Scope) {
        if scope.is_global {
            panic!("Break statement can not in global scope!")
        } else {
            if let Some((_, while_end_bb)) = scope.curr_loop_bb {
                // jump to while end if encounter a break stmt
                let jump_value = value!(self, scope.function.unwrap(), jump, while_end_bb);
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [jump_value]
                );
                self.bbs_status
                    .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                let break_end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let break_end_bb =
                    basicblock!(self, scope.function.unwrap(), Some(break_end_bb_label));
                self.bbs_status.insert(break_end_bb, BBStatus::Pushable);
                insertbb!(self, scope.function.unwrap(), [break_end_bb]);
                scope.basic_block = Some(break_end_bb);
            } else {
                panic!("Break statement must be in a loop!")
            }
        }
    }

    fn visit_continue_stmt(&mut self, scope: &mut Scope) {
        if scope.is_global {
            panic!("Continue statement can not in global scope!")
        } else {
            if let Some((while_entry_bb, _)) = scope.curr_loop_bb {
                // jump to while entry if encounter a continue stmt
                let jump_value = value!(self, scope.function.unwrap(), jump, while_entry_bb);
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [jump_value]
                );
                self.bbs_status
                    .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                let continue_end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let continue_end_bb =
                    basicblock!(self, scope.function.unwrap(), Some(continue_end_bb_label));
                self.bbs_status.insert(continue_end_bb, BBStatus::Pushable);
                insertbb!(self, scope.function.unwrap(), [continue_end_bb]);
                scope.basic_block = Some(continue_end_bb);
            } else {
                panic!("Continue statement must be in a loop!")
            }
        }
    }

    fn visit_while_stmt(&mut self, while_stmt: &ast::WhileStmt, scope: &mut Scope) {
        if scope.is_global {
            panic!("While statement can not in a global scope")
        } else {
            let cur_last_nested_bb = scope.last_nested_bb;
            let cur_loop_bb = scope.curr_loop_bb;

            // make while entry basicblock, while body basicblock, while end basicblock
            let while_entry_bb_label = self.get_new_bb_number(scope.function.unwrap());
            let while_entry_bb =
                basicblock!(self, scope.function.unwrap(), Some(while_entry_bb_label));
            self.bbs_status.insert(while_entry_bb, BBStatus::Pushable);

            // do not label while body and while end, because while entry may generate many bbs..
            let while_body = basicblock!(self, scope.function.unwrap(), None);
            self.bbs_status.insert(while_body, BBStatus::Pushable);
            let while_end = basicblock!(self, scope.function.unwrap(), None);
            self.bbs_status.insert(while_end, BBStatus::Pushable);

            // insert while entry first
            insertbb!(self, scope.function.unwrap(), [while_entry_bb]);

            // jump to while_entry bb
            let jump_value = value!(self, scope.function.unwrap(), jump, while_entry_bb);
            insertvalue!(
                self,
                scope.function.unwrap(),
                scope.basic_block.unwrap(),
                [jump_value]
            );
            self.bbs_status
                .insert(scope.basic_block.unwrap(), BBStatus::Branched);

            // deal with while entry
            scope.basic_block = Some(while_entry_bb);
            scope.last_nested_bb = Some(while_entry_bb);
            let cond_value = self.visit_exp(&while_stmt.cond_exp, scope);

            // set while body bb name
            let while_body_label = self.get_new_bb_number(scope.function.unwrap());
            bbdata!(self, scope.function.unwrap(), while_body).set_name(Some(while_body_label));

            // insert while body and add a branch in while entry
            insertbb!(self, scope.function.unwrap(), [while_body]);
            let br_value = value!(
                self,
                scope.function.unwrap(),
                branch,
                cond_value,
                while_body,
                while_end
            );
            insertvalue!(
                self,
                scope.function.unwrap(),
                scope.basic_block.unwrap(),
                [br_value]
            );
            self.bbs_status
                .insert(scope.basic_block.unwrap(), BBStatus::Branched);

            // visit while body
            scope.basic_block = Some(while_body);
            scope.curr_loop_bb = Some((while_entry_bb, while_end));
            self.visit_stmt(&while_stmt.loop_stmt, scope);
            scope.curr_loop_bb = cur_loop_bb;

            // from while body jump to while entry
            let jump_value = value!(self, scope.function.unwrap(), jump, while_entry_bb);
            insertvalue!(
                self,
                scope.function.unwrap(),
                scope.basic_block.unwrap(),
                [jump_value]
            );
            self.bbs_status
                .insert(scope.basic_block.unwrap(), BBStatus::Branched);

            // set while end bb name and insert while end bb
            let while_end_label = self.get_new_bb_number(scope.function.unwrap());
            bbdata!(self, scope.function.unwrap(), while_end).set_name(Some(while_end_label));
            insertbb!(self, scope.function.unwrap(), [while_end]);

            scope.last_nested_bb = cur_last_nested_bb;

            if let Some(last_nest_bb) = scope.last_nested_bb {
                // insert while end bb and add a jump to last nested construction
                let jump_value = value!(self, scope.function.unwrap(), jump, last_nest_bb);
                insertvalue!(self, scope.function.unwrap(), while_end, [jump_value]);
                self.bbs_status
                    .insert(while_end, BBStatus::JumpToLastNested);
            }
            scope.basic_block = Some(while_end);
        }
    }

    fn visit_if_stmt(&mut self, if_stmt: &ast::IfStmt, scope: &mut Scope) {
        // 1. deal with exp; 2. new bb for then; 3. new bb for else; 4. new bb for end; 5. add jmp from then and else to end, and dont forget to change folowing block item's bb, and jump back to last if's bb.
        if scope.is_global {
            panic!("If statement can not in a global scope")
        } else {
            let cur_last_nested_bb = scope.last_nested_bb;
            let exp_value = self.visit_exp(&if_stmt.cond_exp, scope);

            if let Some(else_stmt) = &if_stmt.else_stmt {
                // with else branch

                // check exp_value first, if exp_value is a integer, we do not need to generate brach
                if let ValueKind::Integer(int) =
                    imvaluedata!(self, scope.function.unwrap(), exp_value).kind()
                {
                    if int.value() != 0 {
                        self.visit_stmt(&if_stmt.then_stmt, scope);
                        return;
                    } else {
                        self.visit_stmt(else_stmt, scope);
                        return;
                    }
                }

                // new basic block for branch "then"
                let then_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let then_bb = basicblock!(self, scope.function.unwrap(), Some(then_bb_label));
                self.bbs_status.insert(then_bb, BBStatus::Pushable);

                // new basic block for branch "else"
                let else_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let else_bb = basicblock!(self, scope.function.unwrap(), Some(else_bb_label));
                self.bbs_status.insert(else_bb, BBStatus::Pushable);

                // new basic block for branch "end"
                let end_bb = basicblock!(self, scope.function.unwrap(), None);
                self.bbs_status.insert(end_bb, BBStatus::Pushable);

                insertbb!(self, scope.function.unwrap(), [then_bb, else_bb]);

                // new a branch value from current bb to "then" and "else" branch
                let br_value = value!(
                    self,
                    scope.function.unwrap(),
                    branch,
                    exp_value,
                    then_bb,
                    else_bb
                );
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [br_value]
                );
                self.bbs_status
                    .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                // change scope of then and else statement
                scope.last_nested_bb = Some(end_bb);

                // visit then statement
                scope.basic_block = Some(then_bb); // change bb of scope
                self.visit_stmt(&if_stmt.then_stmt, scope);

                // visit else statement
                scope.basic_block = Some(else_bb); // change bb of scope
                self.visit_stmt(else_stmt, scope);

                scope.last_nested_bb = cur_last_nested_bb;

                // after visit "then" and "else" statement, we need to insert a jump from them to "end" bb, unless they alreay have an out value(ret, br, jump...).
                let then_jmp_value = value!(self, scope.function.unwrap(), jump, end_bb);
                insertvalue!(self, scope.function.unwrap(), then_bb, [then_jmp_value]);
                self.bbs_status.insert(then_bb, BBStatus::Branched);

                let else_jmp_value = value!(self, scope.function.unwrap(), jump, end_bb);
                insertvalue!(self, scope.function.unwrap(), else_bb, [else_jmp_value]);
                self.bbs_status.insert(else_bb, BBStatus::Branched);

                // To ensure the end bb is in the right order, we have to set its name and insert it to current function after visit "then" and "else" bb.
                let end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let bb_data = bbdata!(self, scope.function.unwrap(), end_bb);
                bb_data.set_name(Some(end_bb_label));
                insertbb!(self, scope.function.unwrap(), [end_bb]);

                // Insert a jump value from current if's end branch to last if's end branch
                if let Some(last_nested_bb) = scope.last_nested_bb {
                    let jmp_value = value!(self, scope.function.unwrap(), jump, last_nested_bb);
                    insertvalue!(self, scope.function.unwrap(), end_bb, [jmp_value]);
                    self.bbs_status.insert(end_bb, BBStatus::JumpToLastNested);
                }

                // after this if statement, the current bb of scope is changed to "end"
                scope.basic_block = Some(end_bb);
            } else {
                // without else branch

                // check exp_value first, if exp_value is a integer, we do not need to generate brach
                if let ValueKind::Integer(int) =
                    imvaluedata!(self, scope.function.unwrap(), exp_value).kind()
                {
                    if int.value() != 0 {
                        self.visit_stmt(&if_stmt.then_stmt, scope);
                        return;
                    } else {
                        return;
                    }
                }

                // new basic block for branch "then"
                let then_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let then_bb = basicblock!(self, scope.function.unwrap(), Some(then_bb_label));
                self.bbs_status.insert(then_bb, BBStatus::Pushable);

                // new basic block for branch "end"
                let end_bb = basicblock!(self, scope.function.unwrap(), None);
                self.bbs_status.insert(end_bb, BBStatus::Pushable);

                insertbb!(self, scope.function.unwrap(), [then_bb]);

                // new a branch value from current bb to "then" and "end" branch
                let br_value = value!(
                    self,
                    scope.function.unwrap(),
                    branch,
                    exp_value,
                    then_bb,
                    end_bb
                );
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [br_value]
                );
                self.bbs_status
                    .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                // change scope of then statement
                scope.last_nested_bb = Some(end_bb);

                // visit then statement
                scope.basic_block = Some(then_bb); // change bb of scope
                self.visit_stmt(&if_stmt.then_stmt, scope);

                scope.last_nested_bb = cur_last_nested_bb;

                // after visit "then" statement, we need to insert a jump from it to "end" bb, unless they alreay have an out value(ret, br, jump...).
                let then_jmp_value = value!(self, scope.function.unwrap(), jump, end_bb);
                insertvalue!(self, scope.function.unwrap(), then_bb, [then_jmp_value]);
                self.bbs_status.insert(then_bb, BBStatus::Branched);

                // To ensure the end bb is in the right order, we have to set its name and insert it to current function after visit "then" and "else" bb.
                let end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                let bb_data = bbdata!(self, scope.function.unwrap(), end_bb);
                bb_data.set_name(Some(end_bb_label));
                insertbb!(self, scope.function.unwrap(), [end_bb]);

                // Insert a jump value from current if's end branch to last if's end branch
                if let Some(last_nested_bb) = scope.last_nested_bb {
                    let jmp_value = value!(self, scope.function.unwrap(), jump, last_nested_bb);
                    insertvalue!(self, scope.function.unwrap(), end_bb, [jmp_value]);
                    self.bbs_status.insert(end_bb, BBStatus::JumpToLastNested);
                }

                // after this if statement, the current bb of scope is changed to "end"
                scope.basic_block = Some(end_bb);
            }
        }
    }

    fn visit_block_stmt(&mut self, block_stmt: &ast::BlockStmt, scope: &mut Scope) {
        if scope.is_global {
            panic!("Block statement can not in global scope!")
        } else {
            let new_node = BlockNode::new();
            let cur_node = scope.block_node.clone();
            BlockNode::insert_block(scope.block_node.clone(), new_node.clone());
            // change the block node before visit new block
            scope.block_node = new_node.clone();
            self.visit_block(&block_stmt.block, scope);
            scope.block_node = cur_node.clone();
            // restore the block node after visit new block
        }
    }

    fn visit_ret_stmt(&mut self, ret_stmt: &crate::ast::RetStmt, scope: &mut Scope) {
        if scope.is_global {
            panic!("Return statement can not be in global scope!")
        } else {
            if let Some(exp) = &ret_stmt.exp {
                // with return value

                // visit exp of return
                let exp_value = self.visit_exp(exp, scope);

                if isentrybb!(self, scope.function.unwrap(), scope.basic_block.unwrap()) {
                    // If in entry bb
                    let ret = value!(self, scope.function.unwrap(), ret, Some(exp_value));
                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [ret]
                    );
                    self.bbs_status
                        .insert(scope.basic_block.unwrap(), BBStatus::Branched);
                } else {
                    // If not in entry bb, jump to end bb
                    let (end_bb, return_value, has_insert_alloc) =
                        self.end_bbs.get(&scope.function.unwrap()).unwrap();

                    let mut inserted = false;

                    // check if already insert alloc value
                    if !*has_insert_alloc {
                        // insert alloc to entry bb
                        let entry_bb = entrybb!(self, scope.function.unwrap());
                        funcdata!(self, scope.function.unwrap())
                            .layout_mut()
                            .bb_mut(entry_bb.unwrap())
                            .insts_mut()
                            .push_key_front(*return_value)
                            .expect("Can not push alloc!");
                        inserted = true;
                    }

                    let store = value!(
                        self,
                        scope.function.unwrap(),
                        store,
                        exp_value,
                        *return_value
                    );
                    let jmp_value = value!(self, scope.function.unwrap(), jump, *end_bb);

                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [store, jmp_value]
                    );
                    self.bbs_status
                        .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                    if inserted {
                        self.end_bbs
                            .insert(scope.function.unwrap(), (*end_bb, *return_value, true));
                    }
                }
            } else {
                // void function without return value
                unimplemented!();
                // let func_data = funcdata!(self, function);
                // let ret = value!(func_data, ret, None);
                // insertvalue!(self, func_data, bb, [ret]);
            }
        }
    }

    fn visit_exp_stmt(&mut self, exp_stmt: &ast::ExpStmt, scope: &mut Scope) {
        if scope.is_global {
            panic!("Exp statement can not in global scope!");
        } else {
            if let Some(exp) = &exp_stmt.exp {
                self.visit_exp(exp, scope);
            }
        }
    }

    fn visit_ass_stmt(&mut self, ass_stmt: &ast::AssignStmt, scope: &mut Scope) {
        if scope.is_global {
            panic!()
        } else {
            let value = self.visit_exp(&ass_stmt.exp, scope);
            let ass_l_value = scope.block_node.lookup_symbol(&ass_stmt.l_val.ident);
            // must do some type check, because we can't assign to a const!
            let store = value!(self, scope.function.unwrap(), store, value, ass_l_value);
            insertvalue!(
                self,
                scope.function.unwrap(),
                scope.basic_block.unwrap(),
                [store]
            );
        }
    }

    fn visit_l_val(&mut self, l_val: &ast::LVal, scope: &Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            // look up symbol table...
            let value = scope.block_node.lookup_symbol(&l_val.ident);
            let value_data = imvaluedata!(self, scope.function.unwrap(), value);

            if matches!(value_data.kind(), ValueKind::Alloc(_)) {
                // Need to load first for alloc
                let load = value!(self, scope.function.unwrap(), load, value);
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [load]
                );
                load
            } else {
                value
            }
        }
    }

    fn visit_const_exp(&mut self, const_exp: &ast::ConstExp, scope: &mut Scope) -> Value {
        // should check if it is a const...
        if scope.is_global {
            unimplemented!()
        } else {
            let const_value = self.visit_exp(&const_exp.exp, scope);
            let val_data = imvaluedata!(self, scope.function.unwrap(), const_value);
            if let ValueKind::Integer(_) = val_data.kind() {
                const_value
            } else {
                panic!(
                    "Const expression's rvalue must be a constant, but got: {:#?}",
                    val_data.kind()
                )
            }
        }
    }

    fn visit_exp(&mut self, exp: &crate::ast::Exp, scope: &mut Scope) -> Value {
        self.visit_l_or_exp(&exp.l_or_exp, scope)
    }

    fn visit_l_or_exp(&mut self, l_or_exp: &ast::LOrExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match l_or_exp {
                LOrExp::LAndExp(l_and_exp) => self.visit_l_and_exp(l_and_exp, scope),
                LOrExp::BinaryOp(l_or_exp, l_and_exp) => {
                    let l_value = self.visit_l_or_exp(l_or_exp, scope);
                    let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);

                    if let ValueKind::Integer(int) = l_val_data.kind() {
                        if int.value() != 0 {
                            let int_value = value!(self, scope.function.unwrap(), integer, 1);
                            return int_value;
                        } else {
                            let r_value = self.visit_l_and_exp(l_and_exp, scope);
                            let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                            if let ValueKind::Integer(int) = r_val_data.kind() {
                                if int.value() != 0 {
                                    let int_value =
                                        value!(self, scope.function.unwrap(), integer, 1);
                                    return int_value;
                                } else {
                                    return r_value;
                                }
                            } else {
                                let false_value = value!(self, scope.function.unwrap(), integer, 0);
                                let ne = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::NotEq,
                                    r_value,
                                    false_value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [ne]
                                );
                                return ne;
                            }
                        }
                    } else {
                        let ori_last_end_exp_bb = scope.last_end_exp_bb;
                        // Generate if statement to check if l_value is true, if so, directly return true, else check r_value.

                        // Alloc a Value to store the l_or_exp's value
                        let alloc_value =
                            value!(self, scope.function.unwrap(), alloc, Type::get_i32());
                        let true_value = value!(self, scope.function.unwrap(), integer, 1);
                        let store_value = value!(
                            self,
                            scope.function.unwrap(),
                            store,
                            true_value,
                            alloc_value
                        );

                        // Compare the l_value to "false"
                        let false_value = value!(self, scope.function.unwrap(), integer, 0);
                        let cmp_l_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::Eq,
                            l_value,
                            false_value
                        );

                        // if l_value is not true, jump to r_value_bb to check if r_value is true
                        let r_value_bb_label = self.get_new_bb_number(scope.function.unwrap());
                        let r_value_bb =
                            basicblock!(self, scope.function.unwrap(), Some(r_value_bb_label));
                        self.bbs_status.insert(r_value_bb, BBStatus::Pushable);

                        // if l_value is true, directly jump to exp_end_bb, or jump to r_value_bb first.
                        let exp_end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                        let exp_end_bb =
                            basicblock!(self, scope.function.unwrap(), Some(exp_end_bb_label));
                        self.bbs_status.insert(exp_end_bb, BBStatus::Pushable);

                        insertbb!(self, scope.function.unwrap(), [r_value_bb, exp_end_bb]);

                        let br_l_value = value!(
                            self,
                            scope.function.unwrap(),
                            branch,
                            cmp_l_value,
                            r_value_bb,
                            exp_end_bb
                        );
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc_value, store_value, cmp_l_value, br_l_value]
                        );
                        self.bbs_status
                            .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                        // Visit r_value
                        scope.basic_block = Some(r_value_bb);
                        scope.last_end_exp_bb = Some(exp_end_bb);
                        let r_value = self.visit_l_and_exp(l_and_exp, scope);
                        scope.last_end_exp_bb = ori_last_end_exp_bb;

                        // Compare r_value to "false", and save it to alloc value below.
                        let cmp_r_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::NotEq,
                            r_value,
                            false_value
                        );
                        let store_value = value!(
                            self,
                            scope.function.unwrap(),
                            store,
                            cmp_r_value,
                            alloc_value
                        );
                        let jump_value = value!(self, scope.function.unwrap(), jump, exp_end_bb);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            r_value_bb,
                            [cmp_r_value, store_value, jump_value]
                        );
                        self.bbs_status.insert(r_value_bb, BBStatus::Branched);

                        let load_final_value =
                            value!(self, scope.function.unwrap(), load, alloc_value);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            exp_end_bb,
                            [load_final_value]
                        );

                        // Insert a jump value from current exp end branch to last exp end branch
                        if let Some(last_end_exp_bb) = scope.last_end_exp_bb {
                            let jmp_value =
                                value!(self, scope.function.unwrap(), jump, last_end_exp_bb);
                            insertvalue!(self, scope.function.unwrap(), exp_end_bb, [jmp_value]);
                            self.bbs_status
                                .insert(exp_end_bb, BBStatus::JumpToLastNested);
                        }

                        // Change basicblock to exp_end_bb
                        scope.basic_block = Some(exp_end_bb);
                        load_final_value
                    }
                }
            }
        }
    }

    fn visit_l_and_exp(&mut self, l_and_exp: &ast::LAndExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match l_and_exp {
                LAndExp::EqExp(eq_exp) => self.visit_eq_exp(eq_exp, scope),
                LAndExp::BinaryOp(l_and_exp, eq_exp) => {
                    let l_value = self.visit_l_and_exp(l_and_exp, scope);
                    let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                    if let ValueKind::Integer(int) = l_val_data.kind() {
                        if int.value() == 0 {
                            return l_value;
                        } else {
                            let r_value = self.visit_eq_exp(eq_exp, scope);
                            let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                            if let ValueKind::Integer(int) = r_val_data.kind() {
                                if int.value() == 0 {
                                    return r_value;
                                } else {
                                    let true_value =
                                        value!(self, scope.function.unwrap(), integer, 1);
                                    return true_value;
                                }
                            } else {
                                let false_value = value!(self, scope.function.unwrap(), integer, 0);
                                let ne = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::NotEq,
                                    r_value,
                                    false_value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [ne]
                                );
                                return ne;
                            }
                        }
                    } else {
                        let ori_last_end_exp_bb = scope.last_end_exp_bb;

                        // Generate if statement to check if l_value is false, if so, directly return false, else check r_value.

                        // Alloc a Value to store the l_and_exp's value
                        let alloc_value =
                            value!(self, scope.function.unwrap(), alloc, Type::get_i32());
                        let false_value = value!(self, scope.function.unwrap(), integer, 0);
                        let store_value = value!(
                            self,
                            scope.function.unwrap(),
                            store,
                            false_value,
                            alloc_value
                        );

                        // Compare the l_value to "false"
                        let cmp_l_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::NotEq,
                            l_value,
                            false_value
                        );

                        // if l_value is not true, jump to r_value_bb to check if r_value is true
                        let r_value_bb_label = self.get_new_bb_number(scope.function.unwrap());
                        let r_value_bb =
                            basicblock!(self, scope.function.unwrap(), Some(r_value_bb_label));
                        self.bbs_status.insert(r_value_bb, BBStatus::Pushable);

                        // if l_value is true, directly jump to exp_end_bb, or jump to r_value_bb first.
                        let exp_end_bb_label = self.get_new_bb_number(scope.function.unwrap());
                        let exp_end_bb =
                            basicblock!(self, scope.function.unwrap(), Some(exp_end_bb_label));
                        self.bbs_status.insert(exp_end_bb, BBStatus::Pushable);

                        insertbb!(self, scope.function.unwrap(), [r_value_bb, exp_end_bb]);

                        let br_l_value = value!(
                            self,
                            scope.function.unwrap(),
                            branch,
                            cmp_l_value,
                            r_value_bb,
                            exp_end_bb
                        );
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc_value, store_value, cmp_l_value, br_l_value]
                        );
                        self.bbs_status
                            .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                        // Visit r_value
                        scope.basic_block = Some(r_value_bb);
                        scope.last_end_exp_bb = Some(exp_end_bb);
                        let r_value = self.visit_eq_exp(eq_exp, scope);
                        scope.last_end_exp_bb = ori_last_end_exp_bb;
                        // Compare r_value to "true", and save it to alloc value below.
                        let cmp_r_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::NotEq,
                            r_value,
                            false_value
                        );
                        let store_value = value!(
                            self,
                            scope.function.unwrap(),
                            store,
                            cmp_r_value,
                            alloc_value
                        );
                        let jump_value = value!(self, scope.function.unwrap(), jump, exp_end_bb);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            r_value_bb,
                            [cmp_r_value, store_value, jump_value]
                        );
                        self.bbs_status.insert(r_value_bb, BBStatus::Branched);

                        let load_final_value =
                            value!(self, scope.function.unwrap(), load, alloc_value);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            exp_end_bb,
                            [load_final_value]
                        );

                        // Insert a jump value from current exp end branch to last exp end branch
                        if let Some(last_end_exp_bb) = scope.last_end_exp_bb {
                            let jmp_value =
                                value!(self, scope.function.unwrap(), jump, last_end_exp_bb);
                            insertvalue!(self, scope.function.unwrap(), exp_end_bb, [jmp_value]);
                            self.bbs_status
                                .insert(exp_end_bb, BBStatus::JumpToLastNested);
                        }

                        // Change basicblock to exp_end_bb
                        scope.basic_block = Some(exp_end_bb);
                        load_final_value
                    }
                }
            }
        }
    }

    fn visit_eq_exp(&mut self, eq_exp: &ast::EqExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match eq_exp {
                EqExp::RelExp(rel_exp) => self.visit_rel_exp(rel_exp, scope),
                EqExp::BinaryOp(eq_exp, op, rel_exp) => match op {
                    Operator::Equal => {
                        let (l_value, r_value) = (
                            self.visit_eq_exp(eq_exp, scope),
                            self.visit_rel_exp(rel_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() == r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let eq = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Eq,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [eq]
                            );
                            eq
                        }
                    }
                    Operator::NotEqual => {
                        let (l_value, r_value) = (
                            self.visit_eq_exp(eq_exp, scope),
                            self.visit_rel_exp(rel_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() != r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let not_eq = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::NotEq,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [not_eq]
                            );
                            not_eq
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        }
    }

    fn visit_rel_exp(&mut self, rel_exp: &ast::RelExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match rel_exp {
                RelExp::AddExp(add_exp) => self.visit_add_exp(add_exp, scope),
                RelExp::BinaryOp(rel_exp, op, add_exp) => match op {
                    Operator::LessThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() < r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let lt = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Lt,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [lt]
                            );
                            lt
                        }
                    }
                    Operator::MoreThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() > r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let mt = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Gt,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [mt]
                            );
                            mt
                        }
                    }
                    Operator::LessOrEqualThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() <= r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let le = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Le,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [le]
                            );
                            le
                        }
                    }
                    Operator::MoreOrEqualThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() >= r_num.value()) as i32
                            );
                            number_value
                        } else {
                            let me = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Ge,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [me]
                            );
                            me
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        }
    }

    fn visit_add_exp(&mut self, add_exp: &ast::AddExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match add_exp {
                AddExp::MulExp(mul_exp) => self.visit_mul_exp(mul_exp, scope),
                AddExp::BinaryOp(add_exp, op, mul_exp) => match op {
                    Operator::Add => {
                        let (l_value, r_value) = (
                            self.visit_add_exp(add_exp, scope),
                            self.visit_mul_exp(mul_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() + r_num.value()
                            );
                            number_value
                        } else {
                            let add = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Add,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [add]
                            );
                            add
                        }
                    }
                    Operator::Subtract => {
                        let (l_value, r_value) = (
                            self.visit_add_exp(add_exp, scope),
                            self.visit_mul_exp(mul_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() - r_num.value()
                            );
                            number_value
                        } else {
                            let sub = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Sub,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [sub]
                            );
                            sub
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        }
    }

    fn visit_mul_exp(&mut self, mul_exp: &ast::MulExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match mul_exp {
                MulExp::UnaryExp(unary_exp) => self.visit_unary_exp(unary_exp, scope),
                MulExp::BinaryOp(mul_exp, op, unary_exp) => match op {
                    Operator::Multiply => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() * r_num.value()
                            );
                            number_value
                        } else {
                            let mul = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Mul,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [mul]
                            );
                            mul
                        }
                    }
                    Operator::Divide => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() / r_num.value()
                            );
                            number_value
                        } else {
                            let div = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Div,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [div]
                            );
                            div
                        }
                    }
                    Operator::GetRemainder => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imvaluedata!(self, scope.function.unwrap(), l_value);
                        let r_val_data = imvaluedata!(self, scope.function.unwrap(), r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            let number_value = value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() % r_num.value()
                            );
                            number_value
                        } else {
                            let mod_value = value!(
                                self,
                                scope.function.unwrap(),
                                binary,
                                BinaryOp::Mod,
                                l_value,
                                r_value
                            );
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [mod_value]
                            );
                            mod_value
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        }
    }

    fn visit_unary_exp(&mut self, unary_exp: &ast::UnaryExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match unary_exp {
                UnaryExp::PrimaryExp(primary_exp) => self.visit_primary_exp(primary_exp, scope),
                UnaryExp::UnaryOp(op, unary_exp) => match op {
                    Operator::Add => self.visit_unary_exp(unary_exp, scope),
                    Operator::Subtract => {
                        let value = self.visit_unary_exp(unary_exp, scope);

                        // check if value is a const
                        let value_data = imvaluedata!(self, scope.function.unwrap(), value);
                        match value_data.kind() {
                            ValueKind::Integer(integer) => {
                                let number_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    -integer.value()
                                );
                                number_value
                            }
                            _ => {
                                let zero_value = value!(self, scope.function.unwrap(), integer, 0);
                                let sub = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::Sub,
                                    zero_value,
                                    value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [sub]
                                );
                                sub
                            }
                        }
                    }
                    Operator::Not => {
                        let value = self.visit_unary_exp(unary_exp, scope);

                        // check if value is a const
                        let value_data = imvaluedata!(self, scope.function.unwrap(), value);
                        match value_data.kind() {
                            ValueKind::Integer(integer) => {
                                let number_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    (integer.value() == 0) as i32
                                );
                                number_value
                            }
                            _ => {
                                let zero_value = value!(self, scope.function.unwrap(), integer, 0);
                                let eq = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::Eq,
                                    zero_value,
                                    value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [eq]
                                );
                                eq
                            }
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        }
    }

    fn visit_primary_exp(&mut self, primary_exp: &ast::PrimaryExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            unimplemented!()
        } else {
            match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, scope),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        let number_value =
                            value!(self, scope.function.unwrap(), integer, *int_const);
                        number_value
                    }
                },
                PrimaryExp::LVal(l_val) => self.visit_l_val(l_val, scope),
            }
        }
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            program: Program::new(),
            bb_number: HashMap::new(),
            bbs_status: HashMap::new(),
            end_bbs: HashMap::new(),
        }
    }

    pub fn get_new_bb_number(&mut self, function: Function) -> String {
        self.bb_number
            .insert(function, *self.bb_number.get(&function).unwrap() + 1);
        format!("%bb{}", *self.bb_number.get(&function).unwrap() - 1)
    }

    pub fn parse(&mut self, source_code: &String) -> Result<&Program, Box<dyn Error>> {
        let ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
        //println!("{:#?}", ast);
        self.visit_comp_unit(&ast);
        self.remove_unused_bb();
        Ok(&self.program)
    }

    fn remove_unused_bb(&mut self) {
        loop {
            let mut changed = false;

            // Remove values that not in layout and never be used.

            let mut layout_values: Vec<Value> = vec![];

            for (_func, func_data) in self.program.funcs() {
                for (_bb, bb_node) in func_data.layout().bbs() {
                    for (value, _) in bb_node.insts() {
                        layout_values.push(*value);
                    }
                }
            }

            let mut unused_values: Vec<(Function, Value)> = vec![];

            for (func, func_data) in self.program.funcs() {
                for (value, value_data) in func_data.dfg().values() {
                    //println!("{{  {:#?}  }}",value_data);
                    if value_data.used_by().is_empty() && !layout_values.contains(value) {
                        unused_values.push((*func, *value));
                        changed = true;
                    }
                }
            }

            for (func, value) in unused_values {
                removevalue!(self, func, value);
            }

            // Remove basicblock that is never used!

            let mut unused_bbs: Vec<(Function, BasicBlock)> = vec![];

            for (func, func_data) in self.program.funcs() {
                for (bb, _) in func_data.layout().bbs() {
                    if imbbdata!(self, *func, *bb).used_by().is_empty()
                        && !isentrybb!(self, *func, *bb)
                    {
                        unused_bbs.push((*func, *bb));
                        changed = true;
                    }
                }
            }

            for (func, bb) in unused_bbs {
                funcdata!(self, func).layout_mut().bbs_mut().remove(&bb);
                funcdata!(self, func).dfg_mut().remove_bb(bb);
            }

            // // combine basicblocks: if a basicblock only used by one value and this value is a jump(not branch) ,we can combine the two bb.
            // let mut dead_bbs: Vec<(Value, Function, BasicBlock)> = vec![];

            // for (func, func_data) in self.program.funcs() {
            //     for (bb, _) in func_data.layout().bbs() {
            //         let used_by_values = imbbdata!(self, *func, *bb).used_by();
            //         if used_by_values.len() == 1 {
            //             for value in used_by_values {
            //                 if matches!(
            //                     imvaluedata!(self, *func, *value).kind(),
            //                     ValueKind::Jump(_)
            //                 ) {
            //                     dead_bbs.push((*value, *func, *bb));
            //                     changed = true;
            //                 }
            //             }
            //         }
            //     }
            // }

            // // find bb of value, and combine two bbs
            // for (dead_value, dead_func, dead_bb) in dead_bbs {
            //     let mut dead_bb_values: Vec<Value> = vec![];
            //     let mut ori_bb: Option<BasicBlock> = None;
            //     let mut ori_func: Option<Function> = None;

            //     for (func, func_data) in self.program.funcs() {
            //         for (bb, bb_node) in func_data.layout().bbs() {
            //             for (value, _) in bb_node.insts() {
            //                 if *value == dead_value {
            //                     // combine dead_bb with bb
            //                     ori_bb = Some(*bb);
            //                     ori_func = Some(*func);
            //                 }
            //             }
            //         }
            //     }

            //     // pop all values of dead bb

            //     while let Some((front_value, _)) = funcdata!(self, dead_func)
            //         .layout_mut()
            //         .bb_mut(dead_bb)
            //         .insts_mut()
            //         .pop_front()
            //     {
            //         dead_bb_values.push(front_value);
            //     }

            //     for value in dead_bb_values {
            //         funcdata!(self, ori_func.unwrap())
            //             .layout_mut()
            //             .bb_mut(ori_bb.unwrap())
            //             .insts_mut()
            //             .extend([value]);
            //     }
            // }

            if !changed {
                break;
            }
        }
    }

    pub fn gen_ir(&self, output_name: &String) -> Result<(), Box<dyn Error>> {
        let mut koopa_gen = KoopaGenerator::from_path(output_name)?;
        koopa_gen.generate_on(&self.program)?;
        Ok(())
    }
}
