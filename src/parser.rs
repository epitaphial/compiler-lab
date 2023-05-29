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
        builder_traits::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
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
    pub symbols: RefCell<HashMap<String, Symbol>>,
    pub parent: RefCell<Option<Weak<BlockNode>>>,
    pub childs: RefCell<Vec<Rc<BlockNode>>>,
}

#[derive(Debug, Clone)]
enum Symbol {
    Value(Value),
    Array(Value, Vec<i32>),
    Function(Function),
}

impl BlockNode {
    pub fn new() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn insert_symbol(&self, name: String, symbol: Symbol) {
        if matches!(self.symbols.borrow().get(&name), None) {
            (*self.symbols.borrow_mut()).insert(name, symbol);
        } else {
            panic!("Redefined symbol: {}", &name);
        }
    }

    pub fn lookup_symbol(&self, name: &String) -> Symbol {
        if let Some(symbol) = self.symbols.borrow().get(name) {
            symbol.clone()
        } else {
            let mut op_parent = (*self.parent.borrow()).clone();
            while let Some(parent) = &op_parent {
                // find symbols
                if let Some(symbol) = (*parent.upgrade().unwrap()).symbols.borrow().get(name) {
                    return symbol.clone();
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

#[derive(Debug, Clone)]
enum InitListVal {
    Exp(Value),
    InitArray(Vec<InitListVal>),
}

pub struct Parser {
    program: Program,
    bb_number: HashMap<Function, u32>, // the basic block number which start from 0
    bbs_status: HashMap<BasicBlock, BBStatus>, // the status of basicblock
    end_bbs: HashMap<Function, (BasicBlock, Option<Value>, bool)>, // The hashmap's value is the end basic block and return value of the key function, exists only has multi return statement.The bool stand for there is already has alloc return value.
    has_ret_statements: Vec<Function>, // If the function has no return statements, we need to add a default return statements...
}

#[derive(Debug, Clone, Default)]
struct Scope {
    is_global: bool,                 // is in global scope?
    function: Option<Function>,      // current function
    basic_block: Option<BasicBlock>, // current basicblock
    block_node: Rc<BlockNode>,       // current blocknode, to determine the scope
    global_symbols: Rc<BlockNode>,
    last_nested_bb: Option<BasicBlock>, // 1."end" basic block of last ifstatement, when in a if or while statement's end bb, we need to jump to last ifstatement's end bb. For example: if(a)if(b); 2."while entry" basic block of last while statement, when in a if or while statement's end bb, we need to jump to last whilestatement's while entry bb.
    last_end_exp_bb: Option<BasicBlock>, // "end_exp" basic block of logic exp, when in a multi-layer logic expression bb, we need to jump to last exp's end_exp bb. For example: a||b&&c
    curr_loop_bb: Option<(BasicBlock, BasicBlock)>, // current loop's while entry and while end, used for build continue and break.
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

macro_rules! globalvalue {
    ($self:ident,$op:ident,$($value:expr),+)=>{
        $self.program.new_value().$op($($value),+)
    };
}

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

macro_rules! imglobalvaluedata {
    ($self:ident,$value:expr) => {
        $self.program.borrow_value($value).clone()
    };
}

macro_rules! removevalue {
    ($self:ident,$func:expr,$value:expr) => {
        funcdata!($self, $func).dfg_mut().remove_value($value)
    };
}

impl Parser {
    fn visit_comp_unit(&mut self, comp_unit: &ast::CompUnit, scope: &mut Scope) {
        match comp_unit {
            ast::CompUnit::Decl(comp_unit, decl) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit, scope);
                }
                self.visit_decl(decl, scope);
            }
            ast::CompUnit::FuncDef(comp_unit, func_def) => {
                if let Some(comp_unit) = comp_unit {
                    self.visit_comp_unit(comp_unit, scope);
                }
                self.visit_func_def(func_def, scope);
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

    fn visit_const_decl(&mut self, const_decl: &ast::ConstDecl, scope: &mut Scope) {
        for const_def in &const_decl.const_defs {
            self.visit_const_def(const_def, scope);
        }
    }

    fn regularization_init_list(
        &mut self,
        dim_array: Vec<i32>,
        init_array: Vec<InitListVal>,
        scope: &Scope,
    ) -> Vec<Value> {
        if dim_array.len() == 0 {
            panic!("Can not be a empty array!")
        }
        let mut edge = 0;
        let mut new_init_array: Vec<Value> = vec![];
        let mut dim_mul_all = 1;
        for dim in &dim_array {
            dim_mul_all *= dim;
        }
        for init_val in init_array {
            match init_val {
                InitListVal::InitArray(inner_list_array) => {
                    if edge % dim_array.last().unwrap() == 0 {
                        if edge == 0 {
                            let new_dim_array = dim_array[1..].to_vec();
                            let new_inner_array = self.regularization_init_list(
                                new_dim_array.clone(),
                                inner_list_array,
                                scope,
                            );
                            new_init_array.extend(new_inner_array);
                            let mut dim_mul = 1;
                            for dim in new_dim_array {
                                dim_mul *= dim;
                            }
                            edge += dim_mul;
                        } else {
                            // calc the edge
                            let mut edge_index: i32 = dim_array.len() as i32 - 1;
                            let mut dim_mul = 1;
                            while edge % dim_mul == 0 {
                                dim_mul *= dim_array[edge_index as usize];
                                edge_index -= 1;
                            }
                            if edge_index == dim_array.len() as i32 - 2 {
                                panic!("Must align to the edge!")
                            } else {
                                let new_dim_array = dim_array[(edge_index + 2) as usize..].to_vec();
                                let new_inner_array = self.regularization_init_list(
                                    new_dim_array,
                                    inner_list_array,
                                    scope,
                                );
                                new_init_array.extend(new_inner_array);
                                edge += dim_mul / dim_array[(edge_index + 1) as usize];
                            }
                        }
                    } else {
                        panic!("Must align to the edge!")
                    }
                }
                InitListVal::Exp(list_value) => {
                    new_init_array.push(list_value);
                    edge += 1;
                }
            }
        }

        // fill with zero
        for _index in 0..dim_mul_all - edge {
            let zero_value = if scope.is_global {
                globalvalue!(self, integer, 0)
            } else {
                value!(self, scope.function.unwrap(), integer, 0)
            };
            new_init_array.push(zero_value);
        }
        new_init_array
    }

    fn visit_const_def(&mut self, const_def: &ast::ConstDef, scope: &mut Scope) {
        if const_def.const_exps.len() == 0 {
            // define a const value
            if let InitListVal::Exp(value) =
                self.visit_const_init_val(&const_def.const_init_val, scope)
            {
                scope
                    .block_node
                    .insert_symbol(const_def.const_ident.clone(), Symbol::Value(value));
            } else {
                panic!("Can not be array!")
            }
        } else {
            // define a const array
            let init_array = self.visit_const_init_val(&const_def.const_init_val, scope);

            let mut dimension_array = vec![];
            for const_exp in &const_def.const_exps {
                let dim_value = self.visit_const_exp(const_exp, scope);
                let value_data = if dim_value.is_global() {
                    imglobalvaluedata!(self, dim_value)
                } else {
                    imvaluedata!(self, scope.function.unwrap(), dim_value)
                };
                if let ValueKind::Integer(dim_len) = value_data.kind() {
                    dimension_array.push(dim_len.value())
                }
            }
            if let InitListVal::InitArray(init_array) = init_array {
                let regularized_init_list =
                    self.regularization_init_list(dimension_array.clone(), init_array, scope);
                let mut array_len = 1;
                for dim in &dimension_array {
                    array_len *= dim;
                }

                if scope.is_global {
                    // global const array
                    let aggregate = globalvalue!(self, aggregate, regularized_init_list);
                    let alloc = globalvalue!(self, global_alloc, aggregate);
                    let mut alloc_name = const_def.const_ident.clone();
                    alloc_name.insert_str(0, "%global_");
                    self.program.set_value_name(alloc, Some(alloc_name.clone()));
                    scope.block_node.insert_symbol(
                        const_def.const_ident.clone(),
                        Symbol::Array(alloc, dimension_array),
                    );
                } else {
                    let aggregate = value!(
                        self,
                        scope.function.unwrap(),
                        aggregate,
                        regularized_init_list
                    );
                    let array_type = Type::get_array(Type::get_i32(), array_len as usize);
                    let alloc = value!(self, scope.function.unwrap(), alloc, array_type);
                    let store = value!(self, scope.function.unwrap(), store, aggregate, alloc);
                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [alloc, store]
                    );
                    scope.block_node.insert_symbol(
                        const_def.const_ident.clone(),
                        Symbol::Array(alloc, dimension_array),
                    );
                }
            } else {
                panic!("Must be an array!")
            }
        }
    }

    fn visit_const_init_val(
        &mut self,
        const_init_val: &ast::ConstInitVal,
        scope: &mut Scope,
    ) -> InitListVal {
        match const_init_val {
            ast::ConstInitVal::ConstExp(const_exp) => {
                InitListVal::Exp(self.visit_const_exp(const_exp, scope))
            }
            ast::ConstInitVal::ConstInitVal(const_init_val) => {
                // const array init values...
                let mut init_array = vec![];
                if let Some(vals) = const_init_val {
                    for val in vals {
                        init_array.push(self.visit_const_init_val(val, scope))
                    }
                }
                InitListVal::InitArray(init_array)
            }
        }
    }

    fn visit_var_decl(&mut self, var_decl: &ast::VarDecl, scope: &mut Scope) {
        // global decl's lvalue must be a const
        for var_def in &var_decl.var_defs {
            self.visit_var_def(var_def, scope);
        }
    }

    fn visit_var_def(&mut self, var_def: &ast::VarDef, scope: &mut Scope) {
        if scope.is_global {
            match var_def {
                // with no initial vals
                ast::VarDef::VarDef(var_ident, const_exps) => {
                    if const_exps.len() == 0 {
                        let zero_init = globalvalue!(self, zero_init, Type::get_i32());
                        let alloc = globalvalue!(self, global_alloc, zero_init);
                        let mut alloc_name = var_ident.clone();
                        alloc_name.insert_str(0, "%global_");
                        self.program.set_value_name(alloc, Some(alloc_name.clone()));
                        scope
                            .block_node
                            .insert_symbol(var_ident.clone(), Symbol::Value(alloc));
                    } else {
                        // arrray
                        let mut dim_array = vec![];
                        for const_exp in const_exps {
                            let dim = self.visit_const_exp(const_exp, scope);
                            if let ValueKind::Integer(int) = imglobalvaluedata!(self, dim).kind() {
                                dim_array.push(int.value());
                            } else {
                                panic!("Must be a const!")
                            }
                        }
                        // calc space to alloc
                        let mut array_len = 1;
                        for dim in &dim_array {
                            array_len *= dim;
                        }
                        let array_type = Type::get_array(Type::get_i32(), array_len as usize);
                        let zeroinit = globalvalue!(self, zero_init, array_type);
                        let alloc = globalvalue!(self, global_alloc, zeroinit);
                        let mut alloc_name = var_ident.clone();
                        alloc_name.insert_str(0, "%global_");
                        self.program.set_value_name(alloc, Some(alloc_name.clone()));
                        scope
                            .block_node
                            .insert_symbol(var_ident.clone(), Symbol::Array(alloc, dim_array));
                    }
                }
                // with initial vals...
                ast::VarDef::InitVal(var_ident, const_exps, init_val) => {
                    if const_exps.len() == 0 {
                        if let InitListVal::Exp(value) = self.visit_init_val(init_val, scope) {
                            let value_data = imglobalvaluedata!(self, value);
                            // make sure the initval of global value is a const
                            match value_data.kind() {
                                ValueKind::Integer(int) => {
                                    let int_val = globalvalue!(self, integer, int.value());
                                    let alloc = globalvalue!(self, global_alloc, int_val);
                                    let mut alloc_name = var_ident.clone();
                                    alloc_name.insert_str(0, "%global_");
                                    self.program.set_value_name(alloc, Some(alloc_name.clone()));
                                    scope
                                        .block_node
                                        .insert_symbol(var_ident.clone(), Symbol::Value(alloc));
                                }
                                _ => {
                                    panic!("Initval of global value must be a const!")
                                }
                            }
                        }
                    } else {
                        // arrray
                        let mut dim_array = vec![];
                        for const_exp in const_exps {
                            let dim = self.visit_const_exp(const_exp, scope);
                            if let ValueKind::Integer(int) = imglobalvaluedata!(self, dim).kind() {
                                dim_array.push(int.value());
                            } else {
                                panic!("Must be a const!")
                            }
                        }
                        // calc space to alloc
                        let mut array_len = 1;
                        for dim in &dim_array {
                            array_len *= dim;
                        }
                        if let InitListVal::InitArray(init_array) =
                            self.visit_init_val(init_val, scope)
                        {
                            let regularized_init_val =
                                self.regularization_init_list(dim_array.clone(), init_array, scope);
                            let aggregate = globalvalue!(self, aggregate, regularized_init_val);
                            let alloc = globalvalue!(self, global_alloc, aggregate);
                            let mut alloc_name = var_ident.clone();
                            alloc_name.insert_str(0, "%global_");
                            self.program.set_value_name(alloc, Some(alloc_name.clone()));
                            scope
                                .block_node
                                .insert_symbol(var_ident.clone(), Symbol::Array(alloc, dim_array));
                        }
                    }
                }
            }
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
                        scope
                            .block_node
                            .insert_symbol(var_ident.clone(), Symbol::Value(alloc));
                    } else {
                        // arrray
                        let mut dim_array = vec![];
                        for const_exp in const_exps {
                            let dim = self.visit_const_exp(const_exp, scope);
                            if let ValueKind::Integer(int) =
                                imvaluedata!(self, scope.function.unwrap(), dim).kind()
                            {
                                dim_array.push(int.value());
                            } else {
                                panic!("Must be a const!")
                            }
                        }
                        // calc space to alloc
                        let mut array_len = 1;
                        for dim in &dim_array {
                            array_len *= dim;
                        }

                        let array_type = Type::get_array(Type::get_i32(), array_len as usize);
                        let alloc = value!(self, scope.function.unwrap(), alloc, array_type);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc]
                        );
                        scope
                            .block_node
                            .insert_symbol(var_ident.clone(), Symbol::Array(alloc, dim_array));
                    }
                }
                // with initial vals...
                ast::VarDef::InitVal(var_ident, const_exps, init_val) => {
                    if const_exps.len() == 0 {
                        if let InitListVal::Exp(value) = self.visit_init_val(init_val, scope) {
                            let alloc =
                                value!(self, scope.function.unwrap(), alloc, Type::get_i32());
                            let store = value!(self, scope.function.unwrap(), store, value, alloc);
                            insertvalue!(
                                self,
                                scope.function.unwrap(),
                                scope.basic_block.unwrap(),
                                [alloc, store]
                            );
                            scope
                                .block_node
                                .insert_symbol(var_ident.clone(), Symbol::Value(alloc));
                        } else {
                            panic!("Can not be array!")
                        }
                    } else {
                        // arrray
                        let mut dim_array = vec![];
                        for const_exp in const_exps {
                            let dim = self.visit_const_exp(const_exp, scope);
                            if let ValueKind::Integer(int) =
                                imvaluedata!(self, scope.function.unwrap(), dim).kind()
                            {
                                dim_array.push(int.value());
                            } else {
                                panic!("Must be a const!")
                            }
                        }
                        // calc space to alloc
                        let mut array_len = 1;
                        for dim in &dim_array {
                            array_len *= dim;
                        }

                        let array_type = Type::get_array(Type::get_i32(), array_len as usize);
                        let alloc = value!(self, scope.function.unwrap(), alloc, array_type);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [alloc]
                        );
                        if let InitListVal::InitArray(init_array) =
                            self.visit_init_val(init_val, scope)
                        {
                            let regularized_init_array =
                                self.regularization_init_list(dim_array.clone(), init_array, scope);
                            for elem_index in 0..regularized_init_array.len() {
                                let index_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    elem_index as i32
                                );
                                let elem_ptr = value!(
                                    self,
                                    scope.function.unwrap(),
                                    get_elem_ptr,
                                    alloc,
                                    index_value
                                );
                                let store = value!(
                                    self,
                                    scope.function.unwrap(),
                                    store,
                                    regularized_init_array[elem_index],
                                    elem_ptr
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [elem_ptr, store]
                                );
                            }
                            scope
                                .block_node
                                .insert_symbol(var_ident.clone(), Symbol::Array(alloc, dim_array));
                        } else {
                            panic!("Must be an array!")
                        }
                    }
                }
            }
        }
    }

    fn visit_init_val(&mut self, init_val: &ast::InitVal, scope: &mut Scope) -> InitListVal {
        match init_val {
            ast::InitVal::Exp(exp) => InitListVal::Exp(self.visit_exp(exp, scope)),
            ast::InitVal::InitVal(init_val) => {
                // const array init values...
                let mut init_array = vec![];
                if let Some(vals) = init_val {
                    for val in vals {
                        init_array.push(self.visit_init_val(val, scope))
                    }
                }
                InitListVal::InitArray(init_array)
            }
        }
    }

    fn visit_func_def(&mut self, func_def: &crate::ast::FuncDef, scope: &mut Scope) {
        let mut sym_func_name = func_def.func_ident.clone();
        sym_func_name.insert(0, '@');

        let func_type = match func_def.func_type {
            ast::Type::Int => Type::get_i32(),
            ast::Type::Void => Type::get_unit(),
        };

        let function = if let Some(func_params) = &func_def.func_params {
            let mut params: Vec<(Option<String>, Type)> = vec![];
            for func_param in func_params {
                let param_type = match func_param.param_type {
                    ast::Type::Int => {
                        if let Some(_param_array) = &func_param.param_array {
                            unimplemented!()
                            //Type::get_pointer(Type::get_i32())
                        } else {
                            Type::get_i32()
                        }
                    }
                    _ => panic!("param type can not be void"),
                };
                let mut param_name = func_param.param_ident.clone();
                param_name.insert(0, '@');
                params.push((Some(param_name), param_type));
            }

            self.program.new_func(FunctionData::with_param_names(
                sym_func_name,
                params,
                func_type,
            ))
        } else {
            self.program
                .new_func(FunctionData::new(sym_func_name, Vec::new(), func_type))
        };

        self.bb_number.insert(function, 0); // init bb number for current function

        // new entry bb for current function
        let entry_bb = basicblock!(self, function, Some("%entry".into()));
        self.bbs_status.insert(entry_bb, BBStatus::Pushable);
        insertbb!(self, function, [entry_bb]);

        // If current function has return value
        if matches!(func_def.func_type, ast::Type::Void) {
            // add an end block to load return value and return
            let end_bb = basicblock!(self, function, Some("%end_bb".into()));
            self.bbs_status.insert(end_bb, BBStatus::Pushable);

            self.end_bbs.insert(function, (end_bb, None, false));
        } else {
            // alloc value to store return value
            let alloc = value!(self, function, alloc, Type::get_i32());

            // add an end block to load return value and return
            let end_bb = basicblock!(self, function, Some("%end_bb".into()));
            self.bbs_status.insert(end_bb, BBStatus::Pushable);

            self.end_bbs.insert(function, (end_bb, Some(alloc), false));
        }

        // new a local symbol table
        let block_node = BlockNode::new();
        BlockNode::insert_block(scope.global_symbols.clone(), block_node.clone());

        // Insert function to global symbol table
        scope
            .global_symbols
            .insert_symbol(func_def.func_ident.clone(), Symbol::Function(function));

        // if function has params, store params.

        if let Some(func_params) = &func_def.func_params {
            let param_values = imfuncdata!(self, function).params();
            let mut param_values_vec: Vec<Value> = vec![];
            for param_value in param_values {
                param_values_vec.push(*param_value);
            }
            for index in 0..func_params.len() {
                let param_value = param_values_vec[index];
                let param_type = match func_params[index].param_type {
                    ast::Type::Int => Type::get_i32(),
                    _ => panic!("param type can not be void"),
                };
                let alloc_value = value!(self, function, alloc, param_type);
                block_node.insert_symbol(
                    func_params[index].param_ident.clone(),
                    Symbol::Value(alloc_value),
                );
                let store_value = value!(self, function, store, param_value, alloc_value);
                insertvalue!(self, function, entry_bb, [alloc_value, store_value]);
            }
        }

        // set scope
        {
            scope.is_global = false;
            scope.function = Some(function);
            scope.basic_block = Some(entry_bb);
            scope.block_node = block_node;
        }

        self.visit_block(&func_def.block, scope);

        // If current function has return value
        if matches!(func_def.func_type, ast::Type::Void) {
            let (end_bb, _, _) = self.end_bbs.get(&function).unwrap();
            // check if current function has a return statement, if not, add a default one.
            if !self.has_ret_statements.contains(&function) {
                let ret_value = value!(self, scope.function.unwrap(), ret, None);
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [ret_value]
                );
            }
            insertbb!(self, function, [*end_bb]);

            let ret = value!(self, function, ret, None);
            insertvalue!(self, function, *end_bb, [ret]);
        } else {
            let (end_bb, alloc, _) = self.end_bbs.get(&function).unwrap();
            // check if current function has a return statement, if not, add a default one.
            if !self.has_ret_statements.contains(&function) {
                let zero_value = value!(self, scope.function.unwrap(), integer, 0);
                let ret_value = value!(self, scope.function.unwrap(), ret, Some(zero_value));
                insertvalue!(
                    self,
                    scope.function.unwrap(),
                    scope.basic_block.unwrap(),
                    [ret_value]
                );
            }
            insertbb!(self, function, [*end_bb]);

            let load = value!(self, function, load, alloc.unwrap());
            let ret = value!(self, function, ret, Some(load));
            insertvalue!(self, function, *end_bb, [load, ret]);
        }
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
            self.has_ret_statements.push(scope.function.unwrap());
            if let Some(exp) = &ret_stmt.exp {
                // with return value

                // visit exp of return
                let exp_value = self.visit_exp(exp, scope);

                // jump to end bb
                let (end_bb, return_value, has_insert_alloc) =
                    self.end_bbs.get(&scope.function.unwrap()).unwrap();

                if let Some(return_value) = return_value {
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
                        self.end_bbs.insert(
                            scope.function.unwrap(),
                            (*end_bb, Some(*return_value), true),
                        );
                    }
                } else {
                    panic!("void function should not return a value!")
                }
            } else {
                // void function without return value

                // If not in entry bb, jump to end bb
                let (end_bb, return_value, _) = self.end_bbs.get(&scope.function.unwrap()).unwrap();

                if matches!(return_value, None) {
                    let jmp_value = value!(self, scope.function.unwrap(), jump, *end_bb);

                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [jmp_value]
                    );
                    self.bbs_status
                        .insert(scope.basic_block.unwrap(), BBStatus::Branched);

                    self.end_bbs
                        .insert(scope.function.unwrap(), (*end_bb, None, false));
                } else {
                    panic!("int function should not return a void!")
                }
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
            panic!("Assign statement can not in global scope!")
        } else {
            let value = self.visit_exp(&ass_stmt.exp, scope);
            let ass_l_value_symbol = scope.block_node.lookup_symbol(&ass_stmt.l_val.ident);
            // TODO:must do some type check, because we can't assign to a const!
            match ass_l_value_symbol {
                Symbol::Value(ass_l_value) => {
                    let store = value!(self, scope.function.unwrap(), store, value, ass_l_value);
                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [store]
                    );
                }
                Symbol::Array(array_value, dim_edge_array) => {
                    // calc every dim size of dim_edge array
                    let mut new_dim_edge_array = vec![];
                    for index1 in 0..dim_edge_array.len() {
                        let mut each_dim_size = 1;
                        for index2 in index1 + 1..dim_edge_array.len() {
                            each_dim_size *= dim_edge_array[index2];
                        }
                        new_dim_edge_array.push(each_dim_size);
                    }

                    // find position in array
                    let mut dim_array = vec![];
                    let mut const_dim_sum = 0;
                    for exp_index in 0..ass_stmt.l_val.exps.len() {
                        let exp_value = self.visit_exp(&ass_stmt.l_val.exps[exp_index], scope);
                        match imvaluedata!(self, scope.function.unwrap(), exp_value).kind() {
                            ValueKind::Integer(int) => {
                                const_dim_sum += int.value() * new_dim_edge_array[exp_index];
                            }
                            _ => {
                                // calc
                                let dim_edge_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    new_dim_edge_array[exp_index]
                                );
                                let mul_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::Mul,
                                    exp_value,
                                    dim_edge_value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [mul_value]
                                );
                                dim_array.push(mul_value);
                            }
                        }
                    }
                    // add all dim to calc the offset
                    let mut dim_sum_value =
                        value!(self, scope.function.unwrap(), integer, const_dim_sum);
                    for dim in dim_array {
                        dim_sum_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::Add,
                            dim_sum_value,
                            dim
                        );
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [dim_sum_value]
                        );
                    }
                    //get element

                    let ptr_value = value!(
                        self,
                        scope.function.unwrap(),
                        get_elem_ptr,
                        array_value,
                        dim_sum_value
                    );
                    let store = value!(self, scope.function.unwrap(), store, value, ptr_value);
                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [ptr_value, store]
                    );
                }
                Symbol::Function(_) => {
                    panic!("Can not assign to a function!")
                }
            }
        }
    }

    fn visit_l_val(&mut self, l_val: &ast::LVal, scope: &mut Scope) -> Value {
        if scope.is_global {
            if l_val.exps.len() == 0 {
                // look up symbol table...
                let value_symbol = scope.block_node.lookup_symbol(&l_val.ident);
                if let Symbol::Value(value) = value_symbol {
                    let value_data = imglobalvaluedata!(self, value);
                    // check if it is a const
                    if !matches!(value_data.kind(), ValueKind::Integer(_)) {
                        panic!("Global left value must be a const!")
                    } else {
                        value
                    }
                } else {
                    panic!("Left value can not be a function!")
                }
            } else {
                panic!("R value can not be array in global scope!")
            }
        } else {
            if l_val.exps.len() == 0 {
                // look up symbol table...
                let value_symbol = scope.block_node.lookup_symbol(&l_val.ident);
                if let Symbol::Value(value) = value_symbol {
                    if value.is_global() {
                        // If it is a const, change it to local value
                        let value_data = imglobalvaluedata!(self, value);
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                value!(self, scope.function.unwrap(), integer, int.value())
                            }
                            ValueKind::GlobalAlloc(_alloc) => {
                                let load = value!(self, scope.function.unwrap(), load, value);
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [load]
                                );
                                load
                            }
                            _ => unimplemented!(),
                        }
                    } else {
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
                } else {
                    panic!("Expression must have a constant value!")
                }
            } else {
                if let Symbol::Array(array_value, dim_edge_array) =
                    scope.block_node.lookup_symbol(&l_val.ident)
                {
                    // calc every dim size of dim_edge array
                    let mut new_dim_edge_array = vec![];
                    for index1 in 0..dim_edge_array.len() {
                        let mut each_dim_size = 1;
                        for index2 in index1 + 1..dim_edge_array.len() {
                            each_dim_size *= dim_edge_array[index2];
                        }
                        new_dim_edge_array.push(each_dim_size);
                    }

                    // find position in array
                    let mut dim_array = vec![];
                    let mut const_dim_sum = 0;
                    for exp_index in 0..l_val.exps.len() {
                        let exp_value = self.visit_exp(&l_val.exps[exp_index], scope);
                        match imvaluedata!(self, scope.function.unwrap(), exp_value).kind() {
                            ValueKind::Integer(int) => {
                                const_dim_sum += int.value() * new_dim_edge_array[exp_index];
                            }
                            _ => {
                                // calc
                                let dim_edge_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    new_dim_edge_array[exp_index]
                                );
                                let mul_value = value!(
                                    self,
                                    scope.function.unwrap(),
                                    binary,
                                    BinaryOp::Mul,
                                    exp_value,
                                    dim_edge_value
                                );
                                insertvalue!(
                                    self,
                                    scope.function.unwrap(),
                                    scope.basic_block.unwrap(),
                                    [mul_value]
                                );
                                dim_array.push(mul_value);
                            }
                        }
                    }
                    // add all dim to calc the offset
                    let mut dim_sum_value =
                        value!(self, scope.function.unwrap(), integer, const_dim_sum);
                    for dim in dim_array {
                        dim_sum_value = value!(
                            self,
                            scope.function.unwrap(),
                            binary,
                            BinaryOp::Add,
                            dim_sum_value,
                            dim
                        );
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [dim_sum_value]
                        );
                    }
                    //get element

                    let ptr_value = value!(
                        self,
                        scope.function.unwrap(),
                        get_elem_ptr,
                        array_value,
                        dim_sum_value
                    );
                    let load = value!(self, scope.function.unwrap(), load, ptr_value);
                    insertvalue!(
                        self,
                        scope.function.unwrap(),
                        scope.basic_block.unwrap(),
                        [ptr_value, load]
                    );
                    return load;
                } else {
                    println!("{:#?}", scope.block_node.lookup_symbol(&l_val.ident));
                    panic!("Must be an array!")
                }
            }
        }
    }

    fn visit_const_exp(&mut self, const_exp: &ast::ConstExp, scope: &mut Scope) -> Value {
        if scope.is_global {
            // check if it is a const...
            let const_value = self.visit_exp(&const_exp.exp, scope);
            let val_data = imglobalvaluedata!(self, const_value);
            if let ValueKind::Integer(_) = val_data.kind() {
                const_value
            } else {
                panic!(
                    "Const expression's rvalue must be a constant, but got: {:#?}",
                    val_data.kind()
                )
            }
        } else {
            // check if it is a const...
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
            match l_or_exp {
                LOrExp::LAndExp(l_and_exp) => self.visit_l_and_exp(l_and_exp, scope),
                LOrExp::BinaryOp(l_or_exp, l_and_exp) => {
                    let (l_value, r_value) = (
                        self.visit_l_or_exp(l_or_exp, scope),
                        self.visit_l_and_exp(l_and_exp, scope),
                    );
                    // check if both lval and rval are const
                    let l_val_data = imglobalvaluedata!(self, l_value);
                    let r_val_data = imglobalvaluedata!(self, r_value);
                    if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                        (l_val_data.kind(), r_val_data.kind())
                    {
                        globalvalue!(
                            self,
                            integer,
                            (l_num.value() != 0 || r_num.value() != 0) as i32
                        )
                    } else {
                        panic!("Must be a const!")
                    }
                }
            }
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
            match l_and_exp {
                LAndExp::EqExp(eq_exp) => self.visit_eq_exp(eq_exp, scope),
                LAndExp::BinaryOp(l_and_exp, eq_exp) => {
                    let (l_value, r_value) = (
                        self.visit_l_and_exp(l_and_exp, scope),
                        self.visit_eq_exp(eq_exp, scope),
                    );
                    // check if both lval and rval are const
                    let l_val_data = imglobalvaluedata!(self, l_value);
                    let r_val_data = imglobalvaluedata!(self, r_value);
                    if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                        (l_val_data.kind(), r_val_data.kind())
                    {
                        globalvalue!(
                            self,
                            integer,
                            (l_num.value() != 0 && r_num.value() != 0) as i32
                        )
                    } else {
                        panic!("Must be a const!")
                    }
                }
            }
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
            match eq_exp {
                EqExp::RelExp(rel_exp) => self.visit_rel_exp(rel_exp, scope),
                EqExp::BinaryOp(eq_exp, op, rel_exp) => match op {
                    Operator::Equal => {
                        let (l_value, r_value) = (
                            self.visit_eq_exp(eq_exp, scope),
                            self.visit_rel_exp(rel_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() == r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::NotEqual => {
                        let (l_value, r_value) = (
                            self.visit_eq_exp(eq_exp, scope),
                            self.visit_rel_exp(rel_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() != r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() == r_num.value()) as i32
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() != r_num.value()) as i32
                            )
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
            match rel_exp {
                RelExp::AddExp(add_exp) => self.visit_add_exp(add_exp, scope),
                RelExp::BinaryOp(rel_exp, op, add_exp) => match op {
                    Operator::LessThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() < r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::MoreThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() > r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::LessOrEqualThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() <= r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::MoreOrEqualThan => {
                        let (l_value, r_value) = (
                            self.visit_rel_exp(rel_exp, scope),
                            self.visit_add_exp(add_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, (l_num.value() >= r_num.value()) as i32)
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() < r_num.value()) as i32
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() > r_num.value()) as i32
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() <= r_num.value()) as i32
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                (l_num.value() >= r_num.value()) as i32
                            )
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
            match add_exp {
                AddExp::MulExp(mul_exp) => self.visit_mul_exp(mul_exp, scope),
                AddExp::BinaryOp(add_exp, op, mul_exp) => match op {
                    Operator::Add => {
                        let (l_value, r_value) = (
                            self.visit_add_exp(add_exp, scope),
                            self.visit_mul_exp(mul_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, l_num.value() + r_num.value())
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::Subtract => {
                        let (l_value, r_value) = (
                            self.visit_add_exp(add_exp, scope),
                            self.visit_mul_exp(mul_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, l_num.value() - r_num.value())
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() + r_num.value()
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() - r_num.value()
                            )
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
            match mul_exp {
                MulExp::UnaryExp(unary_exp) => self.visit_unary_exp(unary_exp, scope),
                MulExp::BinaryOp(mul_exp, op, unary_exp) => match op {
                    Operator::Multiply => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, l_num.value() * r_num.value())
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::Divide => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, l_num.value() / r_num.value())
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    Operator::GetRemainder => {
                        let (l_value, r_value) = (
                            self.visit_mul_exp(mul_exp, scope),
                            self.visit_unary_exp(unary_exp, scope),
                        );
                        // check if both lval and rval are const
                        let l_val_data = imglobalvaluedata!(self, l_value);
                        let r_val_data = imglobalvaluedata!(self, r_value);
                        if let (ValueKind::Integer(l_num), ValueKind::Integer(r_num)) =
                            (l_val_data.kind(), r_val_data.kind())
                        {
                            globalvalue!(self, integer, l_num.value() % r_num.value())
                        } else {
                            panic!("Must be a const!")
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() * r_num.value()
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() / r_num.value()
                            )
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
                            value!(
                                self,
                                scope.function.unwrap(),
                                integer,
                                l_num.value() % r_num.value()
                            )
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
            match unary_exp {
                UnaryExp::PrimaryExp(primary_exp) => self.visit_primary_exp(primary_exp, scope),
                UnaryExp::FuncCall(_, _) => {
                    panic!("Could not be a function call in global symbols!")
                }
                UnaryExp::UnaryOp(op, unary_exp) => match op {
                    Operator::Add => self.visit_unary_exp(unary_exp, scope),
                    Operator::Subtract => {
                        let value = self.visit_unary_exp(unary_exp, scope);

                        // check if value is a const
                        let value_data = imglobalvaluedata!(self, value);
                        match value_data.kind() {
                            ValueKind::Integer(integer) => {
                                globalvalue!(self, integer, -integer.value())
                            }
                            _ => {
                                panic!("Must be a const!")
                            }
                        }
                    }
                    Operator::Not => {
                        let value = self.visit_unary_exp(unary_exp, scope);

                        // check if value is a const
                        let value_data = imglobalvaluedata!(self, value);
                        match value_data.kind() {
                            ValueKind::Integer(integer) => {
                                globalvalue!(self, integer, (integer.value() == 0) as i32)
                            }
                            _ => {
                                panic!("Must be a const!")
                            }
                        }
                    }
                    _ => {
                        panic!("Illegal Operator: {:#?}!", op)
                    }
                },
            }
        } else {
            match unary_exp {
                UnaryExp::PrimaryExp(primary_exp) => self.visit_primary_exp(primary_exp, scope),
                UnaryExp::FuncCall(func_name, func_r_params) => {
                    let mut args: Vec<Value> = vec![];
                    if let Some(func_r_params) = func_r_params {
                        for func_r_param in func_r_params {
                            let arg_value = self.visit_exp(func_r_param, scope);
                            args.push(arg_value);
                        }
                    }
                    let callee_symbol = scope.global_symbols.lookup_symbol(func_name);
                    if let Symbol::Function(callee) = callee_symbol {
                        let call_value = value!(self, scope.function.unwrap(), call, callee, args);
                        insertvalue!(
                            self,
                            scope.function.unwrap(),
                            scope.basic_block.unwrap(),
                            [call_value]
                        );
                        call_value
                    } else {
                        panic!("Must be a function...")
                    }
                }
                UnaryExp::UnaryOp(op, unary_exp) => match op {
                    Operator::Add => self.visit_unary_exp(unary_exp, scope),
                    Operator::Subtract => {
                        let value = self.visit_unary_exp(unary_exp, scope);

                        // check if value is a const
                        let value_data = imvaluedata!(self, scope.function.unwrap(), value);
                        match value_data.kind() {
                            ValueKind::Integer(integer) => {
                                value!(self, scope.function.unwrap(), integer, -integer.value())
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
                                value!(
                                    self,
                                    scope.function.unwrap(),
                                    integer,
                                    (integer.value() == 0) as i32
                                )
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
            match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, scope),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        let number_value = globalvalue!(self, integer, *int_const);
                        number_value
                    }
                },
                PrimaryExp::LVal(l_val) => self.visit_l_val(l_val, scope),
            }
        } else {
            match primary_exp {
                PrimaryExp::Exp(exp) => self.visit_exp(exp, scope),
                PrimaryExp::Number(number) => match number {
                    Number::IntConst(int_const) => {
                        value!(self, scope.function.unwrap(), integer, *int_const)
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
            has_ret_statements: vec![],
        }
    }

    pub fn get_new_bb_number(&mut self, function: Function) -> String {
        self.bb_number
            .insert(function, *self.bb_number.get(&function).unwrap() + 1);
        format!("%bb{}", *self.bb_number.get(&function).unwrap() - 1)
    }

    pub fn parse(&mut self, source_code: &String) -> Result<&Program, Box<dyn Error>> {
        let ast = sysy::CompUnitParser::new().parse(&source_code).unwrap();
        // println!("{:#?}", ast);
        let global_block_node = BlockNode::new();
        let mut scope = Scope {
            is_global: true,
            function: None,
            basic_block: None,
            block_node: global_block_node.clone(),
            global_symbols: global_block_node.clone(),
            last_nested_bb: None,
            last_end_exp_bb: None,
            curr_loop_bb: None,
        };

        {
            // insert sysy lib function

            // decl @getint(): i32
            let getint = self.program.new_func(FunctionData::new_decl(
                "@getint".to_string(),
                vec![],
                Type::get_i32(),
            ));
            scope
                .global_symbols
                .insert_symbol("getint".to_string(), Symbol::Function(getint));

            // decl @getch(): i32
            let getch = self.program.new_func(FunctionData::new_decl(
                "@getch".to_string(),
                vec![],
                Type::get_i32(),
            ));
            scope
                .global_symbols
                .insert_symbol("getch".to_string(), Symbol::Function(getch));

            // decl @getarray(*i32): i32
            //parser.program.new_func(FunctionData::new_decl("@getarray".to_string(), vec![Type::get_pointer(Type::get_i32())], Type::get_i32()));

            // decl @putint(i32)
            let putint = self.program.new_func(FunctionData::new_decl(
                "@putint".to_string(),
                vec![Type::get_i32()],
                Type::get_unit(),
            ));
            scope
                .global_symbols
                .insert_symbol("putint".to_string(), Symbol::Function(putint));

            // decl @putch(i32)
            let putch = self.program.new_func(FunctionData::new_decl(
                "@putch".to_string(),
                vec![Type::get_i32()],
                Type::get_unit(),
            ));
            scope
                .global_symbols
                .insert_symbol("putch".to_string(), Symbol::Function(putch));

            // decl @putarray(i32, *i32)
            //let get_int = FunctionData::new_decl("@get_int".to_string(), vec![], Type::get_i32());

            // decl @starttime()
            let starttime = self.program.new_func(FunctionData::new_decl(
                "@starttime".to_string(),
                vec![],
                Type::get_unit(),
            ));
            scope
                .global_symbols
                .insert_symbol("starttime".to_string(), Symbol::Function(starttime));

            // decl @stoptime()
            let stoptime = self.program.new_func(FunctionData::new_decl(
                "@stoptime".to_string(),
                vec![],
                Type::get_unit(),
            ));
            scope
                .global_symbols
                .insert_symbol("stoptime".to_string(), Symbol::Function(stoptime));
        }
        self.visit_comp_unit(&ast, &mut scope);
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
