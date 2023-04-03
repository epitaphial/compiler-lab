use std::fmt::{self};

pub struct CompUnit {
    pub func_def:FuncDef,
}

impl fmt::Display for CompUnit{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"comp_unit{{\n{}\n}}",self.func_def)
    }
}

pub struct FuncDef {
    pub func_type:FuncType,
    pub func_ident:String,
    pub block:Block,
}

impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"func_def{{\n\
            func_type: {}\n\
            func_ident: {}\n\
            block: {}\n\
        }}",self.func_type,self.func_ident,self.block)
    }
}

pub struct Block {
    pub ret_stmt:RetStmt,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"block{{\n{}\n}}",self.ret_stmt)
    }
}

pub struct RetStmt {
    pub ret_num:i64,   
}

impl fmt::Display for RetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"retstmt{{\nretnum: {}\n}}",self.ret_num)
    }
}

#[derive(Debug)]
pub enum FuncType{
    TypeInt,
    TypeDouble,
    TypeFloat,
    TypeVoid,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FuncType::TypeInt=>write!(f,"int"),
            _=>write!(f,"other type")
        }
    }
}