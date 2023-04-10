use std::fmt;

use koopa::ir::{BasicBlock, Function};

#[derive(Debug)]
pub enum Operator {
    Not,
    Add,
    Subtract,
    Multiply,
    Divide,
    GetRemainder,
    //BitwiseAnd,
    //BitwiseOr,
    //BitwiseXor,
    _LogicalAnd,
    _LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    MoreThan,
    LessOrEqualThan,
    MoreOrEqualThan,
}

// Expect: CompUnit ::= [CompUnit] (Decl | FuncDef);
// Now: CompUnit ::= FuncDef;
pub enum CompUnit {
    FuncDef(FuncDef),
}

impl fmt::Debug for CompUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let CompUnit::FuncDef(func_def) = self;
        write!(f, "CompUnit{{\n{:#?}\n}}", func_def)
    }
}

// Expect: FuncDef ::= FuncType IDENT "(" [FuncFParams] ")" Block;
// Now: FuncDef ::= FuncType IDENT "()" Block;
pub struct FuncDef {
    pub func_type: FuncType,
    pub func_ident: String,
    pub block: Block,
}

impl fmt::Debug for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FuncDef{{\nfunc_type:{:#?}\nfunc_ident:\"{}\"\nblock:{:#?}\n}}",
            self.func_type, self.func_ident, self.block
        )
    }
}

// FuncType ::= "void" | "int";
pub enum FuncType {
    TypeVoid,
    TypeInt,
}

impl fmt::Debug for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FuncType::TypeVoid => {
                write!(f, "FuncType{{\nVoid\n}}\n")
            }
            FuncType::TypeInt => {
                write!(f, "FuncType{{\nInt\n}}\n")
            }
        }
    }
}

// Block ::= "{" BlockItems "}" | "{" "}";
pub enum Block {
    BlockItems(BlockItems),
    Void,
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Block::BlockItems(block_items) => {
                write!(f, "BlockItems{{\n{:#?}\n}}", block_items)
            }
            Block::Void => {
                write!(f, "BlockItems{{\nVoidBlock}}\n")
            }
        }
    }
}

pub type BlockItems = Vec<BlockItem>;

// Expect: BlockItem ::= Decl | Stmt;
// Now: BlockItem ::= Stmt;
pub enum BlockItem {
    Stmt(Stmt),
}

impl fmt::Debug for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockItem::Stmt(stmt) => {
                write!(f, "BlockItem{{\n{:#?}\n}}", stmt)
            }
        }
    }
}

// Expect: Stmt ::= LVal "=" Exp ";"
// | [Exp] ";"
// | Block
// | "if" "(" Exp ")" Stmt ["else" Stmt]
// | "while" "(" Exp ")" Stmt
// | "break" ";"
// | "continue" ";"
// | RetStmt;
// Now: Stmt ::= RetStmt;
pub enum Stmt {
    RetStmt(RetStmt),
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::RetStmt(ret_stmt) => {
                write!(f, "Stmt{{\n{:#?}}}\n", ret_stmt)
            }
        }
    }
}

// RetStmt ::= "return" Exp ";" | "return" ";";
pub enum RetStmt {
    Exp(Exp),
    Void,
}

impl fmt::Debug for RetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetStmt::Exp(exp) => {
                write!(f, "RetStmt{{\n{:#?}}}\n", exp)
            }
            RetStmt::Void => {
                write!(f, "RetStmt{{\n}}\n")
            }
        }
    }
}

// Exp ::= LOrExp;
#[derive(Debug)]
pub struct Exp {
    pub l_or_exp: Box<LOrExp>,
}

// LOrExp ::= LAndExp | LOrExp "||" LAndExp;
#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    BinaryOp(Box<LOrExp>, LAndExp),
}

// LAndExp ::= EqExp | LAndExp "&&" EqExp;
#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    BinaryOp(Box<LAndExp>, EqExp),
}

// EqExp ::= RelExp | EqExp ("==" | "!=") RelExp;
#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    BinaryOp(Box<EqExp>, Operator, RelExp),
}

// RelExp ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    BinaryOp(Box<RelExp>, Operator, AddExp),
}

// AddExp ::= MulExp | AddExp ("+" | "-") MulExp;
#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    BinaryOp(Box<AddExp>, Operator, MulExp),
}

// MulExp ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    BinaryOp(Box<MulExp>, Operator, UnaryExp),
}

// Expect: UnaryExp ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
// Now: UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp(Operator, Box<UnaryExp>),
}

impl fmt::Debug for UnaryExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => {
                write!(f, "UnaryExp{{\n{:#?}}}\n", primary_exp)
            }
            UnaryExp::UnaryOp(unary_op, unary_exp) => {
                write!(f, "UnaryExp{{\n{:#?}{:#?}}}\n", unary_op, unary_exp)
            }
        }
    }
}

// Expect: PrimaryExp ::= "(" Exp ")" | LVal | Number;
// Now: PrimaryExp ::= "(" Exp ")" | Number;
pub enum PrimaryExp {
    Exp(Exp),
    Number(Number),
}

impl fmt::Debug for PrimaryExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimaryExp::Exp(exp) => {
                write!(f, "PrimaryExp{{\n{:#?}\n}}\n", exp)
            }
            PrimaryExp::Number(number) => {
                write!(f, "PrimaryExp{{\n{:#?}\n}}\n", number)
            }
        }
    }
}

pub enum Number {
    IntConst(i32),
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::IntConst(int_const) => {
                write!(f, "Number({})\n", int_const)
            }
        }
    }
}

pub trait Visitor<T> {
    fn visit_comp_unit(&mut self, comp_unit: &CompUnit) -> T;
    fn visit_func_def(&mut self, func_def: &FuncDef) -> T;
    fn visit_block(&mut self, block: &Block, function: &Function, bb: &BasicBlock) -> T;
    fn visit_block_item(
        &mut self,
        block_item: &BlockItem,
        function: &Function,
        bb: &BasicBlock,
    ) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt, function: &Function, bb: &BasicBlock) -> T;
    fn visit_ret_stmt(&mut self, ret_stmt: &RetStmt, function: &Function, bb: &BasicBlock) -> T;
    fn visit_exp(&mut self, exp: &Exp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_l_or_exp(&mut self, l_or_exp: &LOrExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_l_and_exp(&mut self, l_and_exp: &LAndExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_eq_exp(&mut self, eq_exp: &EqExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_rel_exp(&mut self, rel_exp: &RelExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_add_exp(&mut self, add_exp: &AddExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_mul_exp(&mut self, mul_exp: &MulExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_unary_exp(&mut self, unary_exp: &UnaryExp, function: &Function, bb: &BasicBlock) -> T;
    fn visit_primary_exp(
        &mut self,
        primary_exp: &PrimaryExp,
        function: &Function,
        bb: &BasicBlock,
    ) -> T;
}
