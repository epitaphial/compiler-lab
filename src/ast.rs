use std::fmt;

// Expect: CompUnit ::= [CompUnit] (Decl | FuncDef);
// Now: CompUnit ::= [CompUnit] FuncDef;
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
        write!(f, "FuncDef{{\nfunc_type:{:#?}\nfunc_ident:\"{}\"\nblock:{:#?}\n}}", self.func_type,self.func_ident,self.block)
    }
}

// FuncType ::= "void" | "int";
#[derive(Debug)]
pub enum FuncType {
    TypeVoid,
    TypeInt,
}

impl fmt::Debug for CompUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FuncType::TypeVoid =>{

            },
            FuncType::TypeInt=>{

            }
        }
    }
}

// Block ::= "{" BlockItems "}" | "{" "}";
#[derive(Debug)]
pub enum Block {
    BlockItems(BlockItems),
    Void,
}

// BlockItems ::= BlockItems BlockItem | BlockItem;
// #[derive(Debug)]
// pub enum BlockItems {
//     BlockItems(Vec<BlockItem>),
//     BlockItem,
// }
pub type BlockItems = Vec<BlockItem>;

// Expect: BlockItem ::= Decl | Stmt;
// Now: BlockItem ::= Stmt;
#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
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
#[derive(Debug)]
pub enum Stmt {
    RetStmt(RetStmt),
}

// RetStmt ::= "return" Exp ";" | "return" ";";
#[derive(Debug)]
pub enum RetStmt {
    Exp(Exp),
    Void,
}

// Expect: Exp ::= LOrExp;
// Now: Exp ::= UnaryExp;
#[derive(Debug)]
pub struct Exp {
    pub unary_exp: UnaryExp,
}

// Expect: UnaryExp ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
// Now: UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

// UnaryOp ::= "+" | "-" | "!";
#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

// Expect: PrimaryExp ::= "(" Exp ")" | LVal | Number;
// Now: PrimaryExp ::= "(" Exp ")" | Number;
#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Exp),
    Number,
}

#[derive(Debug)]
pub enum Number {
    IntConst(i32),
}
