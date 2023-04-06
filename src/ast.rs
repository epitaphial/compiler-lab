use std::fmt::{self};

// Expect: CompUnit ::= [CompUnit] (Decl | FuncDef);
// Now: CompUnit ::= [CompUnit] FuncDef;
pub enum CompUnit {
    FuncDef(Box<FuncDef>),
}

// Expect: FuncDef ::= FuncType IDENT "(" [FuncFParams] ")" Block;
// Now: FuncDef ::= FuncType IDENT "()" Block;
pub struct FuncDef {
    pub func_type: FuncType,
    pub func_ident: String,
    pub block: Block,
}

// FuncType ::= "void" | "int";
enum FuncType {
    VOID,
    INT,
}

// Block ::= "{" BlockItemList "}" | "{" "}";
pub enum Block {
    BlockItemList,
    VoidBlock,
}

// BlockItemList ::= BlockItemList BlockItem;
pub struct BlockItemList{
    block_item_list:Box<Vec<BlockItem>>,
}


// Expect: BlockItem ::= Decl | Stmt;
// Now: BlockItem ::= Stmt;
pub enum BlockItem {
    Stmt(),
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
    RetStmt(),
}

// RetStmt ::= "return" Exp ";" | "return" ";";
pub enum RetStmt {
    Exp,
    VoidRetStmt,
}


// Expect: Exp ::= LOrExp;
// Now: Exp ::= UnaryExp;
pub struct Exp {
    unary_exp:Box<UnaryExp>,
}

// Expect: UnaryExp ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
// Now: UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
pub enum UnaryExp {
    PrimaryExp(),
    UnaryExp(),
}

// UnaryOp ::= "+" | "-" | "!";
pub enum UnaryOp {
    PLUS,
    MINUS,
    NOT,
}

// Expect: PrimaryExp ::= "(" Exp ")" | LVal | Number;
// Now: PrimaryExp ::= "(" Exp ")" | Number;
pub enum PrimaryExp {
    
}