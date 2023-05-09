pub type Ident = String;

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

// CompUnit ::= [CompUnit] (Decl | FuncDef);
#[derive(Debug)]
pub enum CompUnit {
    Decl(Option<Box<CompUnit>>, Decl),
    FuncDef(Option<Box<CompUnit>>, FuncDef),
}

// Decl ::= ConstDecl | VarDecl;
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

// ConstDecl ::= "const" BType ConstDef {"," ConstDef} ";";
#[derive(Debug)]
pub struct ConstDecl {
    pub const_type: Type,
    pub const_defs: Vec<ConstDef>,
}

// ConstDef ::= IDENT {"[" ConstExp "]"} "=" ConstInitVal;
#[derive(Debug)]
pub struct ConstDef {
    pub const_ident: Ident,
    pub const_exps: Vec<ConstExp>,
    pub const_init_val: ConstInitVal,
}

// ConstInitVal ::= ConstExp | "{" [ConstInitVal {"," ConstInitVal}] "}";
#[derive(Debug)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVal(Option<Vec<ConstInitVal>>),
}

// VarDecl ::= BType VarDef {"," VarDef} ";";
#[derive(Debug)]
pub struct VarDecl {
    pub var_type: Type,
    pub var_defs: Vec<VarDef>,
}

// VarDef ::= IDENT {"[" ConstExp "]"}
// | IDENT {"[" ConstExp "]"} "=" InitVal;
#[derive(Debug)]
pub enum VarDef {
    VarDef(Ident, Vec<ConstExp>),
    InitVal(Ident, Vec<ConstExp>, InitVal),
}

// InitVal ::= Exp | "{" [InitVal {"," InitVal}] "}";
#[derive(Debug)]
pub enum InitVal {
    Exp(Exp),
    InitVal(Option<Vec<InitVal>>),
}

// FuncDef ::= FuncType IDENT "(" [FuncFParams] ")" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: Type,
    pub func_ident: Ident,
    pub func_params: Option<FuncFParams>,
    pub block: Block,
}

// FuncFParams ::= FuncFParam {"," FuncParam};
pub type FuncFParams = Vec<FuncFParam>;

// FuncFParam ::= BType IDENT ["[" "]" {"[" ConstExp "]"}];
#[derive(Debug)]
pub struct FuncFParam {
    pub param_type: Type,
    pub param_ident: Ident,
    pub param_array: Option<Vec<ConstExp>>,
}

// Type ::= "void" | "int";
#[derive(Debug)]
pub enum Type {
    Void,
    Int,
}

// Block ::= "{" {BlockItem} "}";
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

// BlockItem ::= Decl | Stmt;
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

// Stmt ::= LVal "=" Exp ";"
// | [Exp] ";"
// | Block
// | "if" "(" Exp ")" Stmt ["else" Stmt]
// | "while" "(" Exp ")" Stmt
// | "break" ";"
// | "continue" ";"
// | RetStmt;
#[derive(Debug)]
pub enum Stmt {
    RetStmt(RetStmt),
    AssignStmt(AssignStmt),
    ExpStmt(ExpStmt),
    BlockStmt(BlockStmt),
    IfStmt(Box<IfStmt>),
    WhileStmt(Box<WhileStmt>),
    BreakStmt,
    ContinueStmt,
}

// BlockStmt ::= Block
#[derive(Debug)]
pub struct BlockStmt {
    pub block: Block,
}

// RetStmt ::= "return" [Exp] ";";
#[derive(Debug)]
pub struct RetStmt {
    pub exp: Option<Exp>,
}

// ExpStmt ::= [Exp] ";";
#[derive(Debug)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

// IfStmt := "if" "(" Exp ")" Stmt ["else" Stmt];
#[derive(Debug)]
pub struct IfStmt {
    pub cond_exp: Exp,
    pub then_stmt: Stmt,
    pub else_stmt: Option<Stmt>,
}

// WhileStmt := "while" "(" Exp ")" Stmt;
#[derive(Debug)]
pub struct WhileStmt {
    pub cond_exp: Exp,
    pub loop_stmt: Stmt,
}

// AssignStmt ::= LVal "=" Exp ";"
#[derive(Debug)]
pub struct AssignStmt {
    pub l_val: LVal,
    pub exp: Exp,
}

// LVal ::= IDENT {"[" Exp "]"};
#[derive(Debug)]
pub struct LVal {
    pub ident: Ident,
    pub exps: Vec<Exp>,
}

// ConstExp ::= Exp;
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
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

// UnaryExp ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncCall(Ident, Option<FuncRParams>),
    UnaryOp(Operator, Box<UnaryExp>),
}

// FuncRParams ::= Exp {"," Exp};
pub type FuncRParams = Vec<Exp>;

// PrimaryExp ::= "(" Exp ")" | LVal | Number;
#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Exp),
    LVal(LVal),
    Number(Number),
}

#[derive(Debug)]
pub enum Number {
    IntConst(i32),
}
