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

// Expect: FuncDef ::= FuncType IDENT "(" [FuncFParams] ")" Block;
// Now: FuncDef ::= FuncType IDENT "()" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: Type,
    pub func_ident: Ident,
    pub block: Block,
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

// Expect: Stmt ::= LVal "=" Exp ";"
// | [Exp] ";"
// | Block
// | "if" "(" Exp ")" Stmt ["else" Stmt]
// | "while" "(" Exp ")" Stmt
// | "break" ";"
// | "continue" ";"
// | RetStmt;
// Now: Stmt ::= RetStmt
// | AssignStmt;
#[derive(Debug)]
pub enum Stmt {
    RetStmt(RetStmt),
    AssignStmt(AssignStmt),
    ExpStmt(ExpStmt),
    BlockStmt(BlockStmt),
    IfStmt(Box<IfStmt>),
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

// Expect: UnaryExp ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
// Now: UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp(Operator, Box<UnaryExp>),
}

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

pub trait Visitor<T, V> {
    fn visit_comp_unit(&mut self, comp_unit: &CompUnit) -> T;
    fn visit_decl(&mut self, decl: &Decl, v: V) -> T;
    fn visit_const_decl(&mut self, const_decl: &ConstDecl, v: V) -> T;
    fn visit_const_def(&mut self, const_def: &ConstDef, v: V) -> T;
    fn visit_const_init_val(&mut self, cons_init_val: &ConstInitVal, v: V) -> T;
    fn visit_var_decl(&mut self, var_decl: &VarDecl, v: V) -> T;
    fn visit_var_def(&mut self, var_def: &VarDef, v: V) -> T;
    fn visit_init_val(&mut self, init_val: &InitVal, v: V) -> T;
    fn visit_func_def(&mut self, func_def: &FuncDef) -> T;
    fn visit_block(&mut self, block: &Block, v: V) -> T;
    fn visit_block_item(&mut self, block_item: &BlockItem, v: V) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt, v: V) -> T;
    fn visit_if_stmt(&mut self, if_stmt: &IfStmt, v: V) -> T;
    fn visit_block_stmt(&mut self, block_stmt: &BlockStmt, v: V) -> T;
    fn visit_ret_stmt(&mut self, ret_stmt: &RetStmt, v: V) -> T;
    fn visit_exp_stmt(&mut self, exp_stmt: &ExpStmt, v: V) -> T;
    fn visit_ass_stmt(&mut self, ass_stmt: &AssignStmt, v: V) -> T;
    fn visit_l_val(&mut self, l_val: &LVal, v: V) -> T;
    fn visit_const_exp(&mut self, const_exp: &ConstExp, v: V) -> T;
    fn visit_exp(&mut self, exp: &Exp, v: V) -> T;
    fn visit_l_or_exp(&mut self, l_or_exp: &LOrExp, v: V) -> T;
    fn visit_l_and_exp(&mut self, l_and_exp: &LAndExp, v: V) -> T;
    fn visit_eq_exp(&mut self, eq_exp: &EqExp, v: V) -> T;
    fn visit_rel_exp(&mut self, rel_exp: &RelExp, v: V) -> T;
    fn visit_add_exp(&mut self, add_exp: &AddExp, v: V) -> T;
    fn visit_mul_exp(&mut self, mul_exp: &MulExp, v: V) -> T;
    fn visit_unary_exp(&mut self, unary_exp: &UnaryExp, v: V) -> T;
    fn visit_primary_exp(&mut self, primary_exp: &PrimaryExp, v: V) -> T;
}
