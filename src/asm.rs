use std::collections::HashMap;

use koopa::ir::Value;

#[derive(Default, Debug)]
pub struct AsmProgram {
    functions: Vec<AsmFunction>,
    // other symbols...
}

impl AsmProgram {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_func(&mut self, func: AsmFunction) {
        self.functions.push(func);
    }

    pub fn gen_asm(&self) -> String {
        let mut asm_str = String::new();
        for asm_func in &self.functions {
            asm_str.push_str("  .text\n");

            asm_str.push_str(format!("  .globl {}\n", &asm_func.func_name).as_str());
            asm_str.push_str(format!("{}:\n", &asm_func.func_name).as_str());

            for asm_bb in &asm_func.blocks {
                if !asm_bb.bb_name.contains("entry") {
                    asm_str.push_str(
                        format!("{}.{}:\n", asm_func.func_name, &asm_bb.bb_name).as_str(),
                    );
                }
                for asm_inst in &asm_bb.insts {
                    asm_str
                        .push_str(format!("  {}\n", asm_inst.clone().to_riscv_string()).as_str());
                }
            }
            asm_str.push_str(format!("\n").as_str());
        }
        asm_str
    }
}

#[derive(Debug)]
pub enum ArgType {
    Register(Register),
    StackPos(u32),
}

// layout for each function, include temp registers and stack.
#[derive(Debug)]
pub struct Layout {
    pub stack: Stack,
    pub registers: HashMap<Value, Register>,
    pub arg_map: HashMap<Value, ArgType>,
}

impl Layout {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            registers: HashMap::new(),
            arg_map: HashMap::new(),
        }
    }
}

// kind of registers
#[derive(Debug, Clone, Copy)]
pub enum Register {
    _Zero,
    ReturnAddress,
    StackPointer,
    Temp(u32), // t0-t6
    Arg(u32),  // a0-a7
}

impl Register {
    pub fn to_riscv_string(self) -> String {
        match self {
            Register::_Zero => {
                format!("zero")
            }
            Register::ReturnAddress => {
                format!("ra")
            }
            Register::StackPointer => {
                format!("sp")
            }
            Register::Temp(tmp) => {
                format!("t{}", tmp)
            }
            Register::Arg(arg) => {
                format!("a{}", arg)
            }
        }
    }
}

#[derive(Debug)]
pub struct Stack {
    stack_map: HashMap<Value, u32>,
    stack_size: u32,
    stack_sp: u32,
    ra_pos: Option<u32>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            stack_map: HashMap::new(),
            stack_size: 0,
            stack_sp: 0,
            ra_pos: None,
        }
    }
    pub fn get_stack_size(&self) -> u32 {
        self.stack_size
    }

    pub fn get_stack_sp(&self) -> u32 {
        self.stack_sp
    }

    pub fn set_stack_size(&mut self, stack_size: u32) {
        self.stack_size = stack_size;
    }

    pub fn set_stack_sp(&mut self, stack_size: u32) {
        self.stack_sp = stack_size;
    }

    pub fn get_pos_by_value(&self, value: Value) -> u32 {
        *self.stack_map.get(&value).unwrap()
    }

    pub fn insert_value(&mut self, value: Value, value_size: u32) {
        self.stack_map.insert(value, self.stack_sp);
        self.stack_sp -= value_size;
    }

    pub fn push_ra(&mut self) -> u32 {
        self.ra_pos = Some(self.stack_sp);
        self.stack_sp -= 4;
        self.ra_pos.unwrap()
    }

    pub fn get_ra(&self) -> Option<u32> {
        self.ra_pos
    }
}

// For each fucntion, there must be a hashmap to store the values stack location.
#[derive(Default, Debug)]
pub struct AsmFunction {
    func_name: String,
    blocks: Vec<AsmBasicBlock>,
}

impl AsmFunction {
    pub fn new(func_name: String) -> Self {
        AsmFunction {
            func_name: func_name[1..].to_string(),
            ..Self::default()
        }
    }

    pub fn push_bb(&mut self, bb: AsmBasicBlock) {
        self.blocks.push(bb);
    }

    pub fn insert_prologue(&mut self, insts: Vec<AsmInstruction>) {
        for asm_bb in &mut self.blocks {
            let mut new_insts = insts.clone();
            new_insts.reverse();
            if asm_bb.bb_name == "entry" {
                for inst in &new_insts {
                    asm_bb.insert_inst(0, inst.clone());
                }
            }
        }
    }
}

// The riscv instruction include the blockname
#[derive(Default, Debug)]
pub struct AsmBasicBlock {
    bb_name: String,
    insts: Vec<AsmInstruction>,
}

impl AsmBasicBlock {
    pub fn new(bb_name: String) -> Self {
        AsmBasicBlock {
            bb_name: bb_name[1..].to_string(),
            ..Self::default()
        }
    }
    pub fn push_inst(&mut self, inst: AsmInstruction) {
        self.insts.push(inst);
    }
    pub fn insert_inst(&mut self, index: usize, inst: AsmInstruction) {
        self.insts.insert(index, inst);
    }
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Ret,
    J(String),
    Call(String),
    Li(Register, i32),
    Beqz(Register, String),
    Seqz(Register, Register),
    Snez(Register, Register),
    Lw(Register, Register, i32),
    Sw(Register, Register, i32),
    Add(Register, Register, Register),
    Addi(Register, Register, i32),
    Sub(Register, Register, Register),
    Mul(Register, Register, Register),
    Div(Register, Register, Register),
    Rem(Register, Register, Register),
    And(Register, Register, Register),
    Or(Register, Register, Register),
    Xor(Register, Register, Register),
    Xori(Register, Register, i32),
    Slt(Register, Register, Register),
    Sgt(Register, Register, Register),
}

impl AsmInstruction {
    fn to_riscv_string(self) -> String {
        match self {
            AsmInstruction::Ret => {
                format!("ret")
            }
            AsmInstruction::J(label) => {
                format!("j {}", label)
            }
            AsmInstruction::Call(label) => {
                format!("call {}", label)
            }
            AsmInstruction::Li(reg, imm) => {
                format!("li {}, {}", reg.to_riscv_string(), imm)
            }
            AsmInstruction::Lw(reg1, reg2, pos) => {
                format!(
                    "lw {}, {}({})",
                    reg1.to_riscv_string(),
                    pos,
                    reg2.to_riscv_string()
                )
            }
            AsmInstruction::Sw(reg1, reg2, pos) => {
                format!(
                    "sw {}, {}({})",
                    reg1.to_riscv_string(),
                    pos,
                    reg2.to_riscv_string()
                )
            }
            AsmInstruction::Beqz(reg, label) => {
                format!("beqz {}, {}", reg.to_riscv_string(), label)
            }
            AsmInstruction::Seqz(reg1, reg2) => {
                format!(
                    "seqz {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string()
                )
            }
            AsmInstruction::Snez(reg1, reg2) => {
                format!(
                    "snez {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string()
                )
            }
            AsmInstruction::Add(reg1, reg2, reg3) => {
                format!(
                    "add {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Addi(reg1, reg2, imm) => {
                format!(
                    "addi {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    imm
                )
            }
            AsmInstruction::Sub(reg1, reg2, reg3) => {
                format!(
                    "sub {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Mul(reg1, reg2, reg3) => {
                format!(
                    "mul {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Div(reg1, reg2, reg3) => {
                format!(
                    "div {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Rem(reg1, reg2, reg3) => {
                format!(
                    "rem {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::And(reg1, reg2, reg3) => {
                format!(
                    "and {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Or(reg1, reg2, reg3) => {
                format!(
                    "or {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Xor(reg1, reg2, reg3) => {
                format!(
                    "xor {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Xori(reg1, reg2, imm) => {
                format!(
                    "xori {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    imm
                )
            }
            AsmInstruction::Slt(reg1, reg2, reg3) => {
                format!(
                    "slt {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
            AsmInstruction::Sgt(reg1, reg2, reg3) => {
                format!(
                    "sgt {}, {}, {}",
                    reg1.to_riscv_string(),
                    reg2.to_riscv_string(),
                    reg3.to_riscv_string()
                )
            }
        }
    }
}
