use std::collections::HashMap;

use koopa::ir::Value;

#[derive(Default, Debug)]
pub struct AsmProgram {
    functions: Vec<Box<AsmFunction>>,
    // other symbols...
}

impl AsmProgram {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_func(&mut self, func: AsmFunction) {
        self.functions.push(Box::new(func));
    }

    pub fn gen_asm(&self) -> String {
        let mut asm_str = String::new();
        for asm_func in &self.functions {
            asm_str.push_str("  .text\n");
            if let FunctionName::Other(func_name) = &asm_func.func_name {
                asm_str.push_str(format!("  .globl {}\n", func_name).as_str());
                asm_str.push_str(format!("{}:\n", func_name).as_str());
            } else {
                asm_str.push_str("  .globl main\n");
                asm_str.push_str("main:\n");
            }
            for asm_bb in &asm_func.blocks {
                if let BlockName::Other(bb_name) = &asm_bb.bb_name {
                    asm_str.push_str(format!("{}:\n", &bb_name[1..]).as_str());
                }
                for asm_insts in &asm_bb.insts {
                    for asm_inst in &asm_insts.insts_str {
                        asm_str.push_str(format!("  {}\n", asm_inst).as_str());
                    }
                }
            }
        }
        asm_str
    }
}

#[derive(Debug, Default)]
pub enum FunctionName {
    #[default]
    Main,
    Other(String),
}

#[derive(Default, Debug)]
pub struct Stack {
    // first u32 is value's position
    stack_map: HashMap<Value, u32>,
    stack_size: u32,
    stack_sp: u32,
}

impl Stack {
    pub fn new() -> Self {
        Self::default()
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

    pub fn get_pos_by_value(&self, value: Value) -> u32 {
        *self.stack_map.get(&value).unwrap()
    }

    pub fn insert_value(&mut self, value: Value, value_size: u32) {
        self.stack_map.insert(value, self.stack_sp);
        self.stack_sp += value_size;
    }
}

// For each fucntion, there must be a hashmap to store the values stack location.
#[derive(Default, Debug)]
pub struct AsmFunction {
    func_name: FunctionName,
    blocks: Vec<Box<AsmBasicBlock>>,
}

impl AsmFunction {
    pub fn new(func_name: String) -> Self {
        let func_name = if func_name.eq("@main") {
            FunctionName::Main
        } else {
            FunctionName::Other(func_name)
        };
        AsmFunction {
            func_name,
            ..Self::default()
        }
    }

    pub fn push_bb(&mut self, bb: AsmBasicBlock) {
        self.blocks.push(Box::new(bb));
    }

    pub fn insert_inst(&mut self, bb_name: String, index: usize, inst: AsmInstruction) {
        let bb_name = if bb_name.eq("%entry") {
            BlockName::Entry
        } else {
            BlockName::Other(bb_name)
        };
        for bb in self.blocks.iter_mut() {
            if bb_name == bb.bb_name {
                bb.insert_inst(index, inst);
                break;
            }
        }
    }
}

#[derive(Default, Debug, PartialEq)]
pub enum BlockName {
    #[default]
    Entry,
    Other(String),
}

// The riscv instruction include the blockname except the "entry"
#[derive(Default, Debug)]
pub struct AsmBasicBlock {
    bb_name: BlockName,
    insts: Vec<Box<AsmInstruction>>,
}

impl AsmBasicBlock {
    pub fn new(bb_name: String) -> Self {
        let bb_name = if bb_name.eq("%entry") {
            BlockName::Entry
        } else {
            BlockName::Other(bb_name)
        };
        AsmBasicBlock {
            bb_name,
            ..Self::default()
        }
    }
    pub fn push_inst(&mut self, inst: AsmInstruction) {
        self.insts.push(Box::new(inst));
    }
    pub fn insert_inst(&mut self, index: usize, inst: AsmInstruction) {
        self.insts.insert(index, Box::new(inst));
    }
}

#[derive(Default, Debug)]
pub struct AsmInstruction {
    insts_str: Vec<String>,
}

impl AsmInstruction {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn push_inst(&mut self, inst_str: String) {
        self.insts_str.push(inst_str);
    }
}
