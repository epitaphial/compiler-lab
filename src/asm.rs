use std::collections::HashMap;

use koopa::ir::Value;

#[derive(Default, Debug)]
pub struct AsmProgram {
    functions: Box<Vec<AsmFunction>>,
    // other symbols...
}

impl AsmProgram {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_func(&mut self, func: AsmFunction) {
        self.functions.push(func);
    }
}

#[derive(Debug, Default)]
pub enum FunctionName {
    #[default]
    Main,
    Other(String),
}

// For each fucntion, there must be a hashmap to store the values stack location.
#[derive(Default, Debug)]
pub struct AsmFunction {
    func_name: FunctionName,
    blocks: Box<Vec<AsmBasicBlock>>,
    stack: Box<HashMap<Value, u32>>,
    stack_size: u32,
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
    pub fn set_stack_size(& mut self,stack_size:u32){
        self.stack_size = stack_size;
    }



    pub fn push_bb(&mut self, bb: AsmBasicBlock) {
        self.blocks.push(bb);
    }

    pub fn insert_inst(&mut self, bb_name: String,index:usize, inst: AsmInstruction) {
        let bb_name = if bb_name.eq("%entry") {
            BlockName::Entry
        } else {
            BlockName::Other(bb_name)
        };
        for bb in self.blocks.iter_mut() {
            println!("{:#?}", bb.bb_name);
            if bb_name == bb.bb_name {
                bb.insert_inst(index,inst);
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
    insts: Box<Vec<AsmInstruction>>,
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
        self.insts.push(inst);
    }
    pub fn insert_inst(&mut self,index:usize, inst: AsmInstruction) {
        self.insts.insert(index, inst);
    }
}

#[derive(Default, Debug)]
pub struct AsmInstruction {
    inst_name: Option<String>,
}

impl AsmInstruction {
    pub fn new() -> Self {
        Self::default()
    }
}
