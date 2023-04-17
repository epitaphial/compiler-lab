use std::{collections::HashMap, fs::File, io::Write};

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    layout::BasicBlockNode,
    FunctionData, Program, Type, TypeKind, Value, ValueKind,
};

use crate::asm::{self, AsmBasicBlock, AsmFunction, AsmInstruction, AsmProgram, BlockName};

//static mut VALUE_REG_MAP:HashMap<Value,String> = HashMap::new();

trait AsmGenerator {
    fn generate_program(&self) -> AsmProgram {
        panic!()
    }
    fn generate_function(&self) -> AsmFunction {
        panic!()
    }
    fn generate_basic_block(
        &self,
        func_data: &FunctionData,
        bb_node: &BasicBlockNode,
    ) -> AsmBasicBlock {
        panic!()
    }
    fn generate_instruction(&self) -> AsmInstruction {
        panic!()
    }
}

impl AsmGenerator for Program {
    fn generate_program(&self) -> AsmProgram {
        let mut asm_prog = AsmProgram::new();
        for (_, func_data) in self.funcs() {
            asm_prog.push_func(func_data.generate_function());
        }
        asm_prog
    }
}

impl AsmGenerator for FunctionData {
    fn generate_function(&self) -> AsmFunction {
        let mut asm_func = AsmFunction::new(self.name().to_string());
        let mut sp_size = 0;
        for (bb, bb_node) in self.layout().bbs() {
            for (value, _) in bb_node.insts() {
                let value_data = self.dfg().value(*value);
                match value_data.ty().kind() {
                    TypeKind::Int32=>{sp_size+=4}
                    TypeKind::Unit=>{}
                    _ => {}
                }
            }
            let block_data = self.dfg().bb(*bb);
            asm_func.push_bb(block_data.generate_basic_block(self, bb_node));
        }
        asm_func
    }
}

impl AsmGenerator for BasicBlockData {
    fn generate_basic_block(
        &self,
        func_data: &FunctionData,
        bb_node: &BasicBlockNode,
    ) -> AsmBasicBlock {
        let mut asm_bb = AsmBasicBlock::new(self.name().clone().unwrap().to_string());
        for (value, _) in bb_node.insts() {
            let asm_value = func_data.dfg().value(*value).generate_instruction();
            asm_bb.push_inst(asm_value);
        }
        asm_bb
    }
}

impl AsmGenerator for ValueData {
    fn generate_instruction(&self) -> AsmInstruction {
        let mut asm_inst = AsmInstruction::new();
        //asm_inst.inst_name = Some(format!("{:#?}",self));
        //println!("{:#?}",self);
        asm_inst
    }
}

// impl GenerateAsm for ValueData {
//     fn generate(&self, func_data: Option<&FunctionData>) -> Asm {
//         match self.kind() {
//             ValueKind::Integer(_int) => Asm::None,
//             ValueKind::Return(ret) => {
//                 let mut inst_strs = vec![];
//                 let ret_value = ret.value().unwrap();
//                 let ret_value_data = func_data.unwrap().dfg().value(ret_value);
//                 match ret_value_data.kind() {
//                     ValueKind::Integer(ret_int) => {
//                         inst_strs.push(String::from(format!("li a0,{}", ret_int.value())));
//                         inst_strs.push(String::from("ret"));
//                     }
//                     _ => {}
//                 }
//                 Asm::InstAsm {
//                     inst_strs: inst_strs,
//                 }
//                 //println!("{:#?}",ret_value_data);
//             }
//             _ => unreachable!(),
//         }
//     }
// }

pub fn gen_asm(program: &Program, _output_name: &String) {
    let asm = program.generate_program();
    println!("{:#?}", asm);
    //let mut output_file = File::create(output_name).unwrap();
}
