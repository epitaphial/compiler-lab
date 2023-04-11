use std::{fs::File, io::Write};

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    FunctionData, Program, ValueKind,
};

mod asm {
    #[derive(Debug)]
    pub struct Program {
        pub sections: Vec<Section>,
        // other symbols...
    }

    #[derive(Debug)]
    pub enum Section {
        TextSection(Vec<Function>),
    }

    #[derive(Debug)]
    pub struct Function {
        pub func_name: String,
        pub blocks: Vec<BasicBlock>,
    }

    #[derive(Debug)]
    pub struct BasicBlock {
        pub block_name:Option<String>,
        pub insts:Vec<Instruction>,
    }

    pub type Instruction = Option<String>;
}

trait AsmGenerator<T> {
    fn generate_program(&self,func_data:Option<&FunctionData>) -> T;
}

impl AsmGenerator<asm::Program> for Program {
    fn generate_program(&self,_:Option<&FunctionData>) -> asm::Program {
        let mut prog_asm = asm::Program {
            sections: Vec::new(),
        };
        // deal with text section
        let mut funcs = Vec::new();
        for (_, func_data) in self.funcs() {
            funcs.push(func_data.generate_program(None));
        }
        let text_section = asm::Section::TextSection(funcs);
        //deal with data and more section...unimplemented
        // todo
        prog_asm.sections.extend([text_section]);
        prog_asm
    }
}

impl AsmGenerator<asm::Function> for FunctionData {
    fn generate_program(&self,_:Option<&FunctionData>) -> asm::Function {
        let mut func = asm::Function {
            func_name: self.name().into(),
            blocks: Vec::new(),
        };
        for (_,bb_data) in self.dfg().bbs(){
            func.blocks.push(bb_data.generate_program(Some(self)));
        }
        func
    }
}

impl AsmGenerator<asm::BasicBlock> for BasicBlockData {
    fn generate_program(&self,func_data:Option<&FunctionData>) -> asm::BasicBlock {
        let mut block = asm::BasicBlock{
            block_name:self.name().clone(),
            insts:Vec::new(),
        };
        for (_,value_data) in func_data.unwrap().dfg().values(){
            block.insts.push(value_data.generate_program(func_data))
        }
        block
    }
}

impl AsmGenerator<asm::Instruction> for ValueData {
    fn generate_program(&self,func_data:Option<&FunctionData>) -> asm::Instruction {
        println!("{:#?}",self);
        self.name().clone()
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
    let _asm = program.generate_program(None);
    //println!("{:#?}", asm);
    //let mut output_file = File::create(output_name).unwrap();
}
