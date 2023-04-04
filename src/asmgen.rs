use std::{fs::File, io::Write};

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    FunctionData, Program, ValueKind,
};

#[derive(Debug)]
enum Asm {
    ProgramAsm {
        func_list: Vec<crate::asmgen::Asm>,
    },
    FunctionAsm {
        func_name: String,
        bb_list: Vec<crate::asmgen::Asm>,
    },
    BasicBlockAsm {
        bb_name: String,
        inst_list: Vec<crate::asmgen::Asm>,
    },
    InstAsm {
        inst_strs: Vec<String>,
    },
    None,
}

trait GenerateAsm {
    fn generate(&self, func_data: Option<&FunctionData>) -> Asm;
}

impl GenerateAsm for Program {
    fn generate(&self, _: Option<&FunctionData>) -> Asm {
        let mut func_list = vec![];
        for (_, func_data) in self.funcs() {
            func_list.push(func_data.generate(None));
        }
        Asm::ProgramAsm { func_list }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, _: Option<&FunctionData>) -> Asm {
        let mut bb_list = vec![];
        //self.params() deal with params
        for (_, bb_data) in self.dfg().bbs() {
            bb_list.push(bb_data.generate(Some(&self)))
        }
        Asm::FunctionAsm {
            func_name: String::from(self.name()),
            bb_list: bb_list,
        }
    }
}

impl GenerateAsm for BasicBlockData {
    fn generate(&self, func_data: Option<&FunctionData>) -> Asm {
        let mut inst_list = vec![];
        let func_data = func_data.unwrap();
        for (_, value_data) in func_data.dfg().values() {
            let inst = value_data.generate(Some(func_data));
            match &inst {
                Asm::InstAsm { inst_strs:_ }=>inst_list.push(inst),
                _=>{}
            }

        }
        Asm::BasicBlockAsm {
            bb_name: String::from(self.name().as_ref().unwrap()),
            inst_list: inst_list,
        }
    }
}

impl GenerateAsm for ValueData {
    fn generate(&self, func_data: Option<&FunctionData>) -> Asm {
        match self.kind() {
            ValueKind::Integer(_int) => Asm::None,
            ValueKind::Return(ret) => {
                let mut inst_strs = vec![];
                let ret_value = ret.value().unwrap();
                let ret_value_data = func_data.unwrap().dfg().value(ret_value);
                match ret_value_data.kind() {
                    ValueKind::Integer(ret_int) => {
                        inst_strs.push(String::from(format!("li a0,{}", ret_int.value())));
                        inst_strs.push(String::from("ret"));
                    }
                    _ => {}
                }
                Asm::InstAsm {
                    inst_strs: inst_strs,
                }
                //println!("{:#?}",ret_value_data);
            }
            _ => unreachable!(),
        }
    }
}

pub fn gen_asm(program: &Program, output_name: &String) {
    let asm = program.generate(None);
    println!("{:#?}", asm);
    let mut output_file = File::create(output_name).unwrap();
    match asm {
        Asm::ProgramAsm { func_list } => {
            for func in func_list {
                match func {
                    Asm::FunctionAsm { func_name, bb_list } => {
                        output_file.write(format!("{}\n",func_name).as_bytes()).unwrap();
                        for bb in bb_list {
                            match bb {
                                Asm::BasicBlockAsm { bb_name, inst_list } => {
                                    //output_file.write(bb_name.as_bytes()).unwrap();
                                    for inst in inst_list {
                                        match inst {
                                            Asm::InstAsm { inst_strs } => {
                                                for inst_str in inst_strs {
                                                    output_file.write(format!("{}\n",inst_str).as_bytes()).unwrap();
                                                }
                                            }
                                            _ => panic!("Not a inst!"),
                                        }
                                    }
                                }
                                _ => panic!("Not a bb!"),
                            }
                        }
                    }
                    _ => panic!("Not a func!"),
                }
            }
        }
        _ => panic!("Not a program!"),
    }
}
