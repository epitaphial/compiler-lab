use core::panic;
use std::fs::File;
use std::io::Write;

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    layout::BasicBlockNode,
    BinaryOp, FunctionData, Program, TypeKind, Value, ValueKind,
};

use crate::asm::{AsmBasicBlock, AsmFunction, AsmInstruction, AsmProgram, Stack};

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
        _: &FunctionData,
        _: &BasicBlockNode,
        _: &mut Stack,
    ) -> AsmBasicBlock {
        panic!()
    }
    fn generate_instruction(&self, _: &FunctionData, _: &Value, _: &mut Stack) -> AsmInstruction {
        panic!()
    }
    fn get_value(&self, _: &FunctionData, _: &Value, _: &Stack) -> AsmValue {
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
        let mut stack = Stack::new();
        let mut stack_size = 0;
        for (bb, bb_node) in self.layout().bbs() {
            for (value, _) in bb_node.insts() {
                let value_data = self.dfg().value(*value);
                match value_data.ty().kind() {
                    TypeKind::Int32 => stack_size += 4,
                    TypeKind::Unit => {}
                    TypeKind::Pointer(_) => stack_size += 4,
                    _ => {}
                }
            }
            stack.set_stack_size(stack_size);
            let block_data = self.dfg().bb(*bb);
            asm_func.push_bb(block_data.generate_basic_block(self, bb_node, &mut stack));
        }

        // Generate function's prologue
        if stack.get_stack_size() != 0 {
            let mut prologue = AsmInstruction::new();
            prologue.push_inst(format!("addi sp, sp, -{}", stack.get_stack_size()));
            asm_func.insert_inst("%entry".to_string(), 0, prologue);
        }

        asm_func
    }
}

impl AsmGenerator for BasicBlockData {
    fn generate_basic_block(
        &self,
        func_data: &FunctionData,
        bb_node: &BasicBlockNode,
        stack: &mut Stack,
    ) -> AsmBasicBlock {
        let mut asm_bb = AsmBasicBlock::new(self.name().clone().unwrap().to_string());
        for (value, _) in bb_node.insts() {
            let asm_value = func_data
                .dfg()
                .value(*value)
                .generate_instruction(func_data, value, stack);
            asm_bb.push_inst(asm_value);
        }
        asm_bb
    }
}

#[derive(Debug)]
enum AsmValue {
    Literal(i32),
    StackPos(u32),
}

impl AsmGenerator for ValueData {
    fn get_value(&self, func_data: &FunctionData, value: &Value, stack: &Stack) -> AsmValue {
        let value_data = func_data.dfg().value(*value);
        match value_data.kind() {
            // Integer value return the lireral while others return the stack position.
            ValueKind::Integer(int_value) => AsmValue::Literal(int_value.value()),
            _ => {
                let pos = stack.get_pos_by_value(*value);
                AsmValue::StackPos(pos)
            }
        }
    }

    fn generate_instruction(
        &self,
        func_data: &FunctionData,
        value: &Value,
        stack: &mut Stack,
    ) -> AsmInstruction {
        let mut asm_inst = AsmInstruction::new();
        //println!("{:#?}",stack.get_stack_size());
        match self.kind() {
            ValueKind::Binary(bin) => {
                let lhs_value = self.get_value(func_data, &bin.lhs(), stack);
                match lhs_value {
                    AsmValue::Literal(lit) => asm_inst.push_inst(format!("li t0, {}", lit)),
                    AsmValue::StackPos(pos) => asm_inst.push_inst(format!("lw t0, {}(sp)", pos)),
                }

                let rhs_value = self.get_value(func_data, &bin.rhs(), stack);
                match rhs_value {
                    AsmValue::Literal(lit) => asm_inst.push_inst(format!("li t1, {}", lit)),
                    AsmValue::StackPos(pos) => asm_inst.push_inst(format!("lw t1, {}(sp)", pos)),
                }
                match bin.op() {
                    BinaryOp::Add => {
                        asm_inst.push_inst(format!("add t0, t0, t1"));
                    }
                    BinaryOp::Sub => {
                        asm_inst.push_inst(format!("sub t0, t0, t1"));
                    }
                    BinaryOp::Mul => {
                        asm_inst.push_inst(format!("mul t0, t0, t1"));
                    }
                    BinaryOp::Div => {
                        asm_inst.push_inst(format!("div t0, t0, t1"));
                    }
                    BinaryOp::Mod => {
                        asm_inst.push_inst(format!("rem t0, t0, t1"));
                    }
                    BinaryOp::And => {
                        asm_inst.push_inst(format!("and t0, t0, t1"));
                    }
                    BinaryOp::Or => {
                        asm_inst.push_inst(format!("or t0, t0, t1"));
                    }
                    BinaryOp::Eq => {
                        asm_inst.push_inst(format!("xor t0, t0, t1"));
                        asm_inst.push_inst(format!("seqz t0, t0"));
                    }
                    BinaryOp::NotEq => {
                        asm_inst.push_inst(format!("xor t0, t0, t1"));
                        asm_inst.push_inst(format!("snez t0, t0"));
                    }
                    BinaryOp::Lt => {
                        asm_inst.push_inst(format!("slt t0, t0, t1"));
                    }
                    BinaryOp::Le => {
                        asm_inst.push_inst(format!("slt t0, t1, t0"));
                        asm_inst.push_inst(format!("xori t0, t0, 1"));
                    }
                    BinaryOp::Gt => {
                        asm_inst.push_inst(format!("sgt t0, t0, t1"));
                    }
                    BinaryOp::Ge => {
                        asm_inst.push_inst(format!("slt t0, t0, t1"));
                        asm_inst.push_inst(format!("xori t0, t0, 1"));
                    }
                    _ => {
                        unimplemented!()
                    }
                }
                asm_inst.push_inst(format!("sw t0, {}(sp)", stack.get_stack_sp()));
                stack.insert_value(*value, 4);
            }
            ValueKind::Return(ret) => {
                let ret_value = self.get_value(func_data, &ret.value().unwrap(), stack);
                match ret_value {
                    AsmValue::Literal(lit) => {
                        asm_inst.push_inst(format!("li a0, {}", lit));
                    }
                    AsmValue::StackPos(pos) => {
                        asm_inst.push_inst(format!("lw a0, {}(sp)", pos));
                    }
                }
                asm_inst.push_inst(format!("ret"));
            }
            ValueKind::Load(load) => {
                let src_value = self.get_value(func_data, &load.src(), stack);
                if let AsmValue::StackPos(pos) = src_value {
                    asm_inst.push_inst(format!("lw t0, {}(sp)", pos));
                } else {
                    panic!("Can't load this type!")
                }
                asm_inst.push_inst(format!("sw t0, {}(sp)", stack.get_stack_sp()));
                stack.insert_value(*value, 4);
            }
            ValueKind::Store(store) => {
                let dest_value = self.get_value(func_data, &store.dest(), stack);
                let src_value = self.get_value(func_data, &store.value(), stack);
                match dest_value {
                    AsmValue::StackPos(dest_pos) => {
                        match src_value {
                            AsmValue::Literal(src_lit) => {
                                asm_inst.push_inst(format!("li t0, {}", src_lit));
                            }
                            AsmValue::StackPos(src_pos) => {
                                asm_inst.push_inst(format!("lw t0, {}(sp)", src_pos));
                            }
                        }
                        asm_inst.push_inst(format!("sw t0, {}(sp)", dest_pos));
                    }
                    AsmValue::Literal(_lit) => {
                        panic!("Cant be this!")
                    }
                }
            }
            ValueKind::Alloc(_) => {
                stack.insert_value(*value, 4);
            }
            _ => {
                unimplemented!()
            }
        }
        asm_inst
    }
}

pub fn gen_asm(program: &Program, output_name: &String) {
    let asm = program.generate_program();
    let mut output_file = File::create(output_name).unwrap();
    write!(output_file, "{}", asm.gen_asm()).unwrap();
}
