use core::panic;
use std::fs::File;
use std::io::Write;

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    layout::BasicBlockNode,
    BinaryOp, FunctionData, Program, TypeKind, Value, ValueKind,
};

use crate::asm::{
    ArgType, AsmBasicBlock, AsmFunction, AsmInstruction, AsmProgram, Layout, Register,
};

trait AsmGenerator {
    fn generate_program(&self) -> AsmProgram {
        panic!()
    }
    fn generate_function(&self, _: &Program) -> AsmFunction {
        panic!()
    }
    fn generate_basic_block(
        &self,
        _: &Program,
        _: &FunctionData,
        _: &BasicBlockNode,
        _: &mut Layout,
    ) -> AsmBasicBlock {
        panic!()
    }
    fn generate_instruction(
        &self,
        _: &Program,
        _: &FunctionData,
        _: &Value,
        _: &mut Layout,
    ) -> Vec<AsmInstruction> {
        panic!()
    }
    fn get_value(&self, _: &FunctionData, _: &Value, _: &mut Layout) -> AsmValue {
        panic!()
    }
}

impl AsmGenerator for Program {
    fn generate_program(&self) -> AsmProgram {
        let mut asm_prog = AsmProgram::new();
        for (_, func_data) in self.funcs() {
            if func_data.layout().entry_bb() != None{
                asm_prog.push_func(func_data.generate_function(self));
            }
        }
        asm_prog
    }
}

impl AsmGenerator for FunctionData {
    fn generate_function(&self, program: &Program) -> AsmFunction {
        let mut asm_func = AsmFunction::new(self.name().to_string());
        let mut layout = Layout::new();

        let mut alloc_stack_size = 0; // the amount space needed to alloc
        let mut local_var_size = 0; // the space for local vars
        let mut is_leaf_function = true; // if the function is not a leaf function, 4 byte space is need to save ra
        let mut args_stack_num = 0; // if amount of args are more than 8, we need to put the remain on the stack

        // calculate the stack size to alloc
        for (_bb, bb_node) in self.layout().bbs() {
            for (value, _) in bb_node.insts() {
                let value_data = self.dfg().value(*value);
                match value_data.ty().kind() {
                    TypeKind::Int32 => {
                        local_var_size += 4;
                        if let ValueKind::Call(call) = value_data.kind() {
                            is_leaf_function = false;
                            let callee_args_stack_num = call.args().len() as i32 - 8;
                            if callee_args_stack_num > args_stack_num {
                                args_stack_num = callee_args_stack_num;
                            }
                        }
                    }
                    TypeKind::Unit => {
                        if let ValueKind::Call(call) = value_data.kind() {
                            is_leaf_function = false;
                            let callee_args_stack_num = call.args().len() as i32 - 8;
                            if callee_args_stack_num > args_stack_num {
                                args_stack_num = callee_args_stack_num;
                            }
                        }
                    }
                    TypeKind::Pointer(_) => local_var_size += 4,
                    _ => {}
                }
            }
        }

        alloc_stack_size += local_var_size;
        if !is_leaf_function {
            alloc_stack_size += 4;
        }
        alloc_stack_size += 4 * args_stack_num;

        layout.stack.set_stack_size(alloc_stack_size as u32);
        layout.stack.set_stack_sp(alloc_stack_size as u32);

        // Generate function's prologue
        if layout.stack.get_stack_size() != 0 {
            let mut prologue: Vec<AsmInstruction> = vec![];

            // "addi sp, sp, -size"
            prologue.push(AsmInstruction::Addi(
                Register::StackPointer,
                Register::StackPointer,
                -(alloc_stack_size as i32),
            ));

            if !is_leaf_function {
                // "sw ra, size(sp)"
                prologue.push(AsmInstruction::Sw(
                    Register::ReturnAddress,
                    Register::StackPointer,
                    layout.stack.push_ra() as i32,
                ))
            }

            // need to save args
            let func_params = self.params();
            let mut index = 0;
            for func_param in func_params {
                if index < 8 {
                    layout
                        .arg_map
                        .insert(*func_param, ArgType::Register(Register::Arg(index)));
                } else {
                    let stack_pos = layout.stack.get_stack_size() + 4 * (index - 8);
                    layout
                        .arg_map
                        .insert(*func_param, ArgType::StackPos(stack_pos));
                }
                index += 1;
            }

            for (bb, bb_node) in self.layout().bbs() {
                let block_data = self.dfg().bb(*bb);
                asm_func.push_bb(block_data.generate_basic_block(
                    program,
                    self,
                    bb_node,
                    &mut layout,
                ));
            }

            asm_func.insert_prologue(prologue);
        } else {
            for (bb, bb_node) in self.layout().bbs() {
                let block_data = self.dfg().bb(*bb);
                asm_func.push_bb(block_data.generate_basic_block(
                    program,
                    self,
                    bb_node,
                    &mut layout,
                ));
            }
        }

        asm_func
    }
}

impl AsmGenerator for BasicBlockData {
    fn generate_basic_block(
        &self,
        program: &Program,
        func_data: &FunctionData,
        bb_node: &BasicBlockNode,
        layout: &mut Layout,
    ) -> AsmBasicBlock {
        let mut asm_bb = AsmBasicBlock::new(self.name().clone().unwrap());
        for (value, _) in bb_node.insts() {
            let asm_values = func_data
                .dfg()
                .value(*value)
                .generate_instruction(program, func_data, value, layout);
            for asm_value in asm_values {
                asm_bb.push_inst(asm_value);
            }
        }
        asm_bb
    }
}

#[derive(Debug)]
enum AsmValue {
    Literal(i32),
    StackPos(u32),
    Register(Register),
}

impl AsmGenerator for ValueData {
    fn get_value(&self, func_data: &FunctionData, value: &Value, layout: &mut Layout) -> AsmValue {
        let value_data = func_data.dfg().value(*value);
        match value_data.kind() {
            // Integer value return the lireral while others return the stack position.
            ValueKind::Integer(int_value) => AsmValue::Literal(int_value.value()),
            ValueKind::FuncArgRef(_func_arg) => {
                // find func arg in registers and stacks
                let arg = layout.arg_map.get(value).unwrap();
                match arg {
                    ArgType::Register(reg) => AsmValue::Register(*reg),
                    ArgType::StackPos(pos) => AsmValue::StackPos(*pos),
                }
            }
            _ => {
                // todo: register alloc
                let pos = layout.stack.get_pos_by_value(*value);
                AsmValue::StackPos(pos)
            }
        }
    }

    fn generate_instruction(
        &self,
        program: &Program,
        func_data: &FunctionData,
        value: &Value,
        layout: &mut Layout,
    ) -> Vec<AsmInstruction> {
        let mut asm_insts = vec![];
        match self.kind() {
            ValueKind::Binary(bin) => {
                let lhs_value = self.get_value(func_data, &bin.lhs(), layout);
                match lhs_value {
                    // li t0, imm
                    AsmValue::Literal(lit) => {
                        asm_insts.push(AsmInstruction::Li(Register::Temp(0), lit))
                    }
                    // lw t0, imm(sp)
                    AsmValue::StackPos(pos) => asm_insts.push(AsmInstruction::Lw(
                        Register::Temp(0),
                        Register::StackPointer,
                        pos as i32,
                    )),
                    _ => {
                        unimplemented!()
                    }
                }

                let rhs_value = self.get_value(func_data, &bin.rhs(), layout);
                match rhs_value {
                    // li t1, imm
                    AsmValue::Literal(lit) => {
                        asm_insts.push(AsmInstruction::Li(Register::Temp(1), lit))
                    }
                    // lw t1, imm(sp)
                    AsmValue::StackPos(pos) => asm_insts.push(AsmInstruction::Lw(
                        Register::Temp(1),
                        Register::StackPointer,
                        pos as i32,
                    )),
                    _ => {
                        unimplemented!()
                    }
                }

                match bin.op() {
                    BinaryOp::Add => {
                        // add t0, t0, t1
                        asm_insts.push(AsmInstruction::Add(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Sub => {
                        // sub t0, t0, t1
                        asm_insts.push(AsmInstruction::Sub(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Mul => {
                        // mul t0, t0, t1
                        asm_insts.push(AsmInstruction::Mul(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Div => {
                        // div t0, t0, t1
                        asm_insts.push(AsmInstruction::Div(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Mod => {
                        // rem t0, t0, t1
                        asm_insts.push(AsmInstruction::Rem(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::And => {
                        // and t0, t0, t1
                        asm_insts.push(AsmInstruction::And(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Or => {
                        // or t0, t0, t1
                        asm_insts.push(AsmInstruction::Or(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Eq => {
                        // xor t0, t0, t1
                        asm_insts.push(AsmInstruction::Xor(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                        // seqz t0, t0
                        asm_insts.push(AsmInstruction::Seqz(Register::Temp(0), Register::Temp(0)));
                    }
                    BinaryOp::NotEq => {
                        // xor t0, t0, t1
                        asm_insts.push(AsmInstruction::Xor(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                        // snez t0, t0
                        asm_insts.push(AsmInstruction::Snez(Register::Temp(0), Register::Temp(0)));
                    }
                    BinaryOp::Lt => {
                        // slt t0, t0, t1
                        asm_insts.push(AsmInstruction::Slt(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Le => {
                        // slt t0, t1, t0
                        asm_insts.push(AsmInstruction::Xor(
                            Register::Temp(0),
                            Register::Temp(1),
                            Register::Temp(0),
                        ));
                        // xori t0, t0, 1
                        asm_insts.push(AsmInstruction::Xori(
                            Register::Temp(0),
                            Register::Temp(0),
                            1,
                        ));
                    }
                    BinaryOp::Gt => {
                        // sgt t0, t0, t1
                        asm_insts.push(AsmInstruction::Sgt(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                    }
                    BinaryOp::Ge => {
                        // slt t0, t0, t1
                        asm_insts.push(AsmInstruction::Slt(
                            Register::Temp(0),
                            Register::Temp(0),
                            Register::Temp(1),
                        ));
                        // xori t0, t0, 1
                        asm_insts.push(AsmInstruction::Xori(
                            Register::Temp(0),
                            Register::Temp(0),
                            1,
                        ));
                    }
                    _ => {
                        unimplemented!()
                    }
                }
                // sw t0, imm(sp)
                asm_insts.push(AsmInstruction::Sw(
                    Register::Temp(0),
                    Register::StackPointer,
                    layout.stack.get_stack_sp() as i32,
                ));
                layout.stack.insert_value(*value, 4);
            }
            ValueKind::Return(ret) => {
                // if do not return void
                if let Some(return_value) = ret.value() {
                    let asm_ret_value = self.get_value(func_data, &return_value, layout);
                    match asm_ret_value {
                        AsmValue::Literal(lit) => {
                            // li a0, imm
                            asm_insts.push(AsmInstruction::Li(Register::Arg(0), lit));
                        }
                        AsmValue::StackPos(pos) => {
                            // lw a0, imm(sp)
                            asm_insts.push(AsmInstruction::Lw(
                                Register::Arg(0),
                                Register::StackPointer,
                                pos as i32,
                            ));
                        }
                        _ => {
                            unimplemented!()
                        }
                    }
                }

                // make epilogue
                // check if leaf function
                if let Some(ra_pos) = layout.stack.get_ra() {
                    // lw ra, imm(sp)
                    asm_insts.push(AsmInstruction::Lw(
                        Register::ReturnAddress,
                        Register::StackPointer,
                        ra_pos as i32,
                    ));
                }
                // restore stack size
                if layout.stack.get_stack_size() != 0 {
                    // addi sp, sp, imm
                    asm_insts.push(AsmInstruction::Addi(
                        Register::StackPointer,
                        Register::StackPointer,
                        layout.stack.get_stack_size() as i32,
                    ));
                }

                // ret
                asm_insts.push(AsmInstruction::Ret);
            }
            ValueKind::Load(load) => {
                let src_value = self.get_value(func_data, &load.src(), layout);
                if let AsmValue::StackPos(pos) = src_value {
                    // lw t0, imm(sp)
                    asm_insts.push(AsmInstruction::Lw(
                        Register::Temp(0),
                        Register::StackPointer,
                        pos as i32,
                    ));
                } else {
                    panic!("Can't load this type!")
                }
                // sw t0, imm(sp)
                asm_insts.push(AsmInstruction::Sw(
                    Register::Temp(0),
                    Register::StackPointer,
                    layout.stack.get_stack_sp() as i32,
                ));
                layout.stack.insert_value(*value, 4);
            }
            ValueKind::Store(store) => {
                let src_value = self.get_value(func_data, &store.value(), layout);
                let dest_value = self.get_value(func_data, &store.dest(), layout);
                match dest_value {
                    AsmValue::StackPos(dest_pos) => {
                        match src_value {
                            AsmValue::Literal(src_lit) => {
                                // li t0, imm
                                asm_insts.push(AsmInstruction::Li(Register::Temp(0), src_lit));
                                // sw t0, imm(sp)
                                asm_insts.push(AsmInstruction::Sw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    dest_pos as i32,
                                ));
                            }
                            AsmValue::StackPos(src_pos) => {
                                // lw t0, imm(sp)
                                asm_insts.push(AsmInstruction::Lw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    src_pos as i32,
                                ));
                                // sw t0, imm(sp)
                                asm_insts.push(AsmInstruction::Sw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    dest_pos as i32,
                                ));
                            }
                            AsmValue::Register(reg) => {
                                // sw a[0-7], imm(sp)
                                asm_insts.push(AsmInstruction::Sw(
                                    reg,
                                    Register::StackPointer,
                                    dest_pos as i32,
                                ));
                            }
                        }
                    }
                    AsmValue::Literal(_lit) => {
                        panic!("Cant be this!")
                    }
                    _ => {
                        unimplemented!()
                    }
                }
            }
            ValueKind::Jump(jump) => {
                let obj_bb_name = func_data.name()[1..].to_string()
                    + "."
                    + &func_data.dfg().bb(jump.target()).name().clone().unwrap()[1..];
                // j label
                asm_insts.push(AsmInstruction::J(obj_bb_name.to_string()));
            }
            ValueKind::Branch(branch) => {
                let cond_value = self.get_value(func_data, &branch.cond(), layout);
                let true_bb_name = func_data.name()[1..].to_string()
                    + "."
                    + &func_data.dfg().bb(branch.true_bb()).name().clone().unwrap()[1..];
                let false_bb_name = func_data.name()[1..].to_string()
                    + "."
                    + &func_data
                        .dfg()
                        .bb(branch.false_bb())
                        .name()
                        .clone()
                        .unwrap()[1..];
                match cond_value {
                    AsmValue::Literal(lit) => {
                        // li t0, imm
                        asm_insts.push(AsmInstruction::Li(Register::Temp(0), lit));
                        // beqz t0, label
                        asm_insts.push(AsmInstruction::Beqz(
                            Register::Temp(0),
                            false_bb_name.to_string(),
                        ));
                    }
                    AsmValue::StackPos(pos) => {
                        // lw t0, {}(sp)", pos
                        asm_insts.push(AsmInstruction::Lw(
                            Register::Temp(0),
                            Register::StackPointer,
                            pos as i32,
                        ));
                        // beqz t0, label
                        asm_insts.push(AsmInstruction::Beqz(
                            Register::Temp(0),
                            false_bb_name.to_string(),
                        ));
                    }
                    _ => unimplemented!(),
                }
                // j label
                asm_insts.push(AsmInstruction::J(true_bb_name.to_string()));
            }
            ValueKind::Alloc(_) => {
                layout.stack.insert_value(*value, 4);
            }
            ValueKind::Call(call) => {
                // deal with args
                let mut arg_index = 0;
                for arg in call.args() {
                    let value_data = func_data.dfg().value(*arg);
                    if arg_index < 8 {
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                // li a[0-7],imm
                                asm_insts.push(AsmInstruction::Li(
                                    Register::Arg(arg_index),
                                    int.value(),
                                ));
                            }
                            _ => {
                                let pos = layout.stack.get_pos_by_value(*arg);
                                // lw a[0-7],pos(sp)
                                asm_insts.push(AsmInstruction::Lw(
                                    Register::Arg(arg_index),
                                    Register::StackPointer,
                                    pos as i32,
                                ));
                            }
                        }
                    } else {
                        let arg_pos = 4 * (arg_index - 8);
                        match value_data.kind() {
                            ValueKind::Integer(int) => {
                                // li t0, imm
                                asm_insts.push(AsmInstruction::Li(Register::Temp(0), int.value()));

                                // sw t0, pos(sp)
                                asm_insts.push(AsmInstruction::Sw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    arg_pos as i32,
                                ));
                            }
                            _ => {
                                let pos = layout.stack.get_pos_by_value(*arg);
                                // lw t0,pos(sp)
                                asm_insts.push(AsmInstruction::Lw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    pos as i32,
                                ));

                                // sw t0, pos(sp)
                                asm_insts.push(AsmInstruction::Sw(
                                    Register::Temp(0),
                                    Register::StackPointer,
                                    arg_pos as i32,
                                ));
                            }
                        }
                    }
                    arg_index += 1;
                }
                let callee_func_data = program.func(call.callee());
                let callee_func_label = &callee_func_data.name()[1..];
                // call label
                asm_insts.push(AsmInstruction::Call(callee_func_label.to_string()));

                // save return value
                if !self.used_by().is_empty() {
                    // sw a0, (pos)sp
                    asm_insts.push(AsmInstruction::Sw(
                        Register::Arg(0),
                        Register::StackPointer,
                        layout.stack.get_stack_sp() as i32,
                    ));
                    layout.stack.insert_value(*value, 4);
                }
            }
            _ => {
                unimplemented!()
            }
        }
        asm_insts
    }
}

pub fn gen_asm(program: &Program, output_name: &String) {
    let asm = program.generate_program();
    let mut output_file = File::create(output_name).unwrap();
    write!(output_file, "{}", asm.gen_asm()).unwrap();
}
