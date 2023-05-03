  .text
  .globl @fib
@fib:
  addi sp, sp, -48
  lw t0, 4(sp)
  sw t0, 8(sp)
  li t0, 0
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 12(sp)
  lw t0, 8(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 16(sp)
  lw t0, 12(sp)
  lw t1, 16(sp)
  and t0, t0, t1
  sw t0, 20(sp)
  lw t0, 20(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 24(sp)
  li t0, 1
  lw t1, 24(sp)
  or t0, t0, t1
  sw t0, 28(sp)
  lw t0, 28(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 32(sp)
  lw t0, 32(sp)
  li t1, 1
  or t0, t0, t1
  sw t0, 36(sp)
  lw t0, 36(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 40(sp)
  lw t0, 40(sp)
  beqz t0, bb1
  j bb0
bb0:
  li t0, 1
  sw t0, 0(sp)
  j end_bb
bb1:
  li t0, 0
  sw t0, 0(sp)
  j end_bb
end_bb:
  lw t0, 0(sp)
  sw t0, 44(sp)
  lw a0, 44(sp)
  addi sp, sp, 48
  ret
