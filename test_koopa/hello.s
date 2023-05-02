  .text
  .globl @fib
@fib:
  addi sp, sp, -16
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw t0, 8(sp)
  beqz t0, bb1
  j bb0
bb0:
  li t0, 1
  sw t0, 0(sp)
  j end
bb1:
  li t0, 0
  sw t0, 0(sp)
  j end
end:
  lw t0, 0(sp)
  sw t0, 12(sp)
  lw a0, 12(sp)
  addi sp, sp, 16
  ret
