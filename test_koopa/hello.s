  .text
  .globl main
main:
  addi sp, sp, -24
  li t0, 20
  sw t0, 0(sp)
  li t0, 10
  sw t0, 0(sp)
  li t0, 2
  sw t0, 4(sp)
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw t0, 8(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 12(sp)
  lw t0, 12(sp)
  sw t0, 16(sp)
  lw t0, 0(sp)
  sw t0, 20(sp)
  lw a0, 20(sp)
  ret
