  .data
  .globl global_init
global_init:
  .word 1
  .data
  .globl global_x
global_x:
  .zero 4
  .text
  .globl main
main:
  addi sp, sp, -20
  sw ra, 20(sp)
  la t0, global_x
  lw t0, 0(t0)
  sw t0, 12(sp)
  lw a0, 12(sp)
  call putint
  li a0, 32
  call putch
  li a0, 10
  call putint
  li a0, 32
  call putch
  li a0, 11
  call putint
  li a0, 32
  call putch
  la t0, global_init
  lw t0, 0(t0)
  sw t0, 8(sp)
  lw a0, 8(sp)
  call putint
  li a0, 10
  call putch
  li t0, 0
  sw t0, 16(sp)
  j main.end_bb
main.end_bb:
  lw t0, 16(sp)
  sw t0, 4(sp)
  lw a0, 4(sp)
  lw ra, 20(sp)
  addi sp, sp, 20
  ret

