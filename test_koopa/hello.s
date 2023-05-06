  .text
  .globl main
main:
  addi sp, sp, -192
  li t0, 1
  sw t0, 4(sp)
  li t0, 0
  sw t0, 8(sp)
  li t0, 4
  sw t0, 12(sp)
  li t0, 5
  sw t0, 16(sp)
  lw t0, 4(sp)
  sw t0, 20(sp)
  lw t0, 20(sp)
  li t1, 1
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 24(sp)
  li t0, 1
  sw t0, 28(sp)
  lw t0, 24(sp)
  li t1, 0
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 32(sp)
  lw t0, 32(sp)
  beqz t0, bb1
  j bb0
end_bb:
  lw t0, 0(sp)
  sw t0, 36(sp)
  lw a0, 36(sp)
  addi sp, sp, 40
  ret
bb0:
  lw t0, 4(sp)
  sw t0, 40(sp)
  lw t0, 40(sp)
  li t1, 2
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 44(sp)
  lw t0, 44(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 48(sp)
  lw t0, 48(sp)
  sw t0, 28(sp)
  j bb1
bb1:
  lw t0, 28(sp)
  sw t0, 52(sp)
  lw t0, 52(sp)
  beqz t0, bb3
  j bb2
bb2:
  j bb3
bb3:
  lw t0, 8(sp)
  sw t0, 56(sp)
  lw t0, 56(sp)
  li t1, 0
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 60(sp)
  li t0, 1
  sw t0, 64(sp)
  lw t0, 60(sp)
  li t1, 0
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 68(sp)
  lw t0, 68(sp)
  beqz t0, bb5
  j bb4
bb4:
  lw t0, 8(sp)
  sw t0, 72(sp)
  lw t0, 72(sp)
  li t1, 1
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 76(sp)
  lw t0, 76(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 80(sp)
  lw t0, 80(sp)
  sw t0, 64(sp)
  j bb5
bb5:
  lw t0, 64(sp)
  sw t0, 84(sp)
  lw t0, 84(sp)
  beqz t0, bb7
  j bb6
bb6:
  lw t0, 16(sp)
  sw t0, 88(sp)
  lw t0, 88(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 92(sp)
  lw t0, 92(sp)
  sw t0, 16(sp)
  j bb8
bb7:
  j bb8
bb8:
  lw t0, 4(sp)
  sw t0, 96(sp)
  li t0, 0
  sw t0, 100(sp)
  lw t0, 96(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 104(sp)
  lw t0, 104(sp)
  beqz t0, bb10
  j bb9
bb9:
  lw t0, 8(sp)
  sw t0, 108(sp)
  lw t0, 108(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 112(sp)
  lw t0, 112(sp)
  sw t0, 100(sp)
  j bb10
bb10:
  lw t0, 100(sp)
  sw t0, 116(sp)
  li t0, 1
  sw t0, 120(sp)
  lw t0, 116(sp)
  li t1, 0
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 124(sp)
  lw t0, 124(sp)
  beqz t0, bb12
  j bb11
bb11:
  lw t0, 12(sp)
  sw t0, 128(sp)
  li t0, 0
  sw t0, 132(sp)
  lw t0, 128(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 136(sp)
  lw t0, 136(sp)
  beqz t0, bb14
  j bb13
bb12:
  lw t0, 120(sp)
  sw t0, 140(sp)
  lw t0, 140(sp)
  beqz t0, bb16
  j bb15
bb13:
  lw t0, 16(sp)
  sw t0, 144(sp)
  lw t0, 144(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 148(sp)
  lw t0, 148(sp)
  sw t0, 132(sp)
  j bb14
bb14:
  lw t0, 132(sp)
  sw t0, 152(sp)
  j bb12
bb15:
  lw t0, 16(sp)
  sw t0, 156(sp)
  lw t0, 156(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 160(sp)
  lw t0, 160(sp)
  sw t0, 16(sp)
  j bb16
bb16:
  lw t0, 16(sp)
  sw t0, 164(sp)
  li t0, 1
  sw t0, 168(sp)
  lw t0, 164(sp)
  li t1, 0
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 172(sp)
  lw t0, 172(sp)
  beqz t0, bb18
  j bb17
bb17:
  lw t0, 12(sp)
  sw t0, 176(sp)
  li t0, 0
  lw t1, 176(sp)
  xor t0, t0, t1
  seqz t0, t0
  sw t0, 180(sp)
  lw t0, 180(sp)
  li t1, 0
  xor t0, t0, t1
  snez t0, t0
  sw t0, 184(sp)
  lw t0, 184(sp)
  sw t0, 168(sp)
  j bb18
bb18:
  lw t0, 168(sp)
  sw t0, 188(sp)
  lw t0, 188(sp)
  sw t0, 0(sp)
  j end_bb
