.section .text
.global _start

_start:
    li      t0, 33
    li      t1, 10
    li      t2, 0
    beq     t1, t2, 



    sw      t0, 0(x0)
    #lb      t1, 0(x0)
    #sb      t1, 1(x0)
    #sb      t1, 2(x0)

    addi    a7, x0, 64         # write
    addi    a0, x0, 1          # to stdout
    addi    a1, x0, 0          # from buf
    addi    a2, x0, 10         # of len
    ecall

    addi    a7, x0, 93         # exit
    ecall
