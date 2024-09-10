.section .text
.global _start

_start:
    li      t0, 33          # char
    li      t1, 10          # arr_len
    li      t2, 0           # i
    call    _build_arr


_build_arr:
    sw      t0, 0(t2)                   # store char in arr
    beq     t1, t2, _print_and_exit     # print and exit if i == arr_len
    addi    t2, t2, 1                   # i++
    j       _build_arr


_print_and_exit:
    addi    a7, x0, 64         # write
    addi    a0, x0, 1          # to stdout
    addi    a1, x0, 0          # from buf
    addi    a2, x0, 10         # of len
    ecall

    li      a0, 0
    addi    a7, x0, 93         # exit
    ecall
