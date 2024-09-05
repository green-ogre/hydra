global _start
section .text

_start:
    push 10
    push 5
    call add

    mov rdi, rax  ; Move result to rdi for exit status
    mov rax, 60   ; syscall number for exit
    syscall


;push 10
;push 5
;call add

add:
    push rbp
    mov rbp, rsp

    mov rax, [rbp + 16]  ; Get first parameter
    add rax, [rbp + 24]  ; Add second parameter

    pop rbp
    ret 16
