global _start
section .text

main:
	sub rsp, 12
	mov qword [rsp+0], 42
	mov dword [rsp+8], 69
	mov rax, [rsp+8]
	add rsp, 12
	ret

_start:
	call main
	mov rdi, rax
	mov rax, 60
	syscall
