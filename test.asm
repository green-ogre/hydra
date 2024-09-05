global _start
section .text

main:
	sub rsp, 4
	mov dword [rsp+0], 69
	mov rax, [rsp+0]
	add rsp, 4
	ret

_start:
	call main
	mov edi, eax
	mov rax, 60
	syscall
