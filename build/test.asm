global _start
section .text

main:
	;allocating stack
	push rbp
	mov rbp, rsp
	sub rsp, 16

	;body
	;calling into function
	call add
	;moving result into stack
	mov dword [rbp-0], eax

	;move output into the a reg
	mov eax, [rbp-0]

	;clean stack
	leave
	ret

add:
	;allocating stack
	push rbp
	mov rbp, rsp
	sub rsp, 16

	;body

	;move output into the a reg
	mov eax, 4

	;clean stack
	leave
	ret

_start:
	call main
	mov rdi, rax
	mov rax, 60
	syscall
