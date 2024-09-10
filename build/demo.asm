global main
section .text

_print:
	;allocating stack
	sub rsp, 40
	;body
	;move output into the a reg
	;clean stack
	add rsp, 40
	ret

_add:
	;allocating stack
	sub rsp, 8
	;body
	;move output into the a reg
	mov eax, dword [rsp+16]
	mov ebx, dword [rsp+20]
	add rax, rbx
	;clean stack
	add rsp, 8
	ret

_main:
	;allocating stack
	sub rsp, 24
	;body
	;clear memory
	mov rax, rsp
	add rax, 0
	mov rdi, rax
	mov rcx, 10
	mov rax, 33
	rep stosb
	;calling into function
	;pushing parameters onto stack
	sub rsp, 16
	mov rax, rsp
	add rax, 16
	mov qword [rsp+8], 10
	mov qword [rsp+0], rax
	call _print
	add rsp, 16
	mov dword [rsp+4], 10
	mov dword [rsp+8], 2
	;move output into the a reg
	;calling into function
	;pushing parameters onto stack
	sub rsp, 16
	mov eax, dword [rsp+20]
	mov dword [rsp+0], eax
	mov eax, dword [rsp+24]
	mov dword [rsp+4], eax
	call _add
	add rsp, 16
	;clean stack
	add rsp, 24
	ret

main:
	call _main
	mov rdi, rax
	mov rax, 60
	syscall

section .data

message: db "Hello, World!"
