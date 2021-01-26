global _start
section .text
_start:
push rbp
mov rbp, rsp
sub rsp, 24
push 2
push 1
pop rcx
pop rax
add rax, rcx
push rax
pop rax
mov [rbp - 0], rax
mov rax, [rbp - 0]
push rax
push 3
pop rcx
pop rax
sub rax, rcx
push rax
pop rax
mov [rbp - 8], rax
push 1
push 3
pop rcx
pop rax
imul rax, rcx
push rax
mov rax, [rbp - 8]
push rax
mov rax, [rbp - 0]
push rax
pop rcx
pop rax
add rax, rcx
push rax
pop rcx
pop rax
add rax, rcx
push rax
pop rax
mov [rbp - 16], rax
mov rax, [rbp - 16]
push rax
pop rax
mov rsp, rbp
pop rbp
ret
