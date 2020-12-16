global _start
section .text
_start:
push 1
push 34
pop rcx
pop rax
imul rax, rcx
push rax
push 4
pop rcx
pop rax
imul rax, rcx
push rax
push 1
pop rcx
pop rax
imul rax, rcx
push rax
pop rdi
mov rax, 60
syscall
