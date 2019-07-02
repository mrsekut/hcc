.intel_syntax noprefix
.global main
main:
    push 20
    push 3
    push 9
    pop rdi
    pop rax
    imul rdi
    push rax
    push 0
    push 3
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    ret
