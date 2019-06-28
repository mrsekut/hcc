.intel_syntax noprefix
.global main
main:
    push 30
    push 0
    push 20
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
