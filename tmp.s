.intel_syntax noprefix
.global main
main:
    push 3
    push 4
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 3
    push 5
    push 4
    pop rdi
    pop rax
    cqo
    idiv rdi
    push rax
    pop rdi
    pop rax
    imul rdi
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rax
    ret
