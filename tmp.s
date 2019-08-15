.intel_syntax noprefix
.global main
main:
    push 1
    push 2
    push 3
    pop rdi
    pop rax
    imul rdi
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    ret
