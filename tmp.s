.intel_syntax noprefix
.global hcc
hcc:
    push 12
    push 3
    pop rdi
    pop rax
    imul rdi
    push rax
    push 9
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rax
    ret
