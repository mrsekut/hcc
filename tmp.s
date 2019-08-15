.intel_syntax noprefix
.global hcc
hcc:
    push 2
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    sete al
    movzb rax, al
    push rax
    pop rax
    ret
