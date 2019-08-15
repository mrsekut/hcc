.intel_syntax noprefix
.global hcc
hcc:
    push 12
    push 5
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    ret
