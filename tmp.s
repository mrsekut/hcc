.intel_syntax noprefix
.global hcc
hcc:
    push rbp
    mov rbp, rsp
    sub rsp, 208
    push 1
    push 2
    pop rdi
    pop rax
    imul rdi
    push rax
    push 2
    push 3
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    cmp rax, rdi
    setge al
    movzb rax, al
    push rax
    mov rsp, rbp
    pop rbp
    ret
