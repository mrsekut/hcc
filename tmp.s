.intel_syntax noprefix
.global hcc
hcc:
    push rbp
    mov rbp, rsp
    sub rsp, 208
    push 3
    push 5
    pop rdi
    pop rax
    add rax, rdi
    push rax
    mov rsp, rbp
    pop rbp
    ret
