.intel_syntax noprefix
.global hcc
hcc:
    push rbp
    mov rbp, rsp
    sub rsp, 208
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
