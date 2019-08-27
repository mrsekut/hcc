.intel_syntax noprefix
.global hcc
hcc:
    push rbp
    mov rbp, rsp
    sub rsp, 208
    mov rax, rbp
    sub rax, 192
    push rax
    push 123
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    mov rax, rbp
    sub rax, 192
    push rax
    pop rax
    mov rax, [rax]
    push rax
    mov rsp, rbp
    pop rbp
    ret
