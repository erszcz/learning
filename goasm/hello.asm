; Based on hello.asm from nasm

    SECTION .data       ; data section
msg:    db "Hello World",10 ; the string to print, 10=cr
len:    equ $-msg       ; "$" means "here"
                ; len is a value, not an address

    SECTION .text       ; code section

global main.hello        ; make label available to linker (Go)
main.hello:

    ; --- setup stack frame
    push rbp            ; save old base pointer
    mov rbp,rsp   ; use stack pointer as new base pointer

    ; --- print message
    mov edx,len     ; arg3, length of string to print
    mov ecx,msg     ; arg2, pointer to string
    mov ebx,1       ; arg1, where to write, screen
    mov eax,4       ; write sysout command to int 80 hex
    int 0x80        ; interrupt 80 hex, call kernel

    ; --- takedown stack frame
    mov rsp,rbp  ; use base pointer as new stack pointer
    pop rbp      ; get the old base pointer

    ; --- return
    mov rax,0       ; error code 0, normal, no error
    ret         ; return
