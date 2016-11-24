
.section        __TEXT,__text,regular,pure_instructions
.macosx_version_min 10, 11
.globl  _print
.align  4, 0x90
_print:                                 ## @print
.cfi_startproc
## BB#0:
pushq   %rbp
Ltmp0:
.cfi_def_cfa_offset 16
Ltmp1:
.cfi_offset %rbp, -16
movq    %rsp, %rbp
Ltmp2:
.cfi_def_cfa_register %rbp
subq    $16, %rsp
leaq    L_.str(%rip), %rax
movl    %edi, -4(%rbp)
movl    -4(%rbp), %esi
movq    %rax, %rdi
movb    $0, %al
callq   _printf
xorl    %edi, %edi
movl    %eax, -8(%rbp)          ## 4-byte Spill
callq   _exit
.cfi_endproc
.globl _power
.align 4, 0x90
_power:
.cfi_startproc
pushq %rbp
.cfi_def_cfa_offset 16
.cfi_offset %rbp, -16
movq %rsp, %rbp
.cfi_def_cfa_register %rbp
subq $32, %rsp
movq %rdi, -16(%rbp)
movq %rsi, -24(%rbp)
//offset 1
movq -24(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $1
pop %rbx
pop %rax
cmpq %rbx, %rax
je L2
L1:
//offset 1
movq -24(%rbp), %rax
push %rax
//offset 1
movq -24(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $1
pop %rax
pop %rbx
subq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 0
movq -16(%rbp), %rax
push %rax
//offset 0
movq -16(%rbp), %rax
push %rax
pop %rax
movq %rax, %rdi
//offset 1
movq -24(%rbp), %rax
push %rax
pop %rax
movq %rax, %rsi
callq _power
push %rax
pop %rax
pop %rbx
imulq %rax, %rbx
push %rbx
pop %rax
pop %rbx
push %rax
jmp L3
L2:
//offset 0
movq -16(%rbp), %rax
push %rax
L3:
pop %rax
addq $32, %rsp
popq %rbp
retq
.cfi_endproc

.globl  _main
.align  4, 0x90
_main:                                  ## @main
.cfi_startproc
## BB#0:
pushq   %rbp
Ltmp3:
.cfi_def_cfa_offset 16
Ltmp4:
.cfi_offset %rbp, -16
movq    %rsp, %rbp
Ltmp5:
.cfi_def_cfa_register %rbp
subq    $16, %rsp
movl    $260, %edi              ## imm = 0x104
movl    $0, -4(%rbp)
push $2
push $10
leaq -32(%rbp), %rax
push %rax
//offset 1
movq -24(%rbp), %rax
push %rax
pop %rax
movq %rax, %rdi
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rax
movq %rax, %rsi
callq _power
push %rax
//offset 4
movq -48(%rbp), %rax
push %rax
pop %rax
pop %rbx
push %rax
pop %rax
pop %rbx
pop %rcx
push %rax
pop %rax
pop %rbx
push %rax

pop     %rdi
callq   _print
movl    $1, %eax
addq    $16, %rsp
popq    %rbp
retq
.cfi_endproc

.section        __TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
.asciz  "%d
"


.subsections_via_symbols

