
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

.globl  _printint
.align  4, 0x90
_printint:                              ## @printint
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
leaq    L_.str(%rip), %rax
movl    %edi, -4(%rbp)
movl    -4(%rbp), %esi
movq    %rax, %rdi
movb    $0, %al
callq   _printf
movl    %eax, -8(%rbp)          ## 4-byte Spill
addq    $16, %rsp
popq    %rbp
retq
.cfi_endproc


.globl _convolve
.align 4, 0x90
_convolve:
.cfi_startproc
pushq %rbp
.cfi_def_cfa_offset 16
.cfi_offset %rbp, -16
movq %rsp, %rbp
.cfi_def_cfa_register %rbp
subq $48, %rsp
movq %rdi, -16(%rbp)
movq %rsi, -24(%rbp)
movq %rdx, -32(%rbp)
movq %rcx, -40(%rbp)
push $0
leaq -56(%rbp), %rax
push %rax
push $0
leaq -72(%rbp), %rax
push %rax
L1:
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 2
movq -32(%rbp), %rax
push %rax
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rbx
pop %rax
cmpq %rbx, %rax
jge L3
L2:
push $0
leaq -88(%rbp), %rax
push %rax
push $0
leaq -104(%rbp), %rax
push %rax
push $0
leaq -120(%rbp), %rax
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rbx
pop %rax
cmpq %rbx, %rax
jle L5
L4:
//offset 10
movq -96(%rbp), %rax
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rax
pop %rbx
subq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
jmp L6
L5:
//offset 10
movq -96(%rbp), %rax
push %rax
push $0
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
L6:
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 2
movq -32(%rbp), %rax
push %rax
pop %rbx
pop %rax
cmpq %rbx, %rax
jge L8
L7:
//offset 12
movq -112(%rbp), %rax
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 2
movq -32(%rbp), %rax
push %rax
pop %rax
pop %rbx
subq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
jmp L9
L8:
//offset 12
movq -112(%rbp), %rax
push %rax
//offset 2
movq -32(%rbp), %rax
push %rax
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
L9:
//offset 14
movq -128(%rbp), %rax
push %rax
//offset 10
movq -96(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
L10:
//offset 14
movq -128(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 12
movq -112(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rbx
pop %rax
cmpq %rbx, %rax
jge L12
L11:
//offset 8
movq -80(%rbp), %rax
push %rax
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 14
movq -128(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq %rax, %rdi
callq *-16(%rbp)
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 14
movq -128(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
pop %rbx
subq %rax, %rbx
push %rbx
pop %rax
movq %rax, %rdi
callq *-24(%rbp)
push %rax
pop %rax
pop %rbx
imulq %rax, %rbx
push %rbx
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 14
movq -128(%rbp), %rax
push %rax
//offset 14
movq -128(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $1
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
pop %rax
pop %rbx
push %rax
pop %rax
jmp L10
L12:
push $0
//offset 6
movq -64(%rbp), %rax
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $1
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
pop %rax
pop %rbx
push %rax
pop %rax
pop %rbx
push %rax
pop %rax
pop %rbx
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
pop %rcx
push %rax
pop %rax
pop %rbx
pop %rcx
push %rax
pop %rax
jmp L1
L3:
push $0
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
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
pop %rcx
push %rax
pop %rax
addq $48, %rsp
popq %rbp
retq
.cfi_endproc
.globl _fun1
.align 4, 0x90
_fun1:
.cfi_startproc
pushq %rbp
.cfi_def_cfa_offset 16
.cfi_offset %rbp, -16
movq %rsp, %rbp
.cfi_def_cfa_register %rbp
subq $24, %rsp
movq %rdi, -16(%rbp)
//offset 0
movq -16(%rbp), %rax
push %rax
//offset 0
movq -16(%rbp), %rax
push %rax
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rax
addq $24, %rsp
popq %rbp
retq
.cfi_endproc
.globl _fun2
.align 4, 0x90
_fun2:
.cfi_startproc
pushq %rbp
.cfi_def_cfa_offset 16
.cfi_offset %rbp, -16
movq %rsp, %rbp
.cfi_def_cfa_register %rbp
subq $24, %rsp
movq %rdi, -16(%rbp)
//offset 0
movq -16(%rbp), %rax
push %rax
push $10
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
push $2
pop %rbx
pop %rax
cqto
idivq %rbx
push %rax
pop %rax
addq $24, %rsp
popq %rbp
retq
.cfi_endproc

.globl  _main
.align  4, 0x90
_main:                                  ## @main
.cfi_startproc
## BB#0:
pushq   %rbp
Ltmp6:
.cfi_def_cfa_offset 16
Ltmp7:
.cfi_offset %rbp, -16
movq    %rsp, %rbp
Ltmp8:
.cfi_def_cfa_register %rbp
subq    $16, %rsp
movl    $260, %edi              ## imm = 0x104
movl    $0, -4(%rbp)
leaq _fun2(%rip), %rax
push %rax
pop %rax
movq %rax, %rdi
leaq _fun1(%rip), %rax
push %rax
pop %rax
movq %rax, %rsi
push $13
pop %rax
movq %rax, %rdx
push $10
pop %rax
movq %rax, %rcx
callq _convolve
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

