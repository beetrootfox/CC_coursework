
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


.globl _fib
.align 4, 0x90
_fib:
.cfi_startproc
pushq %rbp
.cfi_def_cfa_offset 16
.cfi_offset %rbp, -16
movq %rsp, %rbp
.cfi_def_cfa_register %rbp
subq $24, %rsp
movq %rdi, -16(%rbp)
push $0
leaq -32(%rbp), %rax
push %rax
push $1
leaq -48(%rbp), %rax
push %rax
push $0
leaq -64(%rbp), %rax
push %rax
push $0
leaq -80(%rbp), %rax
push %rax
L1:
//offset 9
movq -88(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 0
movq -16(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rbx
pop %rax
cmpq %rbx, %rax
jge L3
L2:
//offset 7
movq -72(%rbp), %rax
push %rax
//offset 5
movq -56(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 5
movq -56(%rbp), %rax
push %rax
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
//offset 5
movq -56(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 3
movq -40(%rbp), %rax
push %rax
//offset 7
movq -72(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 9
movq -88(%rbp), %rax
push %rax
//offset 9
movq -88(%rbp), %rax
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
//offset 3
movq -40(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rdi
callq _printint
push $0
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
jmp L1
L3:
push $0
//offset 3
movq -40(%rbp), %rax
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
pop %rbx
pop %rcx
push %rax
pop %rax
pop %rbx
pop %rcx
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
push $13
leaq -24(%rbp), %rax
push %rax
//offset 2
movq -32(%rbp), %rax
push %rax
pop %rax
movq %rax, %rdi
callq _fib
push %rax
pop %rax
pop %rbx
pop %rcx
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

