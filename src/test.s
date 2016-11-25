
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
push $5
//offset 1
movq -24(%rbp), %rax
push %rax
push $2
pop %rbx
pop %rax
cqto
idivq %rbx
push %rax
push $4
leaq -40(%rbp), %rax
push %rax
//offset 4
movq -48(%rbp), %rax
push %rax
leaq -56(%rbp), %rax
push %rax
//offset 6
movq -64(%rbp), %rax
push %rax
leaq -72(%rbp), %rax
push %rax
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $8
pop %rbx
pop %rax
movq %rbx, (%rax)
push %rbx
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $2
pop %rax
pop %rbx
subq %rax, %rbx
push %rbx
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $2
pop %rax
pop %rbx
addq %rax, %rbx
push %rbx
//offset 8
movq -80(%rbp), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
pop %rax
movq (%rax), %rax
push %rax
push $2
pop %rax
pop %rbx
imulq %rax, %rbx
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

