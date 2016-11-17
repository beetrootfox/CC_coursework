open Ast
open String
open Hashtbl
open Printf
open Evaluator


let sp = ref 0

let prefix = "
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
"

let postfix =
"
pop     %rdi
callq   _print
movl    $1, %eax
addq    $16, %rsp
popq    %rbp
retq
.cfi_endproc

.section        __TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
.asciz  \"%d\n\"


.subsections_via_symbols
"

let x86_str_of_operator = function
| Plus -> "addq"
| Minus -> "subq"
| Times -> "imulq"
| Divide -> "idivq"
| Leq -> "lt"
| Geq -> "gt"
| Equal -> "eq"
| Noteq -> "neq"
| And -> "and"
| Or -> "or"

module type Ais = sig
val op_ins : bin_opcode -> unit
val id_ins : int -> unit
val st_ins : int -> unit
val let_ins : unit -> unit
val new_ins : unit -> unit
val lea_ins : int -> unit
val deref_ins : unit -> unit
val asg_ins : unit -> unit
end

module Compiler86 (X86 : Ais) = struct

let rec codegen symt = function
| Asg (e1, e2)    ->
    codegen symt e1;
    codegen symt e2;
    X86.asg_ins ()
| Seq (e1, e2)    ->
    codegen symt e1;
    codegen symt e2;
    X86.let_ins ()
| While (e1, e2)  -> ()
    (*let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                let e = Seq(e2, While(e1, e2)) in
                eval_exp env e
                else Command
    | _      -> err_exit (err_string e1 v (Comp true)))*)
| For (x, e1, e2, e3) -> ()
    (*let v1 = eval_exp env e1 in
    let v2 = eval_exp env e2 in
    (match (v1,v2) with
    | (Constant n1, Constant n2) -> if n1 <= n2 then
                                    let e = Seq(e3, For(x, Const(n1 + 1), Const n2, e3)) in
                                    eval_exp ((x,v1)::env) e
                                    else Command
    | _                          -> err_exit "For loop boundaries must be integers!")*)
| If (e1, e2, e3) -> ()
    (*let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                eval_exp env e2
                else
                eval_exp env e3
    | _      -> err_exit (err_string e1 v (Comp true)))*)
| Deref e         ->
    codegen symt e;
    X86.deref_ins ()
| Bin_Operator (op, e1, e2) ->
    codegen symt e1;
    codegen symt e2;
    X86.op_ins op;
    sp := !sp - 1
| Unary_Operator (op, e) -> ()
   (* let addr = interpret symt e in
    INA.unary_op_ins op addr;
    stack_base := addr;
    INA.st_ins addr;
    addr*)
| Const n ->
    X86.st_ins n;
    sp := !sp + 1
| Identifier x ->
    let addr = (try lookup x symt with
                                | Not_found -> err_exit "Id lookup failuer") in
    X86.id_ins addr;
    sp := !sp + 1
| Empty -> ()
| Printint e -> ()
(*let sp = !stack_base in
                let addr = interpret symt e in
                INA.mv_ins addr wr;
                sp*)
| Let (x, e1, e2) ->
        codegen symt e1;
        codegen ((x, !sp) :: symt) e2;
        X86.let_ins ()
| New (x, e1, e2) ->
        codegen symt e1;
        X86.lea_ins !sp;
        sp := !sp + 1;
        codegen ((x, !sp) :: symt) e2;
        X86.new_ins ()
| Application (e1, e2) -> ()
        (*let f = eval_exp env e1 in
        (match f with
        | Id x -> let (args, exp) = (try Hashtbl.find fundefs x with
                                     | Not_found -> err_exit ("Function " ^ x ^ " was not defined!"))
                  in let env' = make_env x env env (args, e2) in
                  eval_exp env' exp
        | Func e -> (match e with
                     | Lambda (args, exp) -> let env' = make_env "lambda" env env (args, e2) in
                                             eval_exp env' exp
                     | _  -> err_exit (exp_to_string e ^ " must be a lambda function"))
        | _    -> err_exit (exp_to_string e1 ^ " is not a function!"))*)
| Lambda (args, e) -> () (*Func l*)
| Array_make (x,e1,e2,e3) -> ()
        (*let index = eval_exp env e1 in
        (match index with
         | Constant n -> let first = eval_exp env e2 in
                         let ar = Array.make n first in
                         Hashtbl.replace arrays x ar;
                         let v2 = eval_exp env e3 in Hashtbl.remove arrays x; v2
         | _           -> err_exit "Array length must be an integer!")*)
| Array_set (e1, e2, e3) -> ()
        (*let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialized!"))
                   in let index = eval_exp env e2 in
                   (match index with
                    | Constant n -> let v = eval_exp env e3 in
                                    (match (Array.get ar 0, v) with
                                    | (Constant _, Constant _) -> (try Array.set ar n v with
                                                                   | Invalid_argument s -> err_exit s); Command
                                    | (Id _, Id _)          ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Command, Command)    ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Comp _, Comp _)     ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Nothing, Nothing)   ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Reference _, Reference _) ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Func _, Func _)  -> (try Array.set ar n v with
                                                           | Invalid_argument s-> err_exit s); Command
                                    | _ -> err_exit "Type mismatch!")
                    | _           -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!") *)
| Array_get (e1, e2) -> ()
       (* let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialised!"))
                       in let index = eval_exp env e2 in
                       (match index with
                        | Constant n -> (try Array.get ar n with
                                         | Invalid_argument s -> err_exit "Array index out of bounds!")
                        | _          -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!")*)
| Readint -> ()
                (*let addr = inc_stp () in
             INA.mv_ins rr addr;
             addr*)
end
;;
