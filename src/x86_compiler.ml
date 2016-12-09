open Ast
open String
open Hashtbl
open Printf
open Evaluator

let clb = ref ""

let functions = ref []

let sp = ref 0

let isp = ref 0

let lnum = ref 0

let apprhs = ref 0

let getlbl () = lnum := !lnum + 1; "L" ^ (string_of_int !lnum)

let aregs = ["rdi";"rsi";"rdx";"rcx";"rbx";"rax"]

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


"

let prefix2 = "
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
val label   : string -> unit
val jmp    : string -> unit
val cmp_ins : string * string *int -> bin_opcode -> unit
val pop_ins : unit -> unit
val push_ins : unit -> unit
val funlabel : string -> unit
val reserve : int -> unit
val leave : int -> unit
val ldarg : string -> unit
val svarg : string -> int -> unit
val handle_ret : unit -> unit
val call : string -> unit
val svstackarg : int -> int -> unit
val cut_stack : int -> unit
val callfptr : int -> unit
val pushfptr : string -> unit
val mkarray : unit -> unit
val array_gc : unit -> unit
val print : unit -> unit
end

module Compiler86 (X86 : Ais) = struct

let rec isFunc fname = function
| [] -> false
| x::xs -> if x = fname then true else isFunc fname xs

let rec prepargs symt e2 aregs = (match (e2, aregs) with
| ([], _) -> ()
| (a::args, [])      ->
        codegen symt [] a;
        prepargs symt args []
| (a::args, r::areg) ->
        codegen symt [] a;
        X86.ldarg r;
        sp := !sp - 1;
        prepargs symt args areg)

and codegen symt control = function
| Break ->
    X86.jmp !clb
| Asg (e1, e2)    ->
    codegen symt control e1;
    codegen symt control e2;
    X86.asg_ins ()
| Seq (e1, e2)    ->
    codegen symt control e1;
    codegen symt control e2;
    X86.let_ins ();
    sp := !sp - 1
| While (e1, e2)  ->
    let l1 = getlbl () in
    let l2 = getlbl () in
    let l3 = getlbl () in
    X86.label l1;
    codegen symt ((l2,l3,-2)::control) e1;
    X86.label l2;
    clb := l3;
    codegen symt control e2;
    X86.pop_ins ();
    X86.jmp l1;
    X86.label l3;
    X86.push_ins ()
| For (x, e1, e2, e3) ->
    let l1 = getlbl () in
    let l2 = getlbl () in
    let l3 = getlbl () in
    codegen symt control e1;
    X86.lea_ins !sp;
    sp := !sp + 1;
    X86.label l1;
    let addr = !sp in (
    codegen ((x, addr) :: symt) ((l2,l3,-2)::control) (Bin_Operator (Leq, (Deref (Identifier x)), e2));
    X86.label l2;
    clb := l3;
    codegen ((x, addr) :: symt) control e3;
    codegen ((x, addr) :: symt) control (Asg ((Identifier x), (Bin_Operator (Plus, (Deref (Identifier x)), Const 1))));
    X86.pop_ins ();
    X86.pop_ins ();
    X86.jmp l1;
    X86.label l3;
    X86.pop_ins ();
    X86.pop_ins ();
    X86.push_ins ())
| If (e1, e2, e3) ->
    (match e3 with
        | Empty ->
            let l1 = getlbl () in
            let l2 = getlbl () in
            codegen symt ((l1,l2,-2)::control) e1;
            X86.label l1;
            codegen symt control e2;
            X86.label l2
        | _     ->
            let l1 = getlbl() in
            let l2 = getlbl() in
            let l3 = getlbl() in
            codegen symt ((l1,l2,-2)::control) e1;
            X86.label l1;
            codegen symt control e2;
           (* X86.pop_ins (); *)
            X86.jmp l3;
            X86.label l2;
            codegen symt control e3;
           (* X86.pop_ins (); *)
            X86.label l3;
            sp := !sp - 1
           (* X86.push_ins () *))
| Deref e         ->
    codegen symt control e;
    X86.deref_ins ()
| Bin_Operator (op, e1, e2) ->
        (match op with
        | And ->
                  (match e1 with
                  | Bin_Operator (Or, _, _) ->
                        let l1' = getlbl () in
                        let (l1, l2, direction) = List.hd control in
                        codegen symt ((l1',l2,1)::(List.tl control)) e1;
                        X86.label l1'
                  | _ ->
                        codegen symt control e1);
                  codegen symt control e2
        | Or  ->
                  (match e1 with
                  | Bin_Operator (And, _, _) ->
                        let l2' = getlbl () in
                        let (l1, l2, direction) = List.hd control in
                        codegen symt ((l1, l2', -2)::(l1, l2', 1)::(List.tl control)) e1;
                        X86.label l2'
                  | _ ->
                        codegen symt control e1);
                    codegen symt control e2;
        | Plus
        | Minus
        | Times
        | Divide ->
                    codegen symt control e1;
                    codegen symt control e2;
                    X86.op_ins op;
                    sp := !sp - 1
        | Leq
        | Geq
        | Equal
        | Noteq ->
                    codegen symt control e1;
                    codegen symt control e2;
                    X86.cmp_ins (List.hd control) op;
                    sp := !sp - 2)
| Unary_Operator (op, e) -> err_exit "Booleans are not supported!"
| Const n ->
    X86.st_ins n;
    sp := !sp + 1
| Identifier x ->
    let addr = (try lookup x symt with
                                | Not_found -> -1) in
    if addr != -1 then (
        if x.[0] = '&' then
        X86.callfptr addr else (
        X86.id_ins addr;
        sp := !sp + 1))
    else (
        if isFunc x !functions then (
            if !apprhs = 1 then (
                X86.pushfptr x;
                sp := !sp + 1) else
                X86.call x) else
        err_exit (x ^ "is not defined!"))
| Empty -> ()
| Printint e ->
        codegen symt control e;
        X86.print ()
(*let sp = !stack_base in
                let addr = interpret symt e in
                INA.mv_ins addr wr;
                sp*)
| Let (x, e1, e2) ->
        codegen symt control e1;
        codegen ((x, !sp) :: symt) control e2;
        X86.let_ins ()
| New (x, e1, e2) ->
        codegen symt control e1;
        X86.lea_ins !sp;
        sp := !sp + 1;
        codegen ((x, !sp) :: symt) control e2;
        X86.new_ins ()
| Application (e1, e2) ->
        apprhs := 1;
        prepargs symt e2 aregs;
        apprhs := 0;
        codegen symt control e1;
        X86.cut_stack ((List.length e2) - (List.length aregs));
        sp := !sp - ((List.length e2) - (List.length aregs));
        X86.handle_ret ();
        sp := !sp + 1
| Lambda (args, e) -> () (*Func l*)
| Array_make (x,e1,e2) -> ()
       (* codegen symt control e1;
        X86.mkarray ();
        sp := !sp + 1;
        codegen ((x, !sp)::symt) control e2;
        X86.array_gc ();*)
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

let rec genargs symt aregs args = (match (args, aregs) with
| ([], _) -> symt
| (a::xs, []) ->
    X86.svstackarg !isp !sp;
    isp := !isp - 1;
    sp := !sp + 1;
    genargs ((a, !sp - 1) :: symt) [] xs
| (a::xs, r::ys) ->
    X86.svarg r !sp;
    sp := !sp + 1;
    genargs ((a, !sp - 1) :: symt) ys xs)

let rec fungen acc = function
| [] -> acc
| (fname, args, exp)::xs -> let (m, mexp) = acc in
if fname = m then err_exit "Program can only have one main function!" else
if fname = "main" then (match args with
| [] -> fungen (fname, exp) xs
| x  -> err_exit "main() cannot have arguments!")
else (
    X86.funlabel fname;
    functions := fname :: !functions;
    X86.reserve (List.length args);
    isp := (List.length args) - (List.length aregs) - 1;
    let env = genargs [] aregs args in
    codegen env [] exp;
    X86.leave (List.length args);
    sp := 0;
    fungen (m, mexp) xs)

end
;;
