open Parser
open Ast
open Lexer
open Lexing
open Buffer
open String
open Printf
open Evaluator
open Optimizer
open Compiler
open Hashtbl
open X86_compiler

(*let fundef_to_string (s, sl, exp) = String.concat "" ["fundef("; s; ",["; String.concat ";" sl; "],"; exp_to_string exp; ")"]*)

(*let fundef_to_value_str (s, sl, exp) = exp |> eval_exp |> value_to_string*)
let fundef_to_value_str flist = let (_, exp) = eval_prog ("",Empty) flist in
                                exp |> eval_exp [] |> value_to_string

(*let fundef_to_value_str flist = let (_, exp) = eval_prog ("",Empty) flist in
               exp |> optimise_exp [] |> exp_of_val |> exp_to_string*)
let fundef_to_value_str_opt flist = let (_, exp) = eval_prog ("",Empty) flist in
exp |> optimise_exp [] |> exp_of_val |> eval_exp [] |> value_to_string


(*let fun_of_op op = match op with
| Plus -> ( + )
| Minus -> ( - )
| Times -> ( * )
| Divide -> ( / )
| Leq -> ( < )
| Geq -> ( > )
| Equal -> ( = )
| And -> ( && )
| Or -> ( || )*)

module Interp = Interpreter(struct

let op_ins op addr1 addr2 = let v1 = Hashtbl.find ram addr1 in
                            (match v1 with
                            | Constant x ->
                                let v2 = Hashtbl.find ram addr2 in
                                (match v2 with
                                | Constant y -> acc := op_app_int op x y
                                | _          -> err_exit "type mismatch op_ins")
                            | Comp x ->
                                let v2 = Hashtbl.find ram addr2 in
                                (match v2 with
                                | Comp y -> acc := op_app_bool op x y
                                | _      -> err_exit "type mismatch op_ins")
                            | Nothing ->
                                let v2 = Hashtbl.find ram addr2 in
                                (match v2 with
                                | Constant y -> acc := Constant (-y)
                                | _ -> err_exit "type mismatch op_ins")
                            | _ -> err_exit "type MISMATCH op_ins")

let unary_op_ins op addr = let v = Hashtbl.find ram addr in
                        (match v with
                        | Comp b -> acc := Comp (not b)
| _ -> err_exit "unary_op_ins fail")

let st_ins addr = Hashtbl.replace ram addr !acc
let ldc_ins v = acc := v
let ldr_ins addr = acc := (Hashtbl.find ram addr)
let mv_ins addr1 addr2 = ldc_ins (Hashtbl.find ram addr1); st_ins addr2
end)

let code = Buffer.create 1000

let _ = Buffer.add_string code prefix

module GenCode = Interpreter(struct

let op_ins op addr1 addr2 =
(ins_str_of_operator op)
^ " r" ^ (string_of_int addr1)
^ ", r" ^ (string_of_int addr2)
^ "\n" |> Buffer.add_string code

let unary_op_ins op addr = "neg r" ^ (string_of_int addr) ^ "\n" |> Buffer.add_string code

let st_ins addr = "st r" ^ (string_of_int addr)
^ "\n" |> Buffer.add_string code
let ldc_ins n = "ld " ^ (value_to_string' n)
^ "\n" |> Buffer.add_string code
let ldr_ins addr = "ld r" ^ (string_of_int addr)
^ "\n" |> Buffer.add_string code
let mv_ins addr1 addr2 = "mv r" ^ (string_of_int addr1)
                          ^ ", r" ^ (string_of_int addr2) ^ "\n" |> Buffer.add_string code
end)

module Gen_x86 = Compiler86(struct

let op_ins op = (match op with
    | Divide ->
        "popq %rbx\n" ^
        "popq %rax\n" ^
        "cqto\n" ^
        (x86_str_of_operator op) ^ " %rbx\n" ^
        "push %rax\n" |> Buffer.add_string code
    | _ ->
        "popq %rax\n" ^
        "popq %rbx\n" ^
        (x86_str_of_operator op) ^ " %rax, %rbx\n" ^
        "push %rbx\n" |> Buffer.add_string code)

let id_ins addr =
    "//offset " ^ (string_of_int addr) ^ "\n" ^
    "movq " ^ (-16 - 8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
    "push %rax\n"
    |> Buffer.add_string code

let st_ins n =
    "push $" ^ (string_of_int n) ^ "\n"
    |> Buffer.add_string code

let let_ins () =
    "pop %rax\n" ^
    "pop %rbx\n" ^
    "push %rax\n"
    |> Buffer.add_string code

let lea_ins addr =
    "leaq " ^ (-16 - 8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
    "push %rax\n"
    |> Buffer.add_string code

let new_ins () =
    "pop %rax\n" ^
    "pop %rbx\n" ^
    "pop %rcx\n" ^
    "push %rax\n"
    |> Buffer.add_string code

let deref_ins () =
    "pop %rax\n" ^
    "movq (%rax), %rax\n" ^
    "push %rax\n"
    |> Buffer.add_string code

let asg_ins () =
    "pop %rbx\n" ^
    "pop %rax\n" ^
    "movq %rbx, (%rax)\n" ^
    "push %rbx\n"
    |> Buffer.add_string code

end)

let compile flist = let (_, exp) = eval_prog ("",Empty) flist in
Interp.interpret [] exp
let generate flist = let (_, exp) = eval_prog ("", Empty) flist in
GenCode.interpret [] exp
let compile_x86 flist = let (_, exp) = eval_prog ("", Empty) flist in
Gen_x86.codegen [] exp

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
    | Parser.Error -> prerr_string (String.concat "" ["Parsing error near \""; Lexing.lexeme lexbuf; "\" "]);
                      Lexer.print_position lexbuf;
                      exit(-1)

let rec read_to_empty buf =
    let s = read_line () in
    if s = "" then buf
    else (Buffer.add_string buf s;
          Buffer.add_string buf "\n";
          read_to_empty buf)

let opt = let len = Array.length Sys.argv in
          if len > 1 then Sys.argv.(1) = "-o" else false

let addr =
    read_to_empty (Buffer.create 1)
    |> Buffer.contents
    |> Lexing.from_string
    |> parse_with_error
    (*|> (if opt then fundef_to_value_str_opt else fundef_to_value_str)*)
    (*
    |> compile|> Hashtbl.find ram |> value_to_string |> print_endline;
    *)
    |> compile_x86
let _ = Buffer.output_buffer stdout code;
postfix |> print_endline;
   (* Printf.printf "%d\n" !eval_steps *)
;;
