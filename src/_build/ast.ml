type bin_opcode =
    | Plus | Minus | Times | Divide
    | Leq | Geq | Equal | Noteq
    | And | Or

type unary_opcode = Not

type expression =
    | Empty
    | Seq of expression * expression (* e; e *)
    | While of expression * expression (* while e do e *)
    | If of expression * expression * expression (* if e do e else e *)
    | Asg of expression * expression (* e := e *)
    | Deref of expression (* !e *)
    | Bin_Operator of bin_opcode * expression * expression (* e + e *)
    | Unary_Operator of unary_opcode * expression (* not e *)
    | Application of expression * expression (* e(e) *)
    | Const of int (* 7 *)
    | Readint (* read_int () *)
    | Printint of expression (* print_int (e) *)
    | Identifier of string (* x *)
    | Let of string * expression * expression (* let x = e in e*)
    | New of string * expression * expression (* new x = e in e*)

type fundef = string * string list * expression

type program = fundef list
;;
