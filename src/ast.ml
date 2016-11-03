type bin_opcode =
    | Plus | Minus | Times | Divide
    | Leq | Geq | Equal | Noteq
    | And | Or

type unary_opcode = Not



type expression =
    | Empty
    | Seq of expression * expression (* e; e *)
    | While of expression * expression (* while e do e *)
    | For of string * expression * expression * expression (* for x = e to e do e*)
    | If of expression * expression * expression (* if e do e else e *)
    | Asg of expression * expression (* e := e *)
    | Deref of expression (* !e *)
    | Bin_Operator of bin_opcode * expression * expression (* e + e *)
    | Unary_Operator of unary_opcode * expression (* not e *)
    | Application of expression * expression list (* e(e) *)
    | Const of int (* 7 *)
    | Readint (* read_int () *)
    | Printint of expression (* print_int (e) *)
    | Identifier of string (* x *)
    | Let of string * expression * expression (* let x = e in e*)
    | New of string * expression * expression (* new x = e in e*)
    | Lambda of string list * expression (* (fun args) { e } *)
    | Array_set of expression * expression * expression
    | Array_get of expression * expression
    | Array_make of string * expression * expression * expression

type fundef = string * string list * expression

type program = fundef list

let op_to_string = function
| Plus -> "Plus"
| Minus -> "Minus"
| Times -> "Times"
| Divide -> "Div"
| Leq -> "Leq"
| Geq -> "Geq"
| Equal -> "Equal"
| Noteq -> "Noteq"
| And -> "And"
| Or -> "Or"

let unop_to_string = function
|  Not -> "Not"

(* Below is an alternative print function for the AST which
produces a more code-like output. *)
(*
let rec exp_to_string = function
| Ast.Empty -> ""
| Ast.Seq (e1, e2) -> String.concat ";" [(exp_to_string e1); (exp_to_string e2)]
| Ast.While (e1, e2) -> String.concat "" ["while("; exp_to_string e1; "){\n"; exp_to_string e2; "\n}"]
| Ast.If (e1, e2, Ast.Empty) -> String.concat "" ["if("; exp_to_string e1; "){\n"; exp_to_string e2; "\n}"]
| Ast.If (e1, e2, e3) -> String.concat "" ["if("; exp_to_string e1; "){\n"; exp_to_string e2; "}else{\n"; exp_to_string e3; "\n}"]
| Ast.Asg (e1, e2) -> String.concat "" [exp_to_string e1; "="; exp_to_string e2]
| Ast.Deref e1 -> String.concat "" ["!"; exp_to_string e1]
| Ast.Bin_Operator (op, Ast.Empty, e2) -> String.concat "" [op_to_string op; exp_to_string e2]
| Ast.Bin_Operator (op, e1, e2) -> String.concat "" [exp_to_string e1; op_to_string op; exp_to_string e2]
| Ast.Unary_Operator (op, e1) -> String.concat "" ["!"; exp_to_string e1]
| Ast.Application (e1, e2) -> String.concat "" [exp_to_string e1; "("; exp_to_string e2; ")"]
| Ast.Const n -> string_of_int n
| Ast.Readint -> "readint()"
| Ast.Printint e1 -> String.concat "" ["printint("; exp_to_string e1; ")\n"]
| Ast.Identifier s -> s
| Ast.Let (s, e1, e2) -> String.concat "" ["let "; s; "="; exp_to_string e1; " in "; exp_to_string e2; "\n"]
| Ast.New (s, e1, e2) -> String.concat "" ["new "; s; "="; exp_to_string e1; " in "; exp_to_string e2; "\n"]
*)

let rec exp_to_string = function
| Empty -> "Empty"
| Seq (e1, e2) -> String.concat "" ["Seq("; exp_to_string e1; ","; exp_to_string e2; ")"]
| While (e1, e2) -> String.concat "" ["While("; exp_to_string e1; ","; exp_to_string e2; ")"]
| For (s, e1, e2, e3) -> ("For(" ^ s ^","^ exp_to_string e1 ^ "," ^ exp_to_string e2 ^ "," ^ exp_to_string e3 ^ ")")
| If (e1, e2, e3) -> String.concat "" ["If("; exp_to_string e1; ","; exp_to_string e2; ","; exp_to_string e3; ")"]
| Asg (e1, e2) -> String.concat "" ["Asg("; exp_to_string e1; ","; exp_to_string e2; ")"]
| Deref e1 -> String.concat "" ["Deref("; exp_to_string e1; ")"]
| Bin_Operator (op, e1, e2) -> String.concat "" ["Bin_Operator("; op_to_string op; ","; exp_to_string e1; ","; exp_to_string e2; ")"]
| Unary_Operator (op, e1) -> String.concat "" ["Unary_Operator("; unop_to_string op; ","; exp_to_string e1; ")"]
| Application (e1, e2) -> String.concat "" ["Application("; exp_to_string e1; ","; " expression list)"]
| Const n -> String.concat "" ["Const("; string_of_int n; ")"]
| Readint -> "readint()"
| Printint e1 -> String.concat "" ["printint("; exp_to_string e1; ")"]
| Identifier s -> String.concat "" ["Identifier("; s; ")"]
| Let (s, e1, e2) -> String.concat "" ["Let("; s; ","; exp_to_string e1; ","; exp_to_string e2; ")"]
| New (s, e1, e2) -> String.concat "" ["New("; s; ","; exp_to_string e1; ","; exp_to_string e2; ")"]
| Lambda (s, e) -> String.concat "" ["Lambda("; String.concat "," s; ","; exp_to_string e;")"]
| Array_get (e1, e2) -> "array def"
| Array_set (e1, e2, e3) -> "array"
| Array_make (s, e1, e2, e3) -> "make"
;;
