open Parser;;
open Ast;;
open Lexer;;
open Buffer;;
open String;;

let op_to_string = function
| Ast.Plus -> "Plus"
| Ast.Minus -> "Minus"
| Ast.Times -> "Times"
| Ast.Divide -> "Div"
| Ast.Leq -> "Leq"
| Ast.Geq -> "Geq"
| Ast.Equal -> "Equal"
| Ast.Noteq -> "Noteq"
| Ast.And -> "And"
| Ast.Or -> "Or"
;;
let unop_to_string = function
| Ast.Not -> "Not"
;;
(*let rec exp_to_string = function
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
| Ast.Let (s, e1) -> String.concat "" ["let "; s; "="; exp_to_string e1; "\n"]
| Ast.New (s, e1) -> String.concat "" ["new "; s; "="; exp_to_string e1; "\n"]
;;*)
let rec exp_to_string = function
| Ast.Empty -> "Empty"
| Ast.Seq (e1, e2) -> String.concat "" ["Seq("; exp_to_string e1; ","; exp_to_string e2; ")"]
| Ast.While (e1, e2) -> String.concat "" ["While("; exp_to_string e1; ","; exp_to_string e2; ")"]
| Ast.If (e1, e2, e3) -> String.concat "" ["If("; exp_to_string e1; ","; exp_to_string e2; ","; exp_to_string e3; ")"]
| Ast.Asg (e1, e2) -> String.concat "" ["Asg("; exp_to_string e1; ","; exp_to_string e2; ")"]
| Ast.Deref e1 -> String.concat "" ["Deref("; exp_to_string e1; ")"]
| Ast.Bin_Operator (op, e1, e2) -> String.concat "" ["Bin_Operator("; op_to_string op; ","; exp_to_string e1; ","; exp_to_string e2; ")"]
| Ast.Unary_Operator (op, e1) -> String.concat "" ["Unary_Operator("; unop_to_string op; ","; exp_to_string e1; ")"]
| Ast.Application (e1, e2) -> String.concat "" ["Application("; exp_to_string e1; ","; exp_to_string e2; ")"]
| Ast.Const n -> String.concat "" ["Const("; string_of_int n; ")"]
| Ast.Readint -> "readint()"
| Ast.Printint e1 -> String.concat "" ["printint("; exp_to_string e1; ")"]
| Ast.Identifier s -> String.concat "" ["Identifier("; s; ")"]
| Ast.Let (s, e1) -> String.concat "" ["Let("; s; ","; exp_to_string e1; ")"]
| Ast.New (s, e1) -> String.concat "" ["New("; s; ","; exp_to_string e1; ")"]
;;
let fundef_to_string (s, sl, exp) = String.concat "" ["fundef("; s; ",["; String.concat ";" sl; "],"; exp_to_string exp; ")"];;

let rec read_to_empty buf =
    let s = read_line () in
    if s = "" then buf
else (Buffer.add_string buf s;
      Buffer.add_string buf "\n";
read_to_empty buf);;

let _ =
    read_to_empty (Buffer.create 1)
    |> Buffer.contents
    |> Lexing.from_string
    |> Parser.prog Lexer.read
    |> List.map fundef_to_string
    |> String.concat "\n"
    |> print_endline;;
