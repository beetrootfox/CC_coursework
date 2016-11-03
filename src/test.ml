open Parser
open Ast
open Lexer
open Lexing
open Buffer
open String
open Printf
open Evaluator
open Optimizer

(*let fundef_to_string (s, sl, exp) = String.concat "" ["fundef("; s; ",["; String.concat ";" sl; "],"; exp_to_string exp; ")"]*)

(*let fundef_to_value_str (s, sl, exp) = exp |> eval_exp |> value_to_string*)
let fundef_to_value_str flist = let (_, exp) = eval_prog ("",Empty) flist in
                                exp |> eval_exp [] |> value_to_string

(*let fundef_to_value_str flist = let (_, exp) = eval_prog ("",Empty) flist in
               exp |> optimise_exp [] |> exp_of_val |> exp_to_string*)
let fundef_to_value_str_opt flist = let (_, exp) = eval_prog ("",Empty) flist in
exp |> optimise_exp [] |> exp_of_val |> eval_exp [] |> value_to_string


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

let _ =
    read_to_empty (Buffer.create 1)
    |> Buffer.contents
    |> Lexing.from_string
    |> parse_with_error
    |> (if opt then fundef_to_value_str_opt else fundef_to_value_str)
    |> print_endline;
    Printf.printf "%d\n" !eval_steps
;;
