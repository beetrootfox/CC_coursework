
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES_OP
  | SEMICOLON
  | RPAREN
  | READ_INT
  | RBRACE
  | PRINT_INT
  | PLUS_OP
  | PIPELINE
  | OR
  | NOT
  | NE_OP
  | MINUS_OP
  | LPAREN
  | LE_OP
  | LBRACE
  | INT of (int)
  | INIT_LOCAL
  | INIT_GLOBAL
  | IN
  | IF
  | IDENTIFIER of (string)
  | GE_OP
  | EQ_OP
  | EOF
  | ELSE
  | DIV_OP
  | DEREF
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
