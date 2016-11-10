
(* The type of tokens. *)

type token = 
  | WHILE
  | TO
  | TIMES_OP
  | STARRAY
  | SEMICOLON
  | RPAREN
  | READ_INT
  | RBRACKET
  | RBRACE
  | PRINT_INT
  | PLUS_OP
  | PIPELINE
  | OR
  | OF
  | NOT
  | NE_OP
  | MKARRAY
  | MINUS_OP
  | LPAREN
  | LE_OP
  | LBRACKET
  | LBRACE
  | LAMBDA
  | INT of (int)
  | INIT_LOCAL
  | INIT_GLOBAL
  | IN
  | IF
  | IDENTIFIER of (string)
  | GE_OP
  | GET
  | FOR
  | EQ_OP
  | EOF
  | ELSE
  | DIV_OP
  | DEREF
  | COMMA
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
