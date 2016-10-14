
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES_OP
  | SEMICOLON
  | RPAREN
  | READ_INT
  | RBRACKET
  | RBRACE
  | PRINT_INT
  | PLUS_OP
  | OR
  | NOT
  | NE_OP
  | MINUS_OP
  | LPAREN
  | LE_OP
  | LBRACKET
  | LBRACE
  | INT of (int)
  | INIT_LOCAL
  | INIT_GLOBAL
  | IF
  | IDENTIFIER of (string)
  | GE_OP
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
