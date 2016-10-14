{
open Lexing
open String
open Parser


let key = ["new", INIT_LOCAL; "let", INIT_GLOBAL; "if", IF; "else", ELSE; "while", WHILE]

let line_num = ref 1

exception Syntax_error of string

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))
}

let int = ['0'-'9']['0'-'9']*
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
    parse
| white         { read lexbuf }
| newline       { read lexbuf }
| int           { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '+'           { PLUS_OP }
| '-'           { MINUS_OP }
| '*'           { TIMES_OP }
| '/'           { DIV_OP }
| "=="          { EQ_OP }
| '<'           { LE_OP }
| '>'           { GE_OP }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| '&'           { AND }
| '|'           { OR }
| '~'           { NOT }
| "!="          { NE_OP }
| '='           { ASSIGN }
| '!'           { DEREF }
| ','           { COMMA }
| ';'           { SEMICOLON }
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| "readint()"   { READ_INT }
| "printint"    { PRINT_INT }
| id as w
{
    let s = String.lowercase_ascii w in
    try List.assoc s key
    with Not_found -> IDENTIFIER w
}
| eof           { EOF }
| _             { syntax_error "wrong token" }
