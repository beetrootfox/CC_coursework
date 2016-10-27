{
open Lexing
open String
open Parser
open Printf


let hash =
[
"new", INIT_LOCAL; "let", INIT_GLOBAL; "if", IF; "else", ELSE; "while", WHILE; "in", IN; "lambda", LAMBDA; "arraymake", MKARRAY; "arrayset", STARRAY; "of", OF; "to", TO; "get", GET
]

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    eprintf "at position %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

}

let int = ['0'-'9']['0'-'9']*
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
    parse
| white         { read lexbuf }
| newline       { read lexbuf }
| "/*"          { read_comment lexbuf }
| int           { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '+'           { PLUS_OP }
| '-'           { MINUS_OP }
| '*'           { TIMES_OP }
| '/'           { DIV_OP }
| "=="          { EQ_OP }
| '<'           { LE_OP }
| '>'           { GE_OP }
| "<-"          { PIPELINE }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| '&'           { AND }
| '|'           { OR }
| '~'           { NOT }
| "!="          { NE_OP }
| '='           { ASSIGN }
| '!'           { DEREF }
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
                    try List.assoc s hash
                    with Not_found -> IDENTIFIER w
                }
| eof           { EOF }
| _             {
                  prerr_string ("Invalid token \"" ^ Lexing.lexeme lexbuf ^ "\" ");
                  print_position lexbuf;
                  exit(-1)
                }

and read_comment =
parse
| "*/"          { read lexbuf }
|  _            { read_comment lexbuf }
