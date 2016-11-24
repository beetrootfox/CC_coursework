# 1 "lexer.mll"
 
open Lexing
open String
open Parser
open Printf


let hash =
[
"new", INIT_LOCAL; "let", INIT_GLOBAL; "if", IF; "else", ELSE; "while", WHILE; "in", IN; "lambda", LAMBDA; "arraymake", MKARRAY; "arrayset", STARRAY; "of", OF; "get", GET;
"for", FOR; "to", TO; "break", BREAK
]

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    eprintf "at position %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum


# 21 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\225\255\226\255\079\000\160\000\230\255\231\255\232\255\
    \023\000\234\255\237\255\002\000\239\255\240\255\241\255\242\255\
    \243\255\245\255\001\000\003\000\249\255\250\255\251\255\235\000\
    \016\000\254\255\001\000\003\000\253\255\247\255\244\255\238\255\
    \037\000\042\000\040\000\036\000\032\000\027\000\002\000\032\000\
    \049\000\055\000\229\255\245\000\064\001\139\001\214\001\033\002\
    \108\002\183\002\128\000\254\255\124\000\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\028\000\028\000\255\255\255\255\255\255\
    \022\000\255\255\255\255\020\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\009\000\019\000\255\255\255\255\255\255\003\000\
    \007\000\255\255\001\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\027\000\255\255\255\255\001\000\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\051\000\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\025\000\025\000\027\000\026\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\011\000\039\000\027\000\000\000\000\000\014\000\000\000\
    \008\000\007\000\020\000\022\000\010\000\021\000\030\000\024\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\028\000\009\000\018\000\019\000\017\000\031\000\
    \029\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\016\000\040\000\015\000\041\000\003\000\
    \042\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\006\000\013\000\005\000\012\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\052\000\053\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\043\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\044\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\045\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\046\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\047\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\048\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \049\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\026\000\027\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\038\000\027\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\024\000\000\000\000\000\000\000\000\000\011\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\039\000\000\000\040\000\000\000\
    \041\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\008\000\032\000\033\000\034\000\035\000\036\000\037\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\050\000\052\000\255\255\255\255\003\000\255\255\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\255\255\255\255\255\255\255\255\004\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \255\255\255\255\255\255\255\255\043\000\255\255\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \050\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\255\255\255\255\255\255\255\255\044\000\
    \255\255\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\255\255\255\255\
    \255\255\255\255\045\000\255\255\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\255\255\255\255\255\255\255\255\046\000\255\255\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\255\255\255\255\255\255\255\255\
    \047\000\255\255\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\255\255\
    \255\255\255\255\255\255\048\000\255\255\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec read lexbuf =
    __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 27 "lexer.mll"
                ( read lexbuf )
# 310 "lexer.ml"

  | 1 ->
# 28 "lexer.mll"
                ( read lexbuf )
# 315 "lexer.ml"

  | 2 ->
# 29 "lexer.mll"
                ( read_comment lexbuf )
# 320 "lexer.ml"

  | 3 ->
# 30 "lexer.mll"
                ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 325 "lexer.ml"

  | 4 ->
# 31 "lexer.mll"
                ( PLUS_OP )
# 330 "lexer.ml"

  | 5 ->
# 32 "lexer.mll"
                ( MINUS_OP )
# 335 "lexer.ml"

  | 6 ->
# 33 "lexer.mll"
                ( TIMES_OP )
# 340 "lexer.ml"

  | 7 ->
# 34 "lexer.mll"
                ( DIV_OP )
# 345 "lexer.ml"

  | 8 ->
# 35 "lexer.mll"
                ( EQ_OP )
# 350 "lexer.ml"

  | 9 ->
# 36 "lexer.mll"
                ( LE_OP )
# 355 "lexer.ml"

  | 10 ->
# 37 "lexer.mll"
                ( GE_OP )
# 360 "lexer.ml"

  | 11 ->
# 38 "lexer.mll"
                ( PIPELINE )
# 365 "lexer.ml"

  | 12 ->
# 39 "lexer.mll"
                ( LBRACKET )
# 370 "lexer.ml"

  | 13 ->
# 40 "lexer.mll"
                ( RBRACKET )
# 375 "lexer.ml"

  | 14 ->
# 41 "lexer.mll"
                ( AND )
# 380 "lexer.ml"

  | 15 ->
# 42 "lexer.mll"
                ( OR )
# 385 "lexer.ml"

  | 16 ->
# 43 "lexer.mll"
                ( NOT )
# 390 "lexer.ml"

  | 17 ->
# 44 "lexer.mll"
                ( NE_OP )
# 395 "lexer.ml"

  | 18 ->
# 45 "lexer.mll"
                ( COMMA )
# 400 "lexer.ml"

  | 19 ->
# 46 "lexer.mll"
                ( ASSIGN )
# 405 "lexer.ml"

  | 20 ->
# 47 "lexer.mll"
                ( DEREF )
# 410 "lexer.ml"

  | 21 ->
# 48 "lexer.mll"
                ( SEMICOLON )
# 415 "lexer.ml"

  | 22 ->
# 49 "lexer.mll"
                ( LPAREN )
# 420 "lexer.ml"

  | 23 ->
# 50 "lexer.mll"
                ( RPAREN )
# 425 "lexer.ml"

  | 24 ->
# 51 "lexer.mll"
                ( LBRACE )
# 430 "lexer.ml"

  | 25 ->
# 52 "lexer.mll"
                ( RBRACE )
# 435 "lexer.ml"

  | 26 ->
# 53 "lexer.mll"
                   ( READ_INT )
# 440 "lexer.ml"

  | 27 ->
# 54 "lexer.mll"
                ( PRINT_INT )
# 445 "lexer.ml"

  | 28 ->
let
# 55 "lexer.mll"
        w
# 451 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 56 "lexer.mll"
                (
                    let s = String.lowercase_ascii w in
                    try List.assoc s hash
                    with Not_found -> IDENTIFIER w
                )
# 459 "lexer.ml"

  | 29 ->
# 61 "lexer.mll"
                ( EOF )
# 464 "lexer.ml"

  | 30 ->
# 62 "lexer.mll"
                (
                  prerr_string ("Invalid token \"" ^ Lexing.lexeme lexbuf ^ "\" ");
                  print_position lexbuf;
                  exit(-1)
                )
# 473 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_comment lexbuf =
    __ocaml_lex_read_comment_rec lexbuf 50
and __ocaml_lex_read_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 70 "lexer.mll"
                ( read lexbuf )
# 485 "lexer.ml"

  | 1 ->
# 71 "lexer.mll"
                ( read_comment lexbuf )
# 490 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_read_comment_rec lexbuf __ocaml_lex_state

;;

