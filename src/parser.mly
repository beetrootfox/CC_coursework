%{
open Ast
%}

%token <int> INT
%token READ_INT PRINT_INT
%token PLUS_OP MINUS_OP TIMES_OP DIV_OP
%token EQ_OP LE_OP GE_OP NE_OP
%token AND OR NOT
%token INIT_LOCAL INIT_GLOBAL ASSIGN DEREF
%token IF ELSE WHILE
%token COMMA SEMICOLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token <string> IDENTIFIER
%token EOF

%nonassoc ASSIGN
%nonassoc AND OR
%nonassoc NOT
%left PLUS_OP MINUS_OP
%left TIMES_OP DIV_OP
%nonassoc EQ_OP LE_OP GE_OP NE_OP
%nonassoc DEREF
%left SEMICOLON

%start prog
%type <Ast.program> prog

%%
prog:
el = separated_list(SEMICOLON, fundef); EOF
{ el }

fundef:
fname = IDENTIFIER; LPAREN; arg_list = separated_list(COMMA, IDENTIFIER); RPAREN; LBRACE; e1 = expression; RBRACE
{ fname, arg_list, e1 }

opt_exp:
|
{ Empty }
| e1 = expression
{ e1 }

%inline bin_op:
| PLUS_OP { Plus }
| MINUS_OP { Minus }
| TIMES_OP { Times }
| DIV_OP { Divide }
| EQ_OP { Equal }
| LE_OP { Leq }
| GE_OP { Geq }
| NE_OP { Noteq }
| AND { And }
| OR { Or }

expression:
| e1 = expression; SEMICOLON; e2 = expression
{ Seq (e1, e2) }
| WHILE; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; RBRACE
{ While (e1, e2) }
| IF; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; RBRACE
{ If (e1, e2, Empty) }
| IF; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; RBRACE; ELSE; LBRACE; e3 = expression; RBRACE
{ If (e1, e2, e3) }
| i = IDENTIFIER; ASSIGN; option(LBRACKET); e2 = expression; option(RBRACKET)
{ Asg (Identifier(i), e2) }
| DEREF; e1 = expression
{ Deref e1 }
| e1 = expression; op = bin_op; e2 = expression
{ Bin_Operator (op, e1, e2) }
| MINUS_OP; e1 = expression
{ Bin_Operator (Minus, Empty, e1) }
| NOT; e1 = expression
{ Unary_Operator (Not, e1) }
| e1 = expression; LPAREN; e2 = opt_exp; RPAREN
{ Application (e1, e2) }
| n = INT
{ Const n }
| READ_INT
{ Readint }
| PRINT_INT; LPAREN; e1 = expression; RPAREN
{ Printint e1 }
| x = IDENTIFIER
{ Identifier x }
| INIT_GLOBAL; x = IDENTIFIER; ASSIGN; option(LBRACKET); e1 = expression; option(RBRACKET)
{ Let (x, e1) }
| INIT_LOCAL; x = IDENTIFIER; ASSIGN; option(LBRACKET); e1 = expression; option(RBRACKET)
{ New (x, e1) }
| LPAREN; e1 = expression; RPAREN
{ e1 }

