%{
open Ast
%}

%token <int> INT
%token READ_INT PRINT_INT
%token PLUS_OP MINUS_OP TIMES_OP DIV_OP
%token EQ_OP LE_OP GE_OP NE_OP
%token AND OR NOT
%token INIT_LOCAL INIT_GLOBAL ASSIGN DEREF IN PIPELINE
%token IF ELSE WHILE
%token SEMICOLON LPAREN RPAREN LBRACE RBRACE
%token <string> IDENTIFIER
%token EOF

%left IN
%left SEMICOLON
%nonassoc ASSIGN

%left AND OR
%nonassoc EQ_OP LE_OP GE_OP NE_OP
%nonassoc NOT


%left PLUS_OP MINUS_OP
%left TIMES_OP DIV_OP

%nonassoc DEREF

%start prog
%type <Ast.program> prog

%%
prog:
    el = separated_list(SEMICOLON, fundef); EOF
    { el }

fundef:
    fname = IDENTIFIER; LPAREN; arg_list = separated_list(SEMICOLON, IDENTIFIER); RPAREN; LBRACE; e1 = expression; option(SEMICOLON); RBRACE
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
    | WHILE; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; option(SEMICOLON); RBRACE
    { While (e1, e2) }
    | IF; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; option(SEMICOLON); RBRACE
    { If (e1, e2, Empty) }
    | IF; LPAREN; e1 = expression; RPAREN; LBRACE; e2 = expression; option(SEMICOLON); RBRACE; ELSE; LBRACE; e3 = expression; option(SEMICOLON); RBRACE
    { If (e1, e2, e3) }
    | e1 = expression; ASSIGN; e2 = expression
    { Asg (e1, e2) }
    | DEREF; e1 = expression
    { Deref e1 }
    | e1 = expression; op = bin_op; e2 = expression
    { Bin_Operator (op, e1, e2) }
    | MINUS_OP; e1 = expression
    { Bin_Operator (Minus, Empty, e1) }
    | NOT; e1 = expression
    { Unary_Operator (Not, e1) }
    | LPAREN; e1 = expression; PIPELINE; e2 = opt_exp; RPAREN
    { Application (e1, e2) }
    | n = INT
    { Const n }
    | READ_INT
    { Readint }
    | PRINT_INT; LPAREN; e1 = expression; RPAREN
    { Printint e1 }
    | x = IDENTIFIER
    { Identifier x }
    | INIT_GLOBAL; x = IDENTIFIER; ASSIGN; e1 = expression; IN; e2 = expression
    { Let (x, e1, e2) }
    | INIT_LOCAL; x = IDENTIFIER; ASSIGN; e1 = expression; IN; e2 = expression
    { New (x, e1, e2) }
    | LPAREN; e1 = expression; RPAREN
    { e1 }
    | e1 = expression; SEMICOLON; e2 = expression
    { Seq (e1, e2) }
