%{
open Ast
%}

%token <int> INT
%token READ_INT PRINT_INT
%token PLUS_OP MINUS_OP TIMES_OP DIV_OP
%token EQ_OP LE_OP GE_OP NE_OP
%token AND OR NOT
%token INIT_LOCAL INIT_GLOBAL ASSIGN DEREF IN PIPELINE
%token IF ELSE WHILE FOR TO
%token SEMICOLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA BREAK
%token <string> IDENTIFIER
%token LAMBDA
%token MKARRAY STARRAY GET
%token EOF

%left IN
%right SEMICOLON
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
LPAREN; fname = IDENTIFIER; PIPELINE; arg_list = separated_list(COMMA, IDENTIFIER); RPAREN; LBRACE; e1 = expression; option(SEMICOLON); RBRACE
    { fname, arg_list, e1 }

(*TODO: deal with parser error op option(SEMICOLON)*)

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
    | FOR; LPAREN; x = IDENTIFIER; ASSIGN; e1 = expression; TO; e2 = expression; RPAREN; LBRACE; e3 = expression; RBRACE;
    { For (x, e1, e2, e3) }
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
    | LPAREN; e1 = expression; PIPELINE; arg_list = separated_list(COMMA, expression); RPAREN
    { Application (e1, arg_list) }
    | n = INT
    { Const n }
    | READ_INT
    { Readint }
    | LPAREN; PRINT_INT; PIPELINE; e1 = expression; RPAREN
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
    | LAMBDA; args = separated_list(COMMA, IDENTIFIER); LBRACE; e = expression; RBRACE;
    { Lambda (args, e) }
    | MKARRAY; x = IDENTIFIER; LBRACKET; e1 = expression; RBRACKET; IN; e2 = expression
    { Array_make (x, e1, e2) }
    | GET; e1 = expression; LBRACKET; e2 = expression; RBRACKET
    { Array_get (e1, e2) }
    | STARRAY; e1 = expression; LBRACKET; e2 = expression; RBRACKET; ASSIGN; e3 = expression
    { Array_set (e1, e2, e3) }
    | BREAK;
    { Break }
