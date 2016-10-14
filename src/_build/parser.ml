
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState95
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState26
  | MenhirState23
  | MenhirState21
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState11
  | MenhirState9
  | MenhirState4
  | MenhirState2
  | MenhirState0
  
open Ast

let rec _menhir_goto_option_RBRACKET_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_5 : (unit option)) = _v in
        let (((_menhir_stack, _menhir_s, (i : (string))), _, (_3 : (unit option))), _, (e2 : (Ast.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expression) = ( Asg (Identifier(i), e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_6 : (unit option)) = _v in
        let ((((_menhir_stack, _menhir_s), (x : (string))), _, (_4 : (unit option))), _, (e1 : (Ast.expression))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expression) = ( Let (x, e1) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_6 : (unit option)) = _v in
        let ((((_menhir_stack, _menhir_s), (x : (string))), _, (_4 : (unit option))), _, (e1 : (Ast.expression))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expression) = ( New (x, e1) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.program)) = _v in
        let _v : (Ast.program) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.program)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.fundef))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_RBRACKET_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_RBRACKET_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_opt_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.expression) = ( Application (e1, e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState37 in
        let _v : (Ast.expression) = ( Empty ) in
        _menhir_goto_opt_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_option_LBRACKET_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | AND | DIV_OP | EQ_OP | GE_OP | LE_OP | MINUS_OP | NE_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) = ( Deref e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | AND | DIV_OP | EQ_OP | GE_OP | LE_OP | MINUS_OP | NE_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) = ( Seq (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e1 : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = ( e1 ) in
            _menhir_goto_opt_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                         ( Times )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                      ( Noteq )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                      ( Leq )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                      ( Geq )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                      ( Equal )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | AND | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                        ( Plus )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | AND | DIV_OP | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | TIMES_OP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                       ( Divide )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                   ( Or )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | AND | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                         ( Minus )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expression) = let op =
              let _1 = _10 in
                    ( And )
            in
            ( Bin_Operator (op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RBRACKET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RBRACE | RPAREN ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | IDENTIFIER _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | IF ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INIT_GLOBAL ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INIT_LOCAL ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | MINUS_OP ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | PRINT_INT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | READ_INT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DEREF ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | IDENTIFIER _v ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
                    | IF ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | INIT_GLOBAL ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | INIT_LOCAL ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | INT _v ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | MINUS_OP ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | NOT ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | PRINT_INT ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | READ_INT ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | WHILE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | DIV_OP | EQ_OP | GE_OP | LE_OP | LPAREN | MINUS_OP | NE_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TIMES_OP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expression) = ( If (e1, e2, Empty) ) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _), _, (e3 : (Ast.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) = ( If (e1, e2, e3) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RBRACKET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RBRACE | RPAREN ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | RBRACKET ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | RBRACE | RPAREN ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) = ( e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | AND | MINUS_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) = ( Bin_Operator (Minus, Empty, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AND | OR | RBRACE | RBRACKET | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) = ( Unary_Operator (Not, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) = ( Printint e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState83 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | IDENTIFIER _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | IF ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | INIT_GLOBAL ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | INIT_LOCAL ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | MINUS_OP ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | PRINT_INT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | READ_INT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState86 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) = ( While (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NE_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState88 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (fname : (string))), _, (xs0 : (string list))), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.fundef) = let arg_list =
              let xs = xs0 in
                  ( xs )
            in
            ( fname, arg_list, e1 ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENTIFIER _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.fundef))) = _menhir_stack in
                let _v : (Ast.program) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TIMES_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | _ ->
        _menhir_fail ()

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_LBRACKET_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_LBRACKET_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expression) = ( Readint ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (int)) = _v in
    let _v : (Ast.expression) = ( Const n ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | DEREF | IDENTIFIER _ | IF | INIT_GLOBAL | INIT_LOCAL | INT _ | LPAREN | MINUS_OP | NOT | PRINT_INT | READ_INT | WHILE ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | DEREF | IDENTIFIER _ | IF | INIT_GLOBAL | INIT_LOCAL | INT _ | LPAREN | MINUS_OP | NOT | PRINT_INT | READ_INT | WHILE ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEREF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | IDENTIFIER _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | IF ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | INIT_GLOBAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DEREF | IDENTIFIER _ | IF | INIT_GLOBAL | INIT_LOCAL | INT _ | LPAREN | MINUS_OP | NOT | PRINT_INT | READ_INT | WHILE ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | AND | DIV_OP | EQ_OP | GE_OP | LE_OP | LPAREN | MINUS_OP | NE_OP | OR | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TIMES_OP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (Ast.expression) = ( Identifier x ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENTIFIER _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INIT_GLOBAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | IDENTIFIER _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | IF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INIT_GLOBAL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (xs0 : (Ast.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.program) = let el =
          let xs = xs0 in
              ( xs )
        in
        ( el ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.program)) = _v in
        Obj.magic _1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _v : (string list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _v : (Ast.program) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

