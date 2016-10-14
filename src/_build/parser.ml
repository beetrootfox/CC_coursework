
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState98
  | MenhirState90
  | MenhirState87
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState63
  | MenhirState62
  | MenhirState58
  | MenhirState57
  | MenhirState56
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
  | MenhirState39
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState31
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
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

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.program)) = _v in
        let _v : (Ast.program) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.program)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.fundef))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
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
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | IDENTIFIER _v ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                    | IF ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | INIT_GLOBAL ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | INIT_LOCAL ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | INT _v ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | MINUS_OP ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | NOT ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | PRINT_INT ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | READ_INT ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | WHILE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | ASSIGN | DIV_OP | EQ_OP | GE_OP | IN | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON | TIMES_OP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _, (_7 : (unit option))) = _menhir_stack in
                let _8 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.expression) =     ( If (e1, e2, Empty) ) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _, (_7 : (unit option))), _, (e3 : (Ast.expression))), _, (_12 : (unit option))) = _menhir_stack in
            let _13 = () in
            let _10 = () in
            let _9 = () in
            let _8 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( If (e1, e2, e3) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _, (_7 : (unit option))) = _menhir_stack in
            let _8 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( While (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (fname : (string))), _, (xs0 : (string list))), _, (e1 : (Ast.expression))), _, (_7 : (unit option))) = _menhir_stack in
            let _8 = () in
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
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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
        let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expression) =     ( Application (e1, e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33
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

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
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

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
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
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
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
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
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

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
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

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.expression) =     ( Deref e1 ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState30 in
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
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | IDENTIFIER _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | IF ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | INIT_GLOBAL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | INIT_LOCAL ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | MINUS_OP ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | PRINT_INT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | READ_INT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
        let _10 = () in
        let _v : (Ast.expression) = let op =
          let _1 = _10 in
                         ( Times )
        in
            ( Bin_Operator (op, e1, e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 | MenhirState69 | MenhirState58 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IN | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Seq (e1, e2) ) in
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
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | AND | ASSIGN | EQ_OP | GE_OP | IN | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
        let _10 = () in
        let _v : (Ast.expression) = let op =
          let _1 = _10 in
                       ( Divide )
        in
            ( Bin_Operator (op, e1, e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | AND | ASSIGN | EQ_OP | GE_OP | IN | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | IN | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Asg (e1, e2) ) in
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
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | AND | ASSIGN | IN | OR | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SEMICOLON ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | RBRACE ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SEMICOLON ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | RBRACE ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState68 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | AND | ASSIGN | DIV_OP | EQ_OP | GE_OP | IN | LE_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON | TIMES_OP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.expression) =     ( Let (x, e1, e2) ) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState72 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | AND | ASSIGN | DIV_OP | EQ_OP | GE_OP | IN | LE_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON | TIMES_OP ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.expression) =     ( New (x, e1, e2) ) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PIPELINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState76 in
                let _v : (Ast.expression) =     ( Empty ) in
                _menhir_goto_opt_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e1 : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) =     ( e1 ) in
            _menhir_goto_opt_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AND | ASSIGN | EQ_OP | GE_OP | IN | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Bin_Operator (Minus, Empty, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ASSIGN | IN | PIPELINE | RBRACE | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Unary_Operator (Not, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Printint e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState84 in
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
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | IDENTIFIER _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | IF ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | INIT_GLOBAL ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | INIT_LOCAL ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | INT _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | MINUS_OP ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | PRINT_INT ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | READ_INT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | WHILE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMICOLON ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RBRACE ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | ASSIGN ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DIV_OP ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EQ_OP ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | GE_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LE_OP ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS_OP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NE_OP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS_OP ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMICOLON ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES_OP ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RBRACE ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        _menhir_fail ()

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
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INIT_GLOBAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState11
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
    let _v : (Ast.expression) =     ( Readint ) in
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
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INIT_GLOBAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14
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
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
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
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
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
    let _v : (Ast.expression) =     ( Const n ) in
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
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
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

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | DEREF ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INIT_LOCAL ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MINUS_OP ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | PRINT_INT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | READ_INT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | WHILE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
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

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INIT_GLOBAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INIT_LOCAL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MINUS_OP ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PRINT_INT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | READ_INT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | WHILE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (string)) = _v in
    let _v : (Ast.expression) =     ( Identifier x ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INIT_GLOBAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INIT_LOCAL ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | MINUS_OP ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | PRINT_INT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | READ_INT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | WHILE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_goto_separated_nonempty_list_SEMICOLON_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
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
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INIT_GLOBAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
    | SEMICOLON ->
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
        _menhir_goto_separated_nonempty_list_SEMICOLON_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
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
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
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
  

