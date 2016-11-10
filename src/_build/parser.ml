
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState137
  | MenhirState129
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState122
  | MenhirState121
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState110
  | MenhirState109
  | MenhirState107
  | MenhirState106
  | MenhirState102
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState87
  | MenhirState86
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState72
  | MenhirState71
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState41
  | MenhirState40
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState15
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState5
  | MenhirState3
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
    | MenhirState137 ->
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
    | MenhirState81 ->
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
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | FOR ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | GET ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | IDENTIFIER _v ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                    | IF ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | INIT_GLOBAL ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | INIT_LOCAL ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | INT _v ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                    | LAMBDA ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | LPAREN ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | MINUS_OP ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | MKARRAY ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | NOT ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | READ_INT ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | STARRAY ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | ASSIGN | COMMA | DIV_OP | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TIMES_OP | TO ->
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
    | MenhirState87 ->
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
    | MenhirState126 ->
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
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (fname : (string))), _, (xs0 : (string list))), _, (e1 : (Ast.expression))), _, (_8 : (unit option))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
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
                | LPAREN ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
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

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expression list)) = _v in
        let _v : (Ast.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expression list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expression))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression list) -> 'ttv_return =
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
        let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (xs0 : (Ast.expression list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expression) = let arg_list =
          let xs = xs0 in
              ( xs )
        in
            ( Application (e1, arg_list) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.expression) =     ( Deref e1 ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState43 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState45 in
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
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
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
    | MenhirState82 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Seq (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
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
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Asg (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AND | ASSIGN | COMMA | IN | LBRACKET | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState72 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _), _, (e3 : (Ast.expression))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( For (x, e1, e2, e3) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Array_get (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState78 in
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
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMICOLON ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SEMICOLON ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Let (x, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( New (x, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState96 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (xs0 : (string list))), _, (e : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) = let args =
              let xs = xs0 in
                  ( xs )
            in
                ( Lambda (args, e) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Printint e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PIPELINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState100 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState102 in
                let _v : (Ast.expression list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState100 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState107 | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState106 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Bin_Operator (Minus, Empty, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (x : (string))), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _), _, (e3 : (Ast.expression))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Array_make (x, e1, e2, e3) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | NE_OP | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Unary_Operator (Not, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState117 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState119 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))), _), _, (e3 : (Ast.expression))) = _menhir_stack in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Array_set (e1, e2, e3) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
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
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | SEMICOLON ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | ASSIGN ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | DIV_OP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | EQ_OP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | GE_OP ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | LE_OP ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUS_OP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | NE_OP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | OR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PLUS_OP ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SEMICOLON ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | TIMES_OP ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GET ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | IDENTIFIER _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INIT_GLOBAL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INIT_LOCAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LAMBDA ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MINUS_OP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MKARRAY ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | READ_INT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | STARRAY ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expression) =     ( Readint ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
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

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | PRINT_INT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState20 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PIPELINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LBRACE ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (int)) = _v in
    let _v : (Ast.expression) =     ( Const n ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
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

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
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

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | FOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | GET ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENTIFIER _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | IF ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INIT_GLOBAL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INIT_LOCAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LAMBDA ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MINUS_OP ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | MKARRAY ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NOT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | READ_INT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | STARRAY ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (string)) = _v in
    let _v : (Ast.expression) =     ( Identifier x ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
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
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | LBRACE | RPAREN ->
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
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | PIPELINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | RPAREN ->
                _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
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
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _v : (Ast.program) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

