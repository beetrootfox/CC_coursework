
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
  | BREAK
  | ASSIGN
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState138
  | MenhirState130
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState122
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState88
  | MenhirState87
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState70
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
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
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
    | MenhirState138 ->
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
    | MenhirState82 ->
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
                    | BREAK ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | DEREF ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | FOR ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | GET ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | IDENTIFIER _v ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | IF ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | INIT_GLOBAL ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | INIT_LOCAL ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | INT _v ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | LAMBDA ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | LPAREN ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | MINUS_OP ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | MKARRAY ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | NOT ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | READ_INT ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | STARRAY ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | WHILE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
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
    | MenhirState88 ->
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
    | MenhirState127 ->
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
    | MenhirState130 ->
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
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
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
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expression list)) = _v in
        let _v : (Ast.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState108 ->
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

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | DEREF ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FOR ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | GET ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENTIFIER _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IF ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INIT_GLOBAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INIT_LOCAL ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LAMBDA ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MINUS_OP ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MKARRAY ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | READ_INT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | STARRAY ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

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
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState44 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState46 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState47 ->
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
    | MenhirState83 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Seq (e1, e2) ) in
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
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | COMMA | IN | LBRACKET | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expression) =     ( Asg (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState73 in
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))), _), _, (e2 : (Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( Array_get (e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState79 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SEMICOLON ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | RBRACE ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SEMICOLON ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | RBRACE ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState91 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState94 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState97 in
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState99 in
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PIPELINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState103 in
                let _v : (Ast.expression list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expression) =     ( e1 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState108 | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | MINUS_OP | NE_OP | OR | PIPELINE | PLUS_OP | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Bin_Operator (Minus, Empty, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState114 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | AND | ASSIGN | COMMA | EQ_OP | GE_OP | IN | LBRACKET | LE_OP | NE_OP | OR | PIPELINE | RBRACE | RBRACKET | RPAREN | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expression) =     ( Unary_Operator (Not, e1) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState118 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | DEREF ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FOR ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | GET ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IDENTIFIER _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | IF ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INIT_GLOBAL ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INIT_LOCAL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | LAMBDA ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | MINUS_OP ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | MKARRAY ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NOT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | READ_INT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | STARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState120 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState124 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | DEREF ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | FOR ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | GET ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | IDENTIFIER _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
                | IF ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | INIT_GLOBAL ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | INIT_LOCAL ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | INT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
                | LAMBDA ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | MINUS_OP ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | MKARRAY ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | NOT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | READ_INT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | STARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SEMICOLON ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | RBRACE ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | ASSIGN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | DIV_OP ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | EQ_OP ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | GE_OP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LE_OP ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MINUS_OP ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | NE_OP ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | OR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | PLUS_OP ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | SEMICOLON ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | TIMES_OP ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | RBRACE ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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
        | BREAK ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState13
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState18
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState19
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState20
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
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState23
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
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState29
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
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32
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
        | BREAK ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
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
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
    | BREAK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expression) =     ( Break ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

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
                | BREAK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
            | BREAK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState25
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

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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
  

