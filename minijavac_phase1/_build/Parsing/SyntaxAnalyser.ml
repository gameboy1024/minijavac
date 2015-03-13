exception Error

type token = 
  | VAR of (string)
  | TYPE of (string)
  | THIS
  | STRING of (string)
  | STATIC
  | SEMICOLON
  | RPAREN
  | RBRACE
  | PLUS
  | OR
  | NULL
  | NOT
  | NEW
  | NE
  | MULTI
  | MULLINECOMMENT of (string)
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LE
  | LBRACE
  | INT of (int)
  | INSTANCEOF
  | INLINECOMMENT of (string)
  | IN
  | IF
  | GT
  | GE
  | EXTENDS
  | EQ
  | EOL
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
  | BOOL of (bool)
  | ASSIGN
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState151
  | MenhirState147
  | MenhirState146
  | MenhirState143
  | MenhirState140
  | MenhirState136
  | MenhirState133
  | MenhirState129
  | MenhirState125
  | MenhirState124
  | MenhirState122
  | MenhirState121
  | MenhirState117
  | MenhirState116
  | MenhirState114
  | MenhirState110
  | MenhirState109
  | MenhirState107
  | MenhirState106
  | MenhirState102
  | MenhirState101
  | MenhirState97
  | MenhirState94
  | MenhirState91
  | MenhirState87
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState69
  | MenhirState68
  | MenhirState66
  | MenhirState62
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
  | MenhirState44
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState16
  | MenhirState15
  | MenhirState12
  | MenhirState8
  | MenhirState5
  | MenhirState3
  | MenhirState0

  
  (* Accessing the AST type *)
  open Expression
let _eRR =
  Error

let rec _menhir_goto_list_attribute_or_method_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.attr_or_method list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let rest = _v in
        let (_menhir_stack, _menhir_s, a) = _menhir_stack in
        let _v : (Expression.attr_or_method list) =       ( a :: rest ) in
        _menhir_goto_attributes_or_methods _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Expression.attr_or_method list) =     ( x :: xs ) in
        _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Expression.attr_or_method list) =     ( [] ) in
    _menhir_goto_list_attribute_or_method_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Expression.param list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_param_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let rest = _v in
        let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
        let _v : (Expression.param list) =       ( Param(t,id) :: rest ) in
        _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_attribute_or_method : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.attr_or_method) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState140 | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | STATIC | TYPE _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | RBRACE ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState136 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | STATIC | TYPE _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RBRACE ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | _ ->
        _menhir_fail ()

and _menhir_goto_params : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOL _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | INLINECOMMENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | MULLINECOMMENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | NEW ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | NOT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | NULL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | STRING _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | THIS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | TYPE _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | VAR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOL _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | INLINECOMMENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | MULLINECOMMENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | NEW ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NOT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | NULL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | STRING _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | THIS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                | TYPE _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | VAR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id = _v in
        let (_menhir_stack, _menhir_s, t) = _menhir_stack in
        let _v : (Expression.param) =       ( Param(t,id) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TYPE _v ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Expression.param list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_param_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce51 : _menhir_env -> ('ttv_tail * _menhir_state * (Expression.class_or_expr)) * _menhir_state * ( Expression.class_or_expr list ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, e), _, rest) = _menhir_stack in
    let _v : ( Expression.class_or_expr list ) =                                                 ( e::rest ) in
    _menhir_goto_file_content _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_class_or_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.class_or_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | BOOL _ | CLASS | EOF | IF | INT _ | LPAREN | MINUS | NEW | NOT | NULL | STRING _ | THIS | TYPE _ | VAR _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
    | MenhirState151 | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | BOOL _ | CLASS | EOF | IF | INT _ | LPAREN | MINUS | NEW | NOT | NULL | STRING _ | THIS | TYPE _ | VAR _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_ : _menhir_env -> 'ttv_tail -> (Expression.mthd) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let m = _v in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    let _v : (Expression.attr_or_method) =       ( Meth(m) ) in
    _menhir_goto_attribute_or_method _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, e), _), _, rest) = _menhir_stack in
        let _v : (Expression.expression list) =                            ( e :: rest ) in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, e), _), mthd), _, args_) = _menhir_stack in
            let _v : (Expression.expression) =       ( Invoke(e, mthd, args_) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Expression.expression list) =     ( [] ) in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let t = _v in
        let ((_menhir_stack, _menhir_s, e), _) = _menhir_stack in
        let _v : (Expression.expression) =       ( Instanceof(e, t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | NEW ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | NOT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | STRING _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | THIS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TYPE _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | VAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | RPAREN ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Expression.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_class_body : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.class_) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let c = _v in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    let _v : (Expression.class_or_expr) =                           ( Class(c) ) in
    _menhir_goto_class_or_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_attribute : _menhir_env -> 'ttv_tail -> (Expression.attr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), a) = _menhir_stack in
        let _v : (Expression.attr_or_method) =       ( Attr(a) ) in
        _menhir_goto_attribute_or_method _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | TYPE _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
            let _v : (Expression.param list) =       ( [Param(t,id)] ) in
            _menhir_goto_params _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_file_content : _menhir_env -> 'ttv_tail -> _menhir_state -> ( Expression.class_or_expr list ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        Obj.magic _1
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | RBRACE | RPAREN | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bsemicolon )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Badd )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bmul )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState62 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState36 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | NEW ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NOT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | STRING _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | THIS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | TYPE _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | VAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | RPAREN ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (Expression.expression list) =            ( [e] ) in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bor )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | IF | IN | INT _ | LPAREN | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bne )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bmod )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bsub )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bdiv )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Blt )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Ble )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bgt )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Bge )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | AND | BOOL _ | CLASS | COMMA | EOF | EQ | IF | IN | INT _ | LPAREN | NE | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Beq )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | AND | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | OR | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) = let o =
                            ( Band )
            in
                  ( Binop(o,e1,e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOL _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | INLINECOMMENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | MULLINECOMMENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | NEW ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | NOT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | NULL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | STRING _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | THIS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | TYPE _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | VAR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState69 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | IF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | INLINECOMMENT _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | INT _v ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | LPAREN ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | MINUS ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | MULLINECOMMENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | NEW ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | NOT ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | NULL ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | STRING _v ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | THIS ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
                    | TYPE _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | VAR _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState73 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, cond), _), _, eif), _), _, eelse) = _menhir_stack in
            let _v : (Expression.expression) =       ( Ifelse(cond, eif, eelse) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | RBRACE | RPAREN | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, t), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( Cast(t, e) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( Unop(Uminus,e) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | EOF | EQ | GE | GT | IF | IN | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( Unop(Unot,e) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState80 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | NEW ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NOT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | STRING _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | THIS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TYPE _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | VAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, t), id), _, e1), _), _, e2) = _menhir_stack in
            let _v : (Expression.expression) =       ( Def(t, id, e1, e2) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _ | CLASS | COMMA | EOF | IF | IN | INT _ | LPAREN | NEW | NOT | NULL | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, id), _, e) = _menhir_stack in
            let _v : (Expression.expression) =       ( Assign(id, e) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState102 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, t), id), _), _, e) = _menhir_stack in
            let _v : (Expression.mthd) =       ( Method(false, t, id, [], e) ) in
            _menhir_goto_method_ _menhir_env _menhir_stack _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, t), id), _, p), _, e) = _menhir_stack in
            let _v : (Expression.mthd) =       ( Method(false, t, id, p, e) ) in
            _menhir_goto_method_ _menhir_env _menhir_stack _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, t), id), _, e) = _menhir_stack in
            let _v : (Expression.attr) =       ( AttributeWithAssign(false, t, id, e) ) in
            _menhir_goto_attribute _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState117 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, t), id), _), _, e) = _menhir_stack in
            let _v : (Expression.mthd) =       ( Method(true, t, id, [], e) ) in
            _menhir_goto_method_ _menhir_env _menhir_stack _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState122 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, t), id), _, p), _, e) = _menhir_stack in
            let _v : (Expression.mthd) =       ( Method(true, t, id, p, e) ) in
            _menhir_goto_method_ _menhir_env _menhir_stack _v
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, t), id), _, e) = _menhir_stack in
            let _v : (Expression.attr) =       ( AttributeWithAssign(true, t, id, e) ) in
            _menhir_goto_attribute _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState147 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | EQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | GE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | GT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | INSTANCEOF ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MOD ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | MULTI ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | OR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SEMICOLON ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | BOOL _ | CLASS | EOF | IF | INT _ | LPAREN | NEW | NOT | NULL | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, e) = _menhir_stack in
            let _v : (Expression.class_or_expr) =                     ( Expr(e) ) in
            _menhir_goto_class_or_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | _ ->
        _menhir_fail ()

and _menhir_goto_attributes_or_methods : _menhir_env -> 'ttv_tail -> _menhir_state -> (Expression.attr_or_method list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), t), _, a) = _menhir_stack in
            let _v : (Expression.class_) =       ( Class_(t, a) ) in
            _menhir_goto_class_body _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), t), st), _, a) = _menhir_stack in
            let _v : (Expression.class_) =       ( ClassWithExtends(t, st, a) ) in
            _menhir_goto_class_body _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run89 : _menhir_env -> 'ttv_tail -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | NEW ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | NOT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | STRING _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | THIS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | TYPE _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | VAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState91 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | BOOL _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | IF ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | INLINECOMMENT _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | INT _v ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | LPAREN ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | MINUS ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | MULLINECOMMENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | NEW ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | NOT ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | NULL ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | STRING _v ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | THIS ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                    | TYPE _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | VAR _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TYPE _v ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, t), id) = _menhir_stack in
            let _v : (Expression.attr) =       ( Attribute(false, t, id) ) in
            _menhir_goto_attribute _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run111 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOL _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | IF ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | INLINECOMMENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | MINUS ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | MULLINECOMMENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | NEW ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | NOT ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | NULL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | STRING _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | THIS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TYPE _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | VAR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState114 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | LBRACE ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | BOOL _v ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | IF ->
                            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | INLINECOMMENT _v ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | INT _v ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | LPAREN ->
                            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | MINUS ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | MULLINECOMMENT _v ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | NEW ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | NOT ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | NULL ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | STRING _v ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | THIS ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                        | TYPE _v ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | VAR _v ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | TYPE _v ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, t), id) = _menhir_stack in
                let _v : (Expression.attr) =       ( Attribute(true, t, id) ) in
                _menhir_goto_attribute _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | AND | BOOL _ | CLASS | COMMA | DIV | DOT | EOF | EQ | GE | GT | IF | IN | INLINECOMMENT _ | INSTANCEOF | INT _ | LE | LPAREN | LT | MINUS | MOD | MULLINECOMMENT _ | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (Expression.expression) =       ( Var id ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | VAR _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Expression.expression) =       ( This ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let _v : (Expression.expression) =       ( String s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Expression.expression) =       ( Null ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let t = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Expression.expression) =       ( New(t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TYPE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | NEW ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NULL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | THIS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | IF ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | MINUS ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | NEW ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | NOT ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | NULL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | STRING _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | THIS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | TYPE _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | VAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | VAR _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | VAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (Expression.expression) =       ( Int i ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce50 : _menhir_env -> 'ttv_tail * _menhir_state * (unit list) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    let _v : ( Expression.class_or_expr list ) =                  ([]) in
    _menhir_goto_file_content _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | TYPE _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INLINECOMMENT _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
                    | MULLINECOMMENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
                    | RBRACE | STATIC | TYPE _ ->
                        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INLINECOMMENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | MULLINECOMMENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | RBRACE | STATIC | TYPE _ ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _v : (Expression.expression) =       ( Bool b ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_nonempty_list_comment_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 | MenhirState3 | MenhirState124 | MenhirState121 | MenhirState116 | MenhirState109 | MenhirState106 | MenhirState101 | MenhirState5 | MenhirState81 | MenhirState8 | MenhirState12 | MenhirState15 | MenhirState16 | MenhirState18 | MenhirState72 | MenhirState68 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState23 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState143 | MenhirState125 | MenhirState122 | MenhirState117 | MenhirState110 | MenhirState107 | MenhirState102 | MenhirState83 | MenhirState80 | MenhirState82 | MenhirState79 | MenhirState78 | MenhirState76 | MenhirState75 | MenhirState66 | MenhirState69 | MenhirState73 | MenhirState24 | MenhirState26 | MenhirState28 | MenhirState30 | MenhirState36 | MenhirState38 | MenhirState61 | MenhirState59 | MenhirState40 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState51 | MenhirState47 | MenhirState49 | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e), _, _) = _menhir_stack in
        let _v : (Expression.expression) =       ( e ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_comment_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_comment_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | CLASS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState3 in
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | MenhirState140 | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STATIC ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | TYPE _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) _v
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            let _v : (Expression.attr_or_method list) =               ( [] ) in
            _menhir_goto_attributes_or_methods _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_comment_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STATIC ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | TYPE _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | CLASS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState147 in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | IF ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MINUS ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | NEW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NOT ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NULL ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | STRING _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | THIS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | TYPE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | VAR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | _ ->
        _menhir_fail ()

and _menhir_goto_comment : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 | MenhirState3 | MenhirState143 | MenhirState124 | MenhirState125 | MenhirState121 | MenhirState122 | MenhirState116 | MenhirState117 | MenhirState109 | MenhirState110 | MenhirState106 | MenhirState107 | MenhirState101 | MenhirState102 | MenhirState5 | MenhirState83 | MenhirState8 | MenhirState80 | MenhirState81 | MenhirState82 | MenhirState12 | MenhirState79 | MenhirState15 | MenhirState78 | MenhirState16 | MenhirState76 | MenhirState18 | MenhirState75 | MenhirState21 | MenhirState66 | MenhirState68 | MenhirState69 | MenhirState72 | MenhirState73 | MenhirState23 | MenhirState24 | MenhirState25 | MenhirState26 | MenhirState27 | MenhirState28 | MenhirState29 | MenhirState30 | MenhirState35 | MenhirState36 | MenhirState62 | MenhirState37 | MenhirState38 | MenhirState60 | MenhirState61 | MenhirState58 | MenhirState59 | MenhirState39 | MenhirState40 | MenhirState56 | MenhirState57 | MenhirState54 | MenhirState55 | MenhirState52 | MenhirState53 | MenhirState50 | MenhirState51 | MenhirState46 | MenhirState47 | MenhirState48 | MenhirState49 | MenhirState41 | MenhirState44 | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | AND | BOOL _ | CLASS | COMMA | DIV | DOT | EOF | EQ | GE | GT | IF | IN | INSTANCEOF | INT _ | LE | LPAREN | LT | MINUS | MOD | MULTI | NE | NEW | NOT | NULL | OR | PLUS | RBRACE | RPAREN | SEMICOLON | STRING _ | THIS | TYPE _ | VAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_comment_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState151 | MenhirState146 | MenhirState0 | MenhirState140 | MenhirState136 | MenhirState133 | MenhirState129 | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINECOMMENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | MULLINECOMMENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | BOOL _ | CLASS | EOF | IF | INT _ | LPAREN | MINUS | NEW | NOT | NULL | RBRACE | STATIC | STRING _ | THIS | TYPE _ | VAR _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState38 ->
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
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_comment_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =                    () in
    _menhir_goto_comment _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =                   () in
    _menhir_goto_comment _menhir_env _menhir_stack _menhir_s _v

and file_content : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( Expression.class_or_expr list ) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INLINECOMMENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MULLINECOMMENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | BOOL _ | CLASS | EOF | IF | INT _ | LPAREN | MINUS | NEW | NOT | NULL | STRING _ | THIS | TYPE _ | VAR _ ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)




