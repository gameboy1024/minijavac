exception Error

type token = 
  | UIDENT of (string)
  | TRUE
  | TIMES
  | THIS
  | STRING of (string)
  | SEMI
  | RPAREN
  | RBRACE
  | PLUS
  | OR
  | NULL
  | NOT
  | NEW
  | NEQ
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LIDENT of (string)
  | LE
  | LBRACE
  | INT of (int)
  | INSTANCEOF
  | IN
  | IF
  | GT
  | GE
  | FALSE
  | EXTENDS
  | EQUAL
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
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
  | MenhirState116
  | MenhirState112
  | MenhirState109
  | MenhirState103
  | MenhirState101
  | MenhirState95
  | MenhirState83
  | MenhirState79
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState43
  | MenhirState39
  | MenhirState35
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState27
  | MenhirState24
  | MenhirState19
  | MenhirState16
  | MenhirState10
  | MenhirState7
  | MenhirState0

  
  open AST
  open Location

  type attOrMethod =
    | Att of astattribute
    | Meth of astmethod

  let mkexp d s e = 
    { 
      edesc = d;
      eloc  = symbol_loc s e;
      etype = None;
    }

  let mkval v s e = 
    { 
      edesc = Val v;
      eloc  = symbol_loc s e;
      etype = None;
    }

  let mkexploc d loc = 
    { 
      edesc = d;
      eloc  = loc;
      etype = None;
    }

  let mkclass n p a m s e = 
    { 
      cname = n;
      cparent = p;
      cattributes = a;
      cmethods = m;
      cloc  = symbol_loc s e;
    }

  let mkatt n t d s e = 
    {
      aname = n;
      atype = t;
      adefault = d;
      aloc  = symbol_loc s e;
    }

  let mkmeth n r a b s e =
    {
      mname = n;
      mreturntype = r;
      margstype = a;
      mbody = b;
      mloc  = symbol_loc s e;
    }

  let mk_type ty s e =
    Located.mk_elem (Type.fromString ty) (symbol_loc s e)

let _eRR =
  Error

let rec _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__6_ = _endpos in
        let ((((_menhir_stack, _menhir_s, o, _startpos_o_, _endpos_o_), m, _startpos_m_, _endpos_m_), _startpos__4_), _, xs0) = _menhir_stack in
        let _startpos = _startpos_o_ in
        let _endpos = _endpos__6_ in
        let _v : (AST.expression) = let params =
          let xs = xs0 in
              ( xs )
        in
              ( mkexp (Call(o,m,params)) _startpos _endpos ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_else_cond : _menhir_env -> 'ttv_tail -> (AST.expression option) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _8 = _v in
    let _endpos__8_ = _endpos in
    let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _3, _startpos__3_, _endpos__3_), _endpos__4_), _, _6, _startpos__6_, _endpos__6_), _endpos__7_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__8_ in
    let _v : (AST.expression) =       ( match _8 with
        | Some e -> mkexp (If(_3, _6, Some e)) _startpos _endpos 
        | None -> mkexp (If(_3, _6, None)) _startpos _endpos 
      ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (AST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (AST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let _startpos__3_ = _startpos in
        let _endpos__3_ = _endpos in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_, _endpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (AST.expression) =       ( 
	let typ = Located.mk_elem (Type.fromString _3) (symbol_loc (_startpos__3_) (_endpos__3_)) in
	mkexp (Instanceof(_1,typ)) _startpos _endpos 
      ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LIDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STRING _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | THIS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | UIDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState43 in
                let _v : (AST.expression list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (AST.expression) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_attributeOrMethod : _menhir_env -> 'ttv_tail -> _menhir_state -> (attOrMethod) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | RBRACE ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_goto_separated_nonempty_list_COMMA_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.argument list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (AST.argument list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (AST.argument list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LIDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STRING _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | THIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TRUE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | UIDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.expression) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | IF ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
                | INT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LIDENT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LPAREN ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
                | MINUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
                | NOT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | STRING _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | THIS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | TRUE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | UIDENT _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "mul" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LIDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STRING _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | THIS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | UIDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (AST.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EOF | IN | RBRACE | RPAREN | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_, _endpos__1_), _, _3, _startpos__3_, _endpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (AST.expression) =       ( mkexp (Seq(_1,_3)) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "add" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "mod" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "div" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EOF | IN | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "or" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | IN | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "neq" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _startpos__2_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "sub" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "lt" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "le" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "gt" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "ge" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | EQ | IN | NEQ | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "eq" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EOF | IN | OR | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (AST.expression) = let op =
                             ( "and" )
            in
                  ( mkexp (Call(e1,op,[e2])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
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
                    | FALSE ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | IF ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
                    | INT _v ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LIDENT _v ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LPAREN ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
                    | MINUS ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
                    | NOT ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
                    | NULL ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | STRING _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | THIS ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TRUE ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | UIDENT _v ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | AND | COMMA | DIV | DOT | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_startp in
                let _v : (AST.expression option) =       ( None ) in
                _menhir_goto_else_cond _menhir_env _menhir_stack _v _endpos
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let (_menhir_stack, _, _3, _startpos__3_, _endpos__3_) = _menhir_stack in
            let _endpos = _endpos__4_ in
            let _v : (AST.expression option) =       ( Some _3 ) in
            _menhir_goto_else_cond _menhir_env _menhir_stack _v _endpos
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EOF | IN | RBRACE | RPAREN | SEMI | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_, _endpos__1_), _, _3, _startpos__3_, _endpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (AST.expression) =       ( mkexp (Assign(_1,_3)) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, _2, _startpos__2_, _endpos__2_), _endpos__3_), _, _4, _startpos__4_, _endpos__4_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : (AST.expression) =       ( 
	let typ = Located.mk_elem (Type.fromString _2) (symbol_loc (_startpos__2_) (_endpos__2_)) in
	mkexp (Cast(typ,_4)) _startpos _endpos
      ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2, _startpos__2_, _endpos__2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (AST.expression) =       ( _2 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2, _startpos__2_, _endpos__2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : (AST.expression) =       ( mkexp (Call(_2,"neg",[])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2, _startpos__2_, _endpos__2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : (AST.expression) =       ( mkexp (Call(_2,"not",[])) _startpos _endpos ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | IF ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
                | INT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LIDENT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LPAREN ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
                | MINUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
                | NOT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | STRING _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | THIS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | TRUE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | UIDENT _v ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _1, _startpos__1_, _endpos__1_), _2, _startpos__2_, _endpos__2_), _, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (AST.expression) =       ( 
	let typ = Located.mk_elem (Type.fromString _1) (symbol_loc (_startpos__1_) (_endpos__1_)) in
	mkexp (Define(_2,typ,e1,e2)) _startpos _endpos
      ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let ((((((_menhir_stack, _menhir_s, typ, _startpos_typ_, _endpos_typ_), name, _startpos_name_, _endpos_name_), _startpos__3_), _, xs0), _endpos__5_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos_typ_ in
            let _endpos = _endpos__8_ in
            let _v : (attOrMethod) = let args =
              let xs = xs0 in
                  ( xs )
            in
                  (
	let typ = Located.mk_elem (Type.fromString typ) (symbol_loc (_startpos_typ_) (_endpos_typ_)) in
	Meth(mkmeth name typ args e _startpos _endpos )
      ) in
            _menhir_goto_attributeOrMethod _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, x0, _startpos_x0_, _endpos_x0_) = _menhir_stack in
            let _v : (AST.expression option) = let x =
              let x = x0 in
                  ( x )
            in
                ( Some x ) in
            _menhir_goto_option_preceded_EQUAL_expression__ _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | INSTANCEOF ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MOD ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (AST.expression option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.value) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.expression) =       ( mkval _1 _startpos _endpos ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_list_attributeOrMethod_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (attOrMethod list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let aOml = _v in
        let _v : (AST.astattribute list * AST.astmethod list) =     (
      List.fold_left 
      (fun (al,ml) aOm -> 
	match aOm with
	  | Att a -> a::al,ml
	  | Meth m -> al,m::ml) 
      ([],[]) 
      (List.rev aOml)
    ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__6_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), name, _startpos_name_, _endpos_name_), p), _, cb) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__6_ in
            let _v : (AST.astclass) =       ( 
	let al,ml = cb in
	let ext = match p with
	  | Some t -> t 
	  | None -> Located.mk_elem (Type.fromString "Object") Location.none in
	mkclass name ext al ml _startpos _endpos
      ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
            | EOF | FALSE | IF | INT _ | LIDENT _ | LPAREN | MINUS | NEW | NOT | NULL | STRING _ | THIS | TRUE | UIDENT _ ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (attOrMethod list) =     ( x :: xs ) in
        _menhir_goto_list_attributeOrMethod_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_preceded_EQUAL_expression__ : _menhir_env -> 'ttv_tail -> (AST.expression option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let initialization = _v in
    let ((_menhir_stack, _menhir_s, typ, _startpos_typ_, _endpos_typ_), name, _startpos_name_, _endpos_name_) = _menhir_stack in
    let _v : (attOrMethod) =       (
	let typ = Located.mk_elem (Type.fromString typ) (symbol_loc (_startpos_typ_) (_endpos_typ_)) in
	Att(mkatt name typ initialization _startpos_typ_ _endpos_name_)
      ) in
    _menhir_goto_attributeOrMethod _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.argument list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LIDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STRING _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | THIS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | UIDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
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

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let name = _v in
        let _startpos_name_ = _startpos in
        let _endpos_name_ = _endpos in
        let (_menhir_stack, _menhir_s, typ, _startpos_typ_, _endpos_typ_) = _menhir_stack in
        let _v : (AST.argument) =     ( 
      let typ = Located.mk_elem (Type.fromString typ) (symbol_loc (_startpos_typ_) (_endpos_typ_)) in
      name,typ
    ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | UIDENT _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (AST.argument list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
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
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.expression option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, cl), _, e) = _menhir_stack in
        let _v : (AST.t) =                                      ( cl, e ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.value) =            ( Boolean true ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.expression) =       ( mkexp (Var "this") _startpos _endpos) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.value) =            ( String _1 ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.value) =            ( Null ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _startpos__2_ = _startpos in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (AST.expression) =       ( 
	let typ = Located.mk_elem (Type.fromString _2) (symbol_loc (_startpos__2_) (_endpos__2_)) in
	mkexp (New typ) _startpos _endpos 
      ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LIDENT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STRING _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | THIS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TRUE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState28 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LIDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LIDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STRING _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | THIS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | UIDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LIDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STRING _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | THIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TRUE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | UIDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | AND | COMMA | DIV | DOT | EOF | EQ | GE | GT | IN | INSTANCEOF | LE | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACE | RPAREN | SEMI | TIMES | UIDENT _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_, _endpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (AST.expression) =       ( mkexp (Var _1) _startpos _endpos ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.value) =            ( Int _1 ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LIDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STRING _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | THIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TRUE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | UIDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (AST.value) =            ( Boolean false ) in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (attOrMethod list) =     ( [] ) in
    _menhir_goto_list_attributeOrMethod_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LIDENT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STRING _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | THIS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TRUE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | UIDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | UIDENT _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState10 in
                let _v : (AST.argument list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | RBRACE | UIDENT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (AST.expression option) =     ( None ) in
            _menhir_goto_option_preceded_EQUAL_expression__ _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_aclass_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (AST.astclass list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LIDENT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STRING _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | THIS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TRUE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | UIDENT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState112 in
            let _v : (AST.expression option) =     ( None ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (AST.astclass list) =     ( x :: xs ) in
        _menhir_goto_list_aclass_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_parent_ : _menhir_env -> 'ttv_tail -> (Type.t Located.t option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | UIDENT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | RBRACE ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (AST.astclass list) =     ( [] ) in
    _menhir_goto_list_aclass_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | UIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _2 = _v in
                let _startpos__2_ = _startpos in
                let _endpos__2_ = _endpos in
                let _v : (Type.t Located.t) =                    ( mk_type _2 (_startpos__2_) (_endpos__2_) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _v : (Type.t Located.t option) =     ( Some x ) in
                _menhir_goto_option_parent_ _menhir_env _menhir_stack _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Type.t Located.t option) =     ( None ) in
            _menhir_goto_option_parent_ _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AST.t) =
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
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | EOF | FALSE | IF | INT _ | LIDENT _ | LPAREN | MINUS | NEW | NOT | NULL | STRING _ | THIS | TRUE | UIDENT _ ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



