
exception Error

let _eRR =
  Error

type token = 
  | WITH
  | UNDERSCORE
  | THEN
  | STAR_DOT
  | STAR
  | SLASH_DOT
  | SLASH
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | PLUS_DOT
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS_GT
  | MINUS_DOT
  | MINUS
  | MATCH
  | LPAREN
  | LET
  | LBRACKET
  | IN
  | IF
  | IDENT of (
# 25 "parser.mly"
       (string)
# 37 "parser.ml"
)
  | FUNCTION
  | EQUAL
  | EOF
  | ELSE
  | CONST_STRING of (
# 20 "parser.mly"
       (string)
# 46 "parser.ml"
)
  | CONST_INT of (
# 18 "parser.mly"
       (int)
# 51 "parser.ml"
)
  | CONST_FLOAT of (
# 19 "parser.mly"
       (float)
# 56 "parser.ml"
)
  | CONST_BOOL of (
# 17 "parser.mly"
       (bool)
# 61 "parser.ml"
)
  | COMP of (
# 16 "parser.mly"
       (Ast.binop)
# 66 "parser.ml"
)
  | COMMA
  | COLONCOLON
  | BAR
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState114
  | MenhirState108
  | MenhirState105
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState84
  | MenhirState77
  | MenhirState72
  | MenhirState70
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState37
  | MenhirState35
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState13
  | MenhirState10
  | MenhirState5
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "parser.mly"
  

  open Ast

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }
  let mk_def d = { pdef_desc = d; pdef_loc = loc () }


# 138 "parser.ml"

let rec _menhir_goto_binding : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_binding -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * 'tv_binding) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv429 * _menhir_state) * _menhir_state * 'tv_binding) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv431 * _menhir_state) * _menhir_state * 'tv_binding) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state) * _menhir_state * 'tv_binding) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * _menhir_state) * _menhir_state * 'tv_binding) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_def = 
# 82 "parser.mly"
    ( let is_rec, patt, body = _2 in
      mk_def (is_rec, patt, body) )
# 206 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_def) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LET ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | EOF ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv436)) : 'freshtv438)) : 'freshtv440)) : 'freshtv442)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_bar : _menhir_env -> 'ttv_tail -> 'tv_opt_bar -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv427 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv423 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv419 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | MINUS_GT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv415 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | CONST_BOOL _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | CONST_FLOAT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | CONST_INT _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | CONST_STRING _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | FUNCTION ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | IDENT _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | LBRACKET ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | LET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | MATCH ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | MINUS ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | MINUS_DOT ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | NOT ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv416)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv417 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv421 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv425 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)

and _menhir_goto_expr_comma_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_comma_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv409 * _menhir_state) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * _menhir_state) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv403 * _menhir_state) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_expr = 
# 113 "parser.mly"
    ( mk_expr (PE_tuple _2) )
# 331 "parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv407 * _menhir_state) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr_comma_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr_comma_list = 
# 201 "parser.mly"
    ( _1 :: _3 )
# 351 "parser.ml"
         in
        _menhir_goto_expr_comma_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)) : 'freshtv414)
    | _ ->
        _menhir_fail ()

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> (
# 16 "parser.mly"
       (Ast.binop)
# 816 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_goto_expr_semi_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_semi_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv397 * _menhir_state) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv393 * _menhir_state) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv391 * _menhir_state) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_expr = 
# 115 "parser.mly"
    ( List.fold_right 
	(fun e acc -> mk_expr (PE_cons (e, acc))) _2 (mk_expr PE_nil) )
# 955 "parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv395 * _menhir_state) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr_semi_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr_semi_list = 
# 212 "parser.mly"
    ( _1 :: _3 )
# 975 "parser.ml"
         in
        _menhir_goto_expr_semi_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_expr_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_expr_list) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_simple_expr_list) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_expr = 
# 123 "parser.mly"
    ( List.fold_left (fun acc e -> mk_expr (PE_app (acc, e))) _1 _2 )
# 997 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_expr_list) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_simple_expr_list) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_simple_expr_list = 
# 195 "parser.mly"
                                            ( _1 :: _2 )
# 1013 "parser.ml"
         in
        _menhir_goto_simple_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)) : 'freshtv390)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv209 * _menhir_state) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | EOF | IN | LET | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv205 * _menhir_state) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 130 "parser.mly"
    ( mk_expr (PE_fun (_2, _4)) )
# 1066 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv207 * _menhir_state) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 148 "parser.mly"
    ( mk_expr (PE_binop (Bmul_f, _1, _3)) )
# 1086 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)) : 'freshtv214)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 146 "parser.mly"
    ( mk_expr (PE_binop (Bmul, _1, _3)) )
# 1099 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 152 "parser.mly"
    ( mk_expr (PE_binop (Bdiv_f, _1, _3)) )
# 1112 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 150 "parser.mly"
    ( mk_expr (PE_binop (Bdiv, _1, _3)) )
# 1125 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 140 "parser.mly"
    ( mk_expr (PE_binop (Badd_f, _1, _3)) )
# 1150 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 138 "parser.mly"
    ( mk_expr (PE_binop (Badd, _1, _3)) )
# 1182 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | EOF | IN | LET | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 162 "parser.mly"
    ( mk_expr (PE_binop (Bor, _1, _3)) )
# 1232 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | NEQ | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 158 "parser.mly"
    ( mk_expr (PE_binop (Bneq, _1, _3)) )
# 1274 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 144 "parser.mly"
    ( mk_expr (PE_binop (Bsub_f, _1, _3)) )
# 1306 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 142 "parser.mly"
    ( mk_expr (PE_binop (Bsub, _1, _3)) )
# 1338 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | NEQ | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 125 "parser.mly"
    ( mk_expr (PE_cons (_1, _3)) )
# 1380 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | NEQ | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 156 "parser.mly"
    ( mk_expr (PE_binop (Beq, _1, _3)) )
# 1422 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_expr) * (
# 16 "parser.mly"
       (Ast.binop)
# 1437 "parser.ml"
        )) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | NEQ | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_expr) * (
# 16 "parser.mly"
       (Ast.binop)
# 1465 "parser.ml"
            )) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
            let _v : 'tv_expr = 
# 154 "parser.mly"
    ( mk_expr (PE_binop (_2, _1, _3)) )
# 1471 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_expr) * (
# 16 "parser.mly"
       (Ast.binop)
# 1481 "parser.ml"
            )) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAR | COMMA | ELSE | EOF | IN | LET | OR | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 160 "parser.mly"
    ( mk_expr (PE_binop (Band, _1, _3)) )
# 1523 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv293 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv294)
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv295 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv303 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | EOF | IN | LET | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv299 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, _2), _, _4), _, _6) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 132 "parser.mly"
    ( mk_expr (PE_if (_2, _4, _6)) )
# 1735 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv301 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState77 | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | RBRACKET ->
                _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv306)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_expr_semi_list = 
# 210 "parser.mly"
    ( [_1] )
# 1826 "parser.ml"
             in
            _menhir_goto_expr_semi_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv317 * _menhir_state) * _menhir_state * 'tv_binding)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | EOF | IN | LET | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv313 * _menhir_state) * _menhir_state * 'tv_binding)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 127 "parser.mly"
    ( let is_rec, patt, body = _2 in
      mk_expr (PE_let (is_rec, patt, body, _4)) )
# 1880 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv315 * _menhir_state) * _menhir_state * 'tv_binding)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv321 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_expr = 
# 107 "parser.mly"
    ( _2 )
# 1930 "parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv323 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr_comma_list = 
# 203 "parser.mly"
    ( [_1; _3] )
# 1992 "parser.ml"
             in
            _menhir_goto_expr_comma_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv345 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
                ((let _1 = () in
                let _v : 'tv_opt_bar = 
# 191 "parser.mly"
               ( () )
# 2052 "parser.ml"
                 in
                _menhir_goto_opt_bar _menhir_env _menhir_stack _v) : 'freshtv334)) : 'freshtv336)
            | LBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
                ((let _v : 'tv_opt_bar = 
# 190 "parser.mly"
               ( () )
# 2061 "parser.ml"
                 in
                _menhir_goto_opt_bar _menhir_env _menhir_stack _v) : 'freshtv338)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv339 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv351 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv347 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | UNDERSCORE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv348)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv349 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((((('freshtv357 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | EOF | IN | LET | RBRACKET | RPAREN | SEMI | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((((('freshtv353 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s), _, _2), _4), _, _8), _, _10), _, _12), _, _14) = _menhir_stack in
            let _13 = () in
            let _11 = () in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 136 "parser.mly"
    ( mk_expr (PE_match (_2, _8, (_10, _12, _14))) )
# 2184 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((((('freshtv355 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)) : 'freshtv358)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv359 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 164 "parser.mly"
    ( mk_expr (PE_unop (Uminus, _2)) )
# 2204 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 166 "parser.mly"
    ( mk_expr (PE_unop (Uminus_f, _2)) )
# 2217 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 168 "parser.mly"
    ( mk_expr (PE_unop (Unot, _2)) )
# 2230 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv375 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2238 "parser.ml"
        )) * _menhir_state * 'tv_pattern_list)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EOF | IN | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv371 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2276 "parser.ml"
            )) * _menhir_state * 'tv_pattern_list)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, _1), _, _2), _, _3), _, _5) = _menhir_stack in
            let _4 = () in
            let _v : 'tv_binding = 
# 90 "parser.mly"
    ( let body = 
         List.fold_right (fun patt e -> mk_expr (PE_fun(patt, e))) _3 _5
      in
      (_1, mk_patt (PP_ident _2), body) )
# 2286 "parser.ml"
             in
            _menhir_goto_binding _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv373 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2296 "parser.ml"
            )) * _menhir_state * 'tv_pattern_list)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)) : 'freshtv376)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv381 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | COLONCOLON ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMP _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | EQUAL ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS_DOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | PLUS_DOT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | SLASH_DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | STAR_DOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EOF | IN | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv377 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _4) = _menhir_stack in
            let _3 = () in
            let _v : 'tv_binding = 
# 88 "parser.mly"
    ( (_1, _2, _4) )
# 2342 "parser.ml"
             in
            _menhir_goto_binding _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv379 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | _ ->
        _menhir_fail ()

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_expr_semi_list = 
# 208 "parser.mly"
    ( [] )
# 2360 "parser.ml"
     in
    _menhir_goto_expr_semi_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState108 | MenhirState15 | MenhirState16 | MenhirState17 | MenhirState18 | MenhirState99 | MenhirState93 | MenhirState19 | MenhirState84 | MenhirState20 | MenhirState24 | MenhirState77 | MenhirState25 | MenhirState72 | MenhirState70 | MenhirState26 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState51 | MenhirState49 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | CONST_FLOAT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | IDENT _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LBRACKET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | SLASH | SLASH_DOT | STAR | STAR_DOT | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_expr = 
# 121 "parser.mly"
    ( _1 )
# 2395 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv200)
    | MenhirState37 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST_BOOL _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | CONST_FLOAT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | CONST_INT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | CONST_STRING _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | IDENT _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | LBRACKET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | AND | BAR | COLONCOLON | COMMA | COMP _ | ELSE | EOF | EQUAL | IN | LET | MINUS | MINUS_DOT | NEQ | OR | PLUS | PLUS_DOT | RBRACKET | RPAREN | SEMI | SLASH | SLASH_DOT | STAR | STAR_DOT | THEN | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_simple_expr_list = 
# 196 "parser.mly"
                                            ( [_1] )
# 2429 "parser.ml"
             in
            _menhir_goto_simple_expr_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv204)
    | _ ->
        _menhir_fail ()

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : 'tv_const) = _v in
    ((let _v : 'tv_simple_expr = 
# 109 "parser.mly"
    ( _1 )
# 2452 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)

and _menhir_goto_pattern_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pattern_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2465 "parser.ml"
        )) * _menhir_state * 'tv_pattern_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2475 "parser.ml"
            )) * _menhir_state * 'tv_pattern_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv184)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 2519 "parser.ml"
            )) * _menhir_state * 'tv_pattern_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_pattern_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_pattern_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_pattern_list = 
# 223 "parser.mly"
                                  ( _1 :: _2 )
# 2532 "parser.ml"
         in
        _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)) : 'freshtv192)
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
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
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
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
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState20 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_const = 
# 173 "parser.mly"
    ( mk_expr (PE_cte Cunit) )
# 2742 "parser.ml"
         in
        _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IDENT _ | LPAREN | UNDERSCORE ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | RBRACKET ->
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST_BOOL _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CONST_FLOAT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CONST_INT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CONST_STRING _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | FUNCTION ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IDENT _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LBRACKET ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MATCH ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS_DOT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
       (string)
# 2848 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 25 "parser.mly"
       (string)
# 2858 "parser.ml"
    )) = _v in
    ((let _v : 'tv_simple_expr = 
# 111 "parser.mly"
    ( mk_expr (PE_ident _1) )
# 2863 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | UNDERSCORE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "parser.mly"
       (string)
# 2887 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 20 "parser.mly"
       (string)
# 2897 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 181 "parser.mly"
    ( mk_expr (PE_cte (Cstring _1)) )
# 2902 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "parser.mly"
       (int)
# 2909 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 18 "parser.mly"
       (int)
# 2919 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 177 "parser.mly"
    ( mk_expr (PE_cte (Cint _1)) )
# 2924 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "parser.mly"
       (float)
# 2931 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 19 "parser.mly"
       (float)
# 2941 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 179 "parser.mly"
    ( mk_expr (PE_cte (Cfloat _1)) )
# 2946 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (bool)
# 2953 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 17 "parser.mly"
       (bool)
# 2963 "parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 175 "parser.mly"
    ( mk_expr (PE_cte (Cbool _1)) )
# 2968 "parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)

and _menhir_goto_pattern_comma_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pattern_comma_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159 * _menhir_state) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv157 * _menhir_state) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_pattern = 
# 102 "parser.mly"
    ( mk_patt (PP_tuple _2) )
# 2994 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv161 * _menhir_state) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern_comma_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_pattern_comma_list = 
# 217 "parser.mly"
    ( _1 :: _3 )
# 3014 "parser.ml"
         in
        _menhir_goto_pattern_comma_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)) : 'freshtv168)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_pattern -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | UNDERSCORE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pattern -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)) : 'freshtv122)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv123 * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_pattern_comma_list = 
# 219 "parser.mly"
    ( [_1; _3] )
# 3071 "parser.ml"
             in
            _menhir_goto_pattern_comma_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv125 * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS_GT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv129 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv131 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv139 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONCOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv135 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | UNDERSCORE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv137 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv145 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS_GT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv141 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((((('freshtv143 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | MenhirState105 | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | UNDERSCORE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_pattern_list = 
# 224 "parser.mly"
                                  ( [_1] )
# 3231 "parser.ml"
             in
            _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv150)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CONST_BOOL _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CONST_FLOAT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CONST_INT _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | CONST_STRING _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | FUNCTION ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IDENT _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LBRACKET ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MATCH ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MINUS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MINUS_DOT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NOT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv152)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv153 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)
    | _ ->
        _menhir_fail ()

and _menhir_reduce43 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 25 "parser.mly"
       (string)
# 3295 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : 'tv_pattern = 
# 100 "parser.mly"
    ( mk_patt (PP_ident _1) )
# 3302 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_pattern = 
# 98 "parser.mly"
    ( mk_patt PP_any )
# 3316 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | UNDERSCORE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
       (string)
# 3340 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_rec_flag : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rec_flag -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_rec_flag) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_rec_flag) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState3 in
        let (_v : (
# 25 "parser.mly"
       (string)
# 3367 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | UNDERSCORE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | EQUAL ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv114)
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | UNDERSCORE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv116)

and _menhir_goto_defs : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_defs -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv103 * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _2 = () in
            let _v : (
# 68 "parser.mly"
      (Ast.plets)
# 3414 "parser.ml"
            ) = 
# 72 "parser.mly"
               ( _1 )
# 3418 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv99) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 68 "parser.mly"
      (Ast.plets)
# 3426 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 68 "parser.mly"
      (Ast.plets)
# 3434 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 68 "parser.mly"
      (Ast.plets)
# 3442 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv96)) : 'freshtv98)) : 'freshtv100)) : 'freshtv102)) : 'freshtv104)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv105 * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_def) * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state * 'tv_def) * _menhir_state * 'tv_defs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_defs = 
# 77 "parser.mly"
              ( _1 :: _2 )
# 3461 "parser.ml"
         in
        _menhir_goto_defs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | _ ->
        _menhir_fail ()

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_rec_flag = 
# 185 "parser.mly"
               ( false )
# 3472 "parser.ml"
     in
    _menhir_goto_rec_flag _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_rec_flag = 
# 186 "parser.mly"
               ( true )
# 3486 "parser.ml"
     in
    _menhir_goto_rec_flag _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * 'tv_def) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv5 * _menhir_state * 'tv_rec_flag) * _menhir_state * 'tv_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv9 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) * _menhir_state * 'tv_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv11 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv13 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv15 * _menhir_state) * _menhir_state * 'tv_expr)) * 'tv_opt_bar)))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv21 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv23 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * 'tv_expr) * (
# 16 "parser.mly"
       (Ast.binop)
# 3558 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_simple_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state) * _menhir_state * 'tv_binding)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv79 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 3692 "parser.ml"
        )) * _menhir_state * 'tv_pattern_list)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_rec_flag) * _menhir_state * (
# 25 "parser.mly"
       (string)
# 3701 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_rec_flag) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv92)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_defs = 
# 76 "parser.mly"
                    ( [] )
# 3735 "parser.ml"
     in
    _menhir_goto_defs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _ | LPAREN | UNDERSCORE ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

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

and lets : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 68 "parser.mly"
      (Ast.plets)
# 3769 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LET ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 220 "/usr/share/menhir/standard.mly"
  


# 3801 "parser.ml"
