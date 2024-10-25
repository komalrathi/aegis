
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TRUE
    | PLUS
    | MULTIPLY
    | MINUS
    | INT of (
# 6 "src/parser_frontend/parser.mly"
       (int)
# 19 "src/parser_frontend/parser.ml"
  )
    | FALSE
    | EOF
    | DIVIDE
  
end

include MenhirBasics

# 1 "src/parser_frontend/parser.mly"
  
    open Ast_types 
    open Parsed_ast

# 34 "src/parser_frontend/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_program) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState06 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 06.
        Stack shape : expr.
        Start symbol: program. *)

  | MenhirState08 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 08.
        Stack shape : expr.
        Start symbol: program. *)

  | MenhirState10 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 10.
        Stack shape : expr.
        Start symbol: program. *)

  | MenhirState12 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 12.
        Stack shape : expr.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Parsed_ast.expr) * Lexing.position

and _menhir_box_program = 
  | MenhirBox_program of (Parsed_ast.expr) [@@unboxed]

let _menhir_action_1 =
  fun _startpos_i_ i ->
    let _startpos = _startpos_i_ in
    (
# 42 "src/parser_frontend/parser.mly"
        (Integer(_startpos, i))
# 75 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_2 =
  fun _startpos_e1_ e1 e2 ->
    let op = 
# 33 "src/parser_frontend/parser.mly"
       ( PLUS )
# 83 "src/parser_frontend/parser.ml"
     in
    let _startpos = _startpos_e1_ in
    (
# 43 "src/parser_frontend/parser.mly"
                            (BinOp(_startpos, op, e1, e2))
# 89 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_3 =
  fun _startpos_e1_ e1 e2 ->
    let op = 
# 34 "src/parser_frontend/parser.mly"
        ( MINUS )
# 97 "src/parser_frontend/parser.ml"
     in
    let _startpos = _startpos_e1_ in
    (
# 43 "src/parser_frontend/parser.mly"
                            (BinOp(_startpos, op, e1, e2))
# 103 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_4 =
  fun _startpos_e1_ e1 e2 ->
    let op = 
# 35 "src/parser_frontend/parser.mly"
           ( MULTIPLY )
# 111 "src/parser_frontend/parser.ml"
     in
    let _startpos = _startpos_e1_ in
    (
# 43 "src/parser_frontend/parser.mly"
                            (BinOp(_startpos, op, e1, e2))
# 117 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_5 =
  fun _startpos_e1_ e1 e2 ->
    let op = 
# 36 "src/parser_frontend/parser.mly"
         ( DIVIDE )
# 125 "src/parser_frontend/parser.ml"
     in
    let _startpos = _startpos_e1_ in
    (
# 43 "src/parser_frontend/parser.mly"
                            (BinOp(_startpos, op, e1, e2))
# 131 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_6 =
  fun _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 44 "src/parser_frontend/parser.mly"
       (Boolean(_startpos, true))
# 140 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_7 =
  fun _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 45 "src/parser_frontend/parser.mly"
        (Boolean(_startpos, false))
# 149 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_action_8 =
  fun e ->
    (
# 39 "src/parser_frontend/parser.mly"
              (e)
# 157 "src/parser_frontend/parser.ml"
     : (Parsed_ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | DIVIDE ->
        "DIVIDE"
    | EOF ->
        "EOF"
    | FALSE ->
        "FALSE"
    | INT _ ->
        "INT"
    | MINUS ->
        "MINUS"
    | MULTIPLY ->
        "MULTIPLY"
    | PLUS ->
        "PLUS"
    | TRUE ->
        "TRUE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _startpos__1_ = _startpos in
      let _v = _menhir_action_6 _startpos__1_ in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
  
  and _menhir_run_13 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULTIPLY ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVIDE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | MINUS | PLUS ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_3 _startpos_e1_ e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState08 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_startpos_i_, i) = (_startpos, _v) in
      let _v = _menhir_action_1 _startpos_i_ i in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_i_ _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _startpos__1_ = _startpos in
      let _v = _menhir_action_7 _startpos__1_ in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_5 _startpos_e1_ e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_4 _startpos_e1_ e1 e2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MULTIPLY ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIVIDE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | MINUS | PLUS ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_2 _startpos_e1_ e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState06 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | MULTIPLY ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          let _menhir_s = MenhirState12 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EOF ->
          let e = _v in
          let _v = _menhir_action_8 e in
          MenhirBox_program _v
      | DIVIDE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
