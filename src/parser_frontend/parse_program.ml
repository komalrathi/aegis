open Lexer
open Lexing
open Core

let print_parser_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d, position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try
    let program = Parser.program Lexer.read_token lexbuf in
    Ok program
  with
  | SyntaxError syntax_error_msg ->
      let error_msg =
        Fmt.str "%s: Syntax error - %s@."
          (print_parser_error_position lexbuf)
          syntax_error_msg
      in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg =
        Fmt.str "%s: Parser error @." (print_parser_error_position lexbuf)
      in
      Error (Error.of_string error_msg)
  | _ ->
      let error_msg =
        Fmt.str "%s: Unexpected error @."
          (print_parser_error_position lexbuf)
      in
      Error (Error.of_string error_msg)
