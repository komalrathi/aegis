
(* The type of tokens. *)

type token = 
  | PLUS
  | MULTIPLY
  | MINUS
  | INT of (int)
  | EOF
  | DIVIDE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsed_ast.expr)
