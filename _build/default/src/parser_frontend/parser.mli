
(* The type of tokens. *)

type token = 
  | TRUE
  | PLUS
  | MULTIPLY
  | MINUS
  | LTE
  | LT
  | INT of (int)
  | GTE
  | GT
  | FALSE
  | EOF
  | DIVIDE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsed_ast.expr)
