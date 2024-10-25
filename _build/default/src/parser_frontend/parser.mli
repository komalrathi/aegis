
(* The type of tokens. *)

type token = 
  | TRUE
  | PLUS
  | OR
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
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsed_ast.expr)
