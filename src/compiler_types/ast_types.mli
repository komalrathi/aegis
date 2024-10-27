(* Types used by complier when evaluating a program, used e.g. to pattern match cases from parser to AST *)

type loc = Lexing.position

type bin_op = 
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

type comp_op =
  | LT
  | GT
  | LTE
  | GTE

type bool_comp_op = 
  | AND
  | OR

type identifier = string