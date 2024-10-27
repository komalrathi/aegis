(* Types used in compiler definition when evaluating a program, used e.g. to pattern match cases from parser to AST *)
(* Using OCaml type system to e.g. make sure we have a fixed set of binary operations, and use pattern matching to guaranteee we handle all cases *)


type loc = Lexing.position

(* We support 4 types of binary operations: +, -, *, /. We use type bin_op to enforce that, *)
type bin_op = 
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

(* We support 4 types of comparison operations: <, >, <=, >=. We use type comp_op to enforce that, *)
type comp_op =
  | LT
  | GT
  | LTE
  | GTE

(* We support 2 types of boolean comparison operations: &&, || . We use type bool_comp_op to enforce that, *)
type bool_comp_op = 
  | AND
  | OR

(* We use type identifier to represent a variable name, and differentiate it from normal strings. *)
type identifier = string