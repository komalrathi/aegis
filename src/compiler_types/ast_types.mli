(* Types used in compiler definition when evaluating a program, used e.g. to
   pattern match cases from parser to AST *)
(* Using OCaml type system to e.g. make sure we have a fixed set of binary
   operations, and use pattern matching to guaranteee we handle all cases *)
open Language_types

type loc = Lexing.position

(* We support 6 types of binary operations: +, -, *, /, ^, %. We use type
   bin_op to enforce that. *)
type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMultiply
  | BinOpDivide
  | BinOpExponentiation
  | BinOpModulus

(* We support 4 types of comparison operations: <, >, <=, >=. We use type
   comp_op to enforce that. *)
type comp_op =
  | CompOpLessThan
  | CompOpGreaterThan
  | CompOpLessThanEqual
  | CompOpGreaterThanEqual
  | CompOpEqual

(* We support 2 types of boolean comparison operations: && and || . We use
   type bool_comp_op to enforce that. *)
type bool_op = BoolOpAnd | BoolOpOr

(* We support 1 type of unary operation: ! . We use type unary_op to enforce
   that. *)
type unary_op = UnaryOpNot

type identifier = string

(* This allows us to have arguments paired with their type expressions (e.g.
   for functions) *)
type argument = TArg of identifier * type_expr
