open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int
  | BinOp of loc * bin_op * expr * expr
  | CompOp of loc * comp_op * expr * expr
  | Boolean of loc * bool
  | BoolCompOp of loc * bool_comp_op * expr * expr
  | Identifier of loc * identifier
  | Assign of loc * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr

type program = Prog of expr
