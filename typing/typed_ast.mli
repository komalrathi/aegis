open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int * security_level_type
  | BinOp of loc * type_expr * bin_op * expr * expr
  | CompOp of loc * type_expr * comp_op * expr * expr
  | Boolean of loc * bool * security_level_type
  | BoolCompOp of loc * type_expr * bool_comp_op * expr * expr
  | Identifier of loc * type_expr * identifier
  | Assign of loc * type_expr * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr * type_expr
  | If of loc * expr * expr * expr * type_expr

type program = Prog of expr
