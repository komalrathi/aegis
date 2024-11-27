open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int * security_level_type
  | BinOp of loc * bin_op * expr * expr
  | CompOp of loc * comp_op * expr * expr
  | Boolean of loc * bool * security_level_type
  | BoolOp of loc * bool_op * expr * expr
  | Identifier of loc * identifier
  | Assign of loc * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr
  | If of loc * expr * expr * expr
  | Classify of loc * expr
  | Declassify of loc * expr

type function_defn =
  | FunctionDefn of identifier * argument list * type_expr * expr

type program = Prog of function_defn list * expr
