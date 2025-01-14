open Compiler_types.Ast_types
open Compiler_types.Language_types

type expr =
  | Integer of loc * int * security_level_type
  | BinOp of loc * type_expr * bin_op * expr * expr
  | CompOp of loc * type_expr * comp_op * expr * expr
  | Boolean of loc * bool * security_level_type
  | BoolOp of loc * type_expr * bool_op * expr * expr
  | UnaryOp of loc * type_expr * unary_op * expr
  | Identifier of loc * type_expr * identifier
  | Assign of loc * type_expr * identifier * expr
  | Let of loc * identifier * type_expr * expr * expr * type_expr
  | If of loc * expr * expr * expr * type_expr
  | Classify of loc * expr * type_expr
  | Declassify of loc * expr * type_expr
  | FunctionApp of loc * type_expr * identifier * expr list
  | While of loc * expr * expr * type_expr
  | Seq of loc * expr * expr * type_expr
  (* do not need type_expr for print as it is always of type unit *)
  | Print of loc * expr list
  | SecurePrint of loc * expr list

type function_defn =
  | FunctionDefn of identifier * argument list * type_expr * expr

type program = Prog of function_defn list * expr
