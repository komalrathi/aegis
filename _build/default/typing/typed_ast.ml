open Ast_types

type expr =
  | Integer     of loc * int
  | BinOp       of loc * type_expr * bin_op * expr * expr
  | Boolean     of loc * bool

type program = Prog of expr