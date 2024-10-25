open Ast_types

type expr =
  | Integer     of loc * int
  | BinOp       of loc * type_expr * bin_op * expr * expr
  | CompOp      of loc * type_expr * comp_op * expr * expr
  | Boolean     of loc * bool
  | BoolCompOp  of loc * type_expr * bool_comp_op * expr * expr


type program = Prog of expr