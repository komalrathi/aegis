open Ast_types

type expr =
    | Integer of loc * int
    | BinOp of loc * bin_op * expr * expr
    | CompOp of loc * comp_op * expr * expr
    | Boolean of loc * bool

type program = Prog of expr