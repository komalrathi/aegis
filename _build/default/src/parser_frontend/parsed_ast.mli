open Ast_types

type expr =
    | Integer of loc * int
    | BinOp of loc * bin_op * expr * expr

type program = Prog of expr