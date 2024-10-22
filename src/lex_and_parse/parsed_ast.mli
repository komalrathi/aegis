open Ast_types

type expr =
    | INT of loc * int
    | BINOP of loc * binop * expr * expr

type program = Prog of expr