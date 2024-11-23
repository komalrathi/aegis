open Language_types

type loc = Lexing.position

type bin_op = BinOpPlus | BinOpMinus | BinOpMultiply | BinOpDivide

type comp_op = CompOpLessThan | CompOpGreaterThan | CompOpLessThanEqual | CompOpGreaterThanEqual

type bool_op = BoolOpAnd | BoolOpOr

type identifier = string

type argument = TArg of identifier * type_expr
