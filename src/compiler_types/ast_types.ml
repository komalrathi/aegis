open Language_types

type loc = Lexing.position

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMultiply
  | BinOpDivide
  | BinOpExponentiation
  | BinOpModulus

type comp_op =
  | CompOpLessThan
  | CompOpGreaterThan
  | CompOpLessThanEqual
  | CompOpGreaterThanEqual
  | CompOpEqual

type bool_op = BoolOpAnd | BoolOpOr

type unary_op = UnaryOpNot

type identifier = string

type argument = TArg of identifier * type_expr
