open Language_types

type loc = Lexing.position

type bin_op = PLUS | MINUS | MULTIPLY | DIVIDE

type comp_op = LT | GT | LTE | GTE

type bool_comp_op = AND | OR

type identifier = string

type argument = TArg of identifier * type_expr
