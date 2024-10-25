type loc = Lexing.position

type bin_op = 
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

type comp_op = 
  | LT
  | GT
  | LTE
  | GTE

type bool_comp_op = 
  | AND
  | OR

type type_expr = 
  | TEInt
  | TEBool

type interpreter_val = 
  | VInt of int
  | VBool of bool