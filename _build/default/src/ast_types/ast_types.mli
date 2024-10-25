type loc = Lexing.position

type bin_op = 
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

type type_expr = 
  | TEInt
  | TEBool

type interpreter_val = 
  | VInt of int
  | VBool of bool