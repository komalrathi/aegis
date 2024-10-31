type type_expr = 
  | TEInt
  | TEBool

type interpreter_val = 
  | VInt of int
  | VBool of bool