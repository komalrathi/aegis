type exception_type = DivisionByZero | IntegerOverflow

type core_type =
  | TEInt
  | TEBool
  | TFunction of core_type list * core_type
  | TEObject of string
  | TEUnit
  | TException of exception_type

type security_level_type = TSLow | TSHigh

type type_expr = core_type * security_level_type

type interpret_expr_result =
  | IValue of interpreter_val * value_environment
  | IException of exception_type * interpreter_val * bool

and interpreter_val =
  | VInt of int
  | VBool of bool
  | VObject of string * interpreter_val list
  | VUnit of unit
  | VContinuation of
      (interpret_expr_result, interpret_expr_result) Effect.Deep.continuation

and value_environment = (string * interpreter_val) list
