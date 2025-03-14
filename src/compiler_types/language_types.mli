(* Types defined in the Aegis language specification *)

(* There are 2 supported exceptions in Aegis: division by zero, and integer
   overflow *)
type exception_type = DivisionByZero | IntegerOverflow

(* Aegis has 3 types of expressions in the type system: int, bool, and
   function *)
type core_type =
  | TEInt
  | TEBool
  (* function arguments, return type *)
  | TFunction of core_type list * core_type
  (* class_name *)
  | TEObject of string
  | TEUnit
  | TException of exception_type

(* For now, Aegis only supports 2 security levels *)
type security_level_type = TSLow | TSHigh

(* The type is now a tuple, following the language specification *)
type type_expr = core_type * security_level_type

type interpret_expr_result =
  | IValue of interpreter_val * value_environment
    (* bool is_resumable indicates whether the exception is resumable - True
       if resumable, False if not *)
  | IException of exception_type * interpreter_val * bool

(* Aegis has 5 kind of values that the program is evaluated to: int, bool,
   unit, object, and continuation. Continuation is used to implement
   resumable exception handling *)
and interpreter_val =
  | VInt of int
  | VBool of bool
  | VObject of string * interpreter_val list
  | VUnit of unit
  | VContinuation of
      (interpret_expr_result, interpret_expr_result) Effect.Deep.continuation

and value_environment = (string * interpreter_val) list
