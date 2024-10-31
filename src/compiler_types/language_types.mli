(* Types defined in the Aegis language specification *)

(* Aegis only has 2 types of expressions in the type system, int and bool *)
type type_expr = TEInt | TEBool

(* Aegis only has 2 kinds of values that the program is evaluated to, int and
   bool *)
type interpreter_val = VInt of int | VBool of bool
