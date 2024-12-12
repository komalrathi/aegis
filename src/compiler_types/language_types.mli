(* Types defined in the Aegis language specification *)

(* Aegis has 3 types of expressions in the type system: int, bool, and
   function *)
type core_type = TEInt | TEBool | TFunction of core_type list * core_type

(* For now, Aegis only supports 2 security levels *)
type security_level_type = TSLow | TSHigh

(* The type is now a tuple, following the language specification *)
type type_expr = core_type * security_level_type

(* Aegis only has 2 kinds of values that the program is evaluated to, int and
   bool *)
type interpreter_val = VInt of int | VBool of bool
