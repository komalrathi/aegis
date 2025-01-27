(* Types defined in the Aegis language specification *)

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

(* For now, Aegis only supports 2 security levels *)
type security_level_type = TSLow | TSHigh

(* The type is now a tuple, following the language specification *)
type type_expr = core_type * security_level_type

(* Aegis has 4 kind of values that the program is evaluated to: int, bool,
   unit, and object *)
type interpreter_val =
  | VInt of int
  | VBool of bool
  (* class_name, field_value) list *)
  | VObject of string * interpreter_val list
  | VUnit of unit
