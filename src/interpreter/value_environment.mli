open Compiler_types.Language_types
open Compiler_types.Ast_types

(* value_environment (tuple of identifier and value) *)
type value_environment = (identifier * interpreter_val) list

val lookup_var_value: value_environment -> identifier -> interpreter_val option