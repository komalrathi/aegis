open Compiler_types.Language_types

(* performs structural equality on types and type expressions *)
val equal_core_type : core_type -> core_type -> bool

val equal_security_level_type :
  security_level_type -> security_level_type -> bool

val equal_type_expr : type_expr -> type_expr -> bool
