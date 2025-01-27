open Compiler_types.Language_types

val subtyping_check :
  security_level_type -> security_level_type -> security_level_type -> bool

val join : security_level_type -> security_level_type -> security_level_type

val less_than_or_equal : security_level_type -> security_level_type -> bool
