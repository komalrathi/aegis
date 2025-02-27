open Compiler_types.Language_types
open Compiler_types.Ast_types

val type_expr_to_string : type_expr -> string

val sec_level_to_string : security_level_type -> string

val bin_op_to_string : bin_op -> string

val comp_op_to_string : comp_op -> string

val bool_op_to_string : bool_op -> string

val unary_op_to_string : unary_op -> string

val arg_to_string : argument -> string

val value_to_string : interpreter_val -> string

val exception_type_to_string : exception_type -> string
