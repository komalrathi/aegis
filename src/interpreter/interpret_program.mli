open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types
open Value_environment
open Typing

val interpret_program : Typed_ast.program -> interpreter_val Or_error.t

val apply_int_bin_op :
  bin_op -> int -> int -> (interpreter_val, Error.t) result

val apply_comp_op :
  comp_op -> int -> int -> (interpreter_val, Error.t) result

val interpret_expr :
  Typed_ast.expr -> value_environment -> interpreter_val Or_error.t
