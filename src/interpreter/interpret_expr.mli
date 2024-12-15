open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types
open Value_environment
open Typing

type function_environment =
  (identifier * (identifier list * Typed_ast.expr)) list

val apply_int_bin_op :
  bin_op -> int -> int -> (interpreter_val, Error.t) result

val apply_comp_op :
  comp_op -> int -> int -> (interpreter_val, Error.t) result

val interpret_expr :
     Typed_ast.expr
  -> value_environment
  -> function_environment
  -> interpreter_val Or_error.t

val interpret_fn_defns :
     Typed_ast.function_defn list
  -> function_environment
  -> (function_environment, Error.t) result
