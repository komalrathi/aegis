open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types
open Value_environment
open Typing
open Interpret_class_defn

val apply_int_bin_op :
  bin_op -> int -> int -> (interpreter_val, Error.t) result

val apply_comp_op :
  comp_op -> int -> int -> (interpreter_val, Error.t) result

val interpret_expr :
     Typed_ast.expr
  -> value_environment
  -> function_environment
  -> class_environment
  -> (interpreter_val * value_environment) Or_error.t
