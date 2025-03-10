open Core
open Compiler_types.Language_types
open Value_environment
open Typing
open Interpret_fn_defn

type interpret_expr_result =
  | IValue of interpreter_val * value_environment
  (* is_resumable is a bool, and is true if the exception is resumable and
     false otherwise *)
  | IException of exception_type * interpreter_val * bool

val interpret_expr :
     Typed_ast.expr
  -> value_environment
  -> function_environment
  -> Typed_ast.class_defn list
  -> interpret_expr_result Or_error.t
