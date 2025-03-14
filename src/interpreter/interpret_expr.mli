open Core
open Compiler_types.Language_types
open Value_environment
open Typing
open Interpret_fn_defn

val interpret_expr :
     Typed_ast.expr
  -> value_environment
  -> function_environment
  -> Typed_ast.class_defn list
  -> interpret_expr_result Or_error.t
