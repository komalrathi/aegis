open Core
open Compiler_types.Ast_types
open Typing

type function_environment =
  (identifier * (identifier list * Typed_ast.expr)) list

val interpret_fn_defns :
     Typed_ast.function_defn list
  -> function_environment
  -> (function_environment, Error.t) result
