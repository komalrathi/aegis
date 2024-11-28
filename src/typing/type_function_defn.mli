open Core
open Parser_frontend
open Type_environment
open Compiler_types.Language_types

val type_function_defn :
     Parsed_ast.function_defn
  -> type_environment
  -> (type_expr * Typed_ast.function_defn) Or_error.t
