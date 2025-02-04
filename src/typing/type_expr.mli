open Core
open Compiler_types.Language_types
open Parser_frontend
open Type_environment

val type_expr :
     Parsed_ast.expr
  -> type_environment
  -> Parsed_ast.class_defn list
  -> security_level_type
  -> (type_expr * Typed_ast.expr * security_level_type) Or_error.t
