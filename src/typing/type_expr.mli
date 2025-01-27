open Core
open Compiler_types.Language_types
open Parser_frontend
open Type_environment
open Class_environment

val type_expr :
     Parsed_ast.expr
  -> type_environment
  -> class_environment
  -> security_level_type
  -> (type_expr * Typed_ast.expr * security_level_type) Or_error.t
