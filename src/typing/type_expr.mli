open Core
open Compiler_types.Language_types
open Parser_frontend
open Type_environment

type row = (exception_type * security_level_type) list

val type_expr :
     Parsed_ast.expr
  -> type_environment
  -> Parsed_ast.class_defn list
  -> security_level_type
  -> row
  -> (type_expr * Typed_ast.expr * security_level_type * row) Or_error.t
