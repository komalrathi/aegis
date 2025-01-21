open Core
open Parser_frontend
open Type_environment

val type_class_defns :
     Parsed_ast.class_defn list
  -> type_environment
  -> Typed_ast.class_defn list Or_error.t
