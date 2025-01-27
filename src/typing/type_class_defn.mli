open Core
open Parser_frontend
open Type_environment
open Class_environment

val type_class_defns :
     Parsed_ast.class_defn list
  -> type_environment
  -> class_environment
  -> (Typed_ast.class_defn list * class_environment) Or_error.t
