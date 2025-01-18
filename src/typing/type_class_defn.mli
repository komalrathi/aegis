open Core
open Parser_frontend
open Type_environment

val type_class_defn: Parsed_ast.class_defn -> type_environment -> Typed_ast.class_defn Or_error.t