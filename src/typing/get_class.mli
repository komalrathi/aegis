open Compiler_types.Ast_types
open Parser_frontend

val get_class :
  identifier -> Parsed_ast.class_defn list -> Parsed_ast.class_defn option

val get_method :
  identifier -> Parsed_ast.method_defn list -> Parsed_ast.method_defn option
