open Core
open Parser_frontend

val type_program :
     Parsed_ast.program
  -> (Typed_ast.function_defn list * Typed_ast.expr, Core.Error.t) result
