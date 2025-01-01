open Core
open Parser_frontend

val type_program :
  Parsed_ast.program -> (Typed_ast.program, Error.t) Result.t
