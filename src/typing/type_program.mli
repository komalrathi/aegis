open Core
open Parser_frontend

val type_expr :
  Parsed_ast.expr -> ('a * 'b) list -> (Typed_ast.expr * 'c, Error.t) result

val type_program : Parsed_ast.program -> Typed_ast.program Or_error.t
