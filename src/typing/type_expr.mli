open Core
open Compiler_types.Language_types
open Parser_frontend
open Type_environment

(* The bool is used to indicate if the exception is resumable or not. True if
   resumable, false otherwise *)
type row = (exception_type * security_level_type * bool) list

val type_expr :
     Parsed_ast.expr
  -> type_environment
  -> Parsed_ast.class_defn list
  -> security_level_type
  -> row
  -> (type_expr * Typed_ast.expr * security_level_type * row) Or_error.t
