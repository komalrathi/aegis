open Core
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Parser_frontend
open Type_environment

val type_expr :
  Parsed_ast.expr -> type_environment -> (type_expr * Typed_ast.expr) Or_error.t

val type_program : Parsed_ast.program -> Typed_ast.program Or_error.t
