open Core
open Parser_frontend
open Type_environment
open Compiler_types.Language_types
open Compiler_types.Ast_types

val type_function_defn :
     Parsed_ast.function_defn
  -> type_environment
  -> Parsed_ast.class_defn list
  -> row
  -> (identifier * type_expr * Typed_ast.function_defn * row) Or_error.t
