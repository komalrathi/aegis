open Core
open Parser_frontend
open Type_environment
open Compiler_types.Language_types
open Compiler_types.Ast_types
open Class_environment

val type_function_defn :
     Parsed_ast.function_defn
  -> type_environment
  -> class_environment
  -> (identifier * type_expr * Typed_ast.function_defn) Or_error.t
