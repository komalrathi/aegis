open Core
open Parser_frontend
open Typed_ast
open Compiler_types.Language_types
open Type_program

(* Converts a Parsed_ast.expr to a Typed_ast.expr *)
let typed_function_defn (parsed_function_defn : Parsed_ast.function_defn) :
    Typed_ast.typed_function_defn =
  match parsed_function_defn with
  | Parsed_ast.TFunction (name, args, return_type, expr_body) ->
      let typed_args = args in
      let typed_return_type = return_type in
      let typed_body = Type_program.type_expr expr_body [] in
      Typed_ast.TFunction (name, typed_args, typed_return_type, typed_body)
