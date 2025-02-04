open Core
open Compiler_types.Language_types
open Parser_frontend
open Type_expr
open Type_class_defn
open Type_function_defn
open Typed_ast

let rec type_program_function fn_defns expr type_env typed_class_defns pc =
  let ( >>= ) = Result.( >>= ) in
  match fn_defns with
  | fn :: fn_defns ->
      type_function_defn fn type_env typed_class_defns
      >>= fun (fn_name, fn_type, typed_f_defn) ->
      type_program_function fn_defns expr
        ((fn_name, fn_type) :: type_env)
        typed_class_defns pc
      >>= fun (typed_fn_defns, typed_expr) ->
      Ok (typed_f_defn :: typed_fn_defns, typed_expr)
  | [] ->
      type_expr expr type_env typed_class_defns pc
      >>= fun (_, typed_expr, _) -> Ok ([], typed_expr)

(* initialise pc as low, and pass it through as an argument, keep updating it
   in type_expr *)
let type_program (Parsed_ast.Prog (class_defns, fn_defns, expr)) =
  let ( >>= ) = Result.( >>= ) in
  let pc = TSLow in
  type_class_defns class_defns []
  >>= fun typed_class_defns ->
  type_program_function fn_defns expr [] class_defns pc
  >>= fun (typed_fn_defns, typed_expr) ->
  Ok (Prog (typed_class_defns, typed_fn_defns, typed_expr))
