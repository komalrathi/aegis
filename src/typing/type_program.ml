open Core
open Parser_frontend
open Type_expr
open Type_function_defn

let rec type_program_function fn_defns expr type_env =
  let ( >>= ) = Result.( >>= ) in
  match fn_defns with
  | fn :: fn_defns ->
      type_function_defn fn type_env
      >>= fun (fn_name, fn_type, typed_f_defn) ->
      type_program_function fn_defns expr ((fn_name, fn_type) :: type_env)
      >>= fun (typed_fn_defns, typed_expr) ->
      Ok (typed_f_defn :: typed_fn_defns, typed_expr)
  | [] ->
      type_expr expr type_env >>= fun (_, typed_expr) -> Ok ([], typed_expr)

let type_program (Parsed_ast.Prog (fn_defns, expr)) =
  type_program_function fn_defns expr []
