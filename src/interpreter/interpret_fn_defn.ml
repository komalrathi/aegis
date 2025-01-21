open Core
open Typing.Typed_ast
open Compiler_types.Ast_types

type function_environment = (identifier * (identifier list * expr)) list

let rec interpret_fn_defns fn_defns function_environment =
  match fn_defns with
  | [] -> Ok function_environment
  | FunctionDefn (fn_name, param_list, (_, _), fn_body) :: fn_defns ->
      (* get the list of parameter names from the argument list *)
      let param_names =
        Stdlib.List.map (fun (TArg (id, _)) -> id) param_list
      in
      (* add function to the function_environment *)
      let new_env =
        (fn_name, (param_names, fn_body)) :: function_environment
      in
      interpret_fn_defns fn_defns new_env
