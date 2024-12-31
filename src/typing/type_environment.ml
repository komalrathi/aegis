(* open Core *)
open Compiler_types.Language_types
open Compiler_types.Ast_types

type type_binding = identifier * type_expr

type type_environment = type_binding list

let rec lookup_var_type type_environment var_name =
  match type_environment with
  | [] -> None
  | (list_var_name, list_var_type) :: t ->
      if var_name = list_var_name then Some list_var_type
      else lookup_var_type t var_name

let get_function_types type_environment fn_name =
  let ( >>= ) = Core.Result.( >>= ) in
  let rec get_function_types_helper type_environment fn_name =
    match type_environment with
    | [] -> Error (Core.Error.of_string "Function does not exist")
    | (var_name, var_type) :: t ->
        if fn_name = var_name then
          get_function_types_helper t fn_name
          >>= fun types -> Ok (var_type :: types)
        else get_function_types_helper t fn_name
  in
  get_function_types_helper type_environment fn_name
