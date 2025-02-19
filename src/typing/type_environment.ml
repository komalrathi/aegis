(* open Core *)
open Compiler_types.Language_types
open Compiler_types.Ast_types

type row = (exception_type * security_level_type) list

type type_binding = identifier * type_expr

type type_environment = type_binding list

let rec lookup_var_type type_environment var_name =
  match type_environment with
  | [] -> None
  | (list_var_name, list_var_type) :: t ->
      if var_name = list_var_name then Some list_var_type
      else lookup_var_type t var_name

let get_function_types type_environment fn_name =
  let rec get_function_types_helper type_environment fn_name =
    match type_environment with
    | [] -> Error (Core.Error.of_string "Function does not exist")
    | (var_name, var_type) :: _ when fn_name = var_name -> Ok var_type
    | _ :: t -> get_function_types_helper t fn_name
  in
  get_function_types_helper type_environment fn_name
