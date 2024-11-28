open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types

type type_binding = identifier * type_expr

type type_environment = type_binding list

let rec lookup_var_type type_environment var =
  match type_environment with
  | [] -> None
  | (list_var, list_var_type) :: t ->
      if phys_equal var list_var then Some list_var_type
      else lookup_var_type t var

let get_function_types type_environment fn_name =
  let ( >>= ) = Result.( >>= ) in
  let rec get_function_types_helper type_environment fn_name =
    match type_environment with
    | [] -> Error (Error.of_string "Function does not exist")
    | (var_name, var_type) :: t ->
        if phys_equal fn_name var_name then 
          get_function_types_helper t fn_name
          >>= fun types -> Ok (var_type :: types)
        else get_function_types_helper t fn_name
  in
  get_function_types_helper type_environment fn_name