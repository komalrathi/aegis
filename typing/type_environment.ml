open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types

type type_binding = identifier * type_expr
type type_environment = type_binding list

let rec lookup_var_type type_environment var = 
  match type_environment with
  | [] -> None 
  | (list_var, list_var_type) :: t -> if phys_equal var list_var then Some list_var_type 
                                      else lookup_var_type t var

