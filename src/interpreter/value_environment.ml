open Compiler_types.Language_types
open Compiler_types.Ast_types

type value_environment = (identifier * interpreter_val) list

let rec lookup_var_value value_environment var =
  match value_environment with
  | [] -> None
  | (list_var, list_var_value) :: t ->
      if var = list_var then Some list_var_value else lookup_var_value t var
