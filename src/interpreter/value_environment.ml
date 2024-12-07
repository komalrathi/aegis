open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types

type value_environment = (identifier * interpreter_val) list

(* type function_environment = (identifier * interpreter_val) list *)

let rec lookup_var_value value_environment var =
  match value_environment with
  | [] -> None
  | (list_var, list_var_value) :: t ->
      if phys_equal var list_var then Some list_var_value
      else lookup_var_value t var

(* This checks the function_environment for a specific function, and returns
   the value of the function. Call by Value semantics mean that a function
   has to be evaluated to its value first before the next expression can be
   evaluated *)
let get_function_value function_environment fn_name =
  (* let ( >>= ) = Result.( >>= ) in *)
  let rec get_function_value_helper function_environment fn_name =
    match function_environment with
    (* | [] -> Error.of_string "Function does not exist" *)
    | (var_name, var_value) :: t ->
        if phys_equal fn_name var_name then Ok var_value
          (* >>= fun values -> Ok (var_value :: values) *)
        else get_function_value_helper t fn_name
    | [] -> Error(Error.of_string "Function does not exist")
  in
  get_function_value_helper function_environment fn_name
