open Core
open Parser_frontend
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Type_expr

(* Converts a Parsed_ast.function_defn to a Typed_ast.function_defn *)
let type_function_defn
    (Parsed_ast.FunctionDefn
      (fn_name, args, (fn_core_type, fn_security_level_type), expr_body) )
    type_env =
  let ( >>= ) = Result.( >>= ) in
  let fn_return_type = (fn_core_type, fn_security_level_type) in
  let arg_types_env =
    List.map
      ~f:(fun (TArg (arg_name, arg_type)) -> (arg_name, arg_type))
      args
  in
  let arg_core_types =
    List.map ~f:(fun (TArg (_, (arg_core_type, _))) -> arg_core_type) args
  in
  let fn_type = TFunction (arg_core_types, fn_core_type) in
  type_expr expr_body
    ( (fn_name, (fn_type, fn_security_level_type))
    :: (arg_types_env @ type_env) )
  >>= fun (expr_body_type, typed_expr_body) ->
  if phys_equal fn_return_type expr_body_type then
    Ok
      ( fn_name
      , fn_return_type
      , Typed_ast.FunctionDefn
          (fn_name, args, fn_return_type, typed_expr_body) )
  else
    Error
      (Error.of_string
         "The function return type does not match the function body type" )
