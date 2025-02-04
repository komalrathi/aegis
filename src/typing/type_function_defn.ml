open Core
open Parser_frontend
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Equal_type_expr
open Type_expr

(* Converts a Parsed_ast.function_defn to a Typed_ast.function_defn *)
let type_function_defn
    (Parsed_ast.FunctionDefn
       ( fn_name
       , args
       , (fn_return_core_type, fn_return_security_level_type)
       , expr_body ) ) type_env class_defns =
  let ( >>= ) = Result.( >>= ) in
  let fn_return_type =
    (fn_return_core_type, fn_return_security_level_type)
  in
  let arg_types_env =
    List.map
      ~f:(fun (TArg (arg_name, (arg_core_type, arg_security_level))) ->
        (arg_name, (arg_core_type, arg_security_level)) )
      args
  in
  let arg_core_types =
    List.map ~f:(fun (TArg (_, (arg_core_type, _))) -> arg_core_type) args
  in
  let fn_type = TFunction (arg_core_types, fn_return_core_type) in
  type_expr expr_body
    ( (fn_name, (fn_type, fn_return_security_level_type))
    :: (arg_types_env @ type_env) )
    class_defns TSLow
  >>= fun ((expr_body_core_type, expr_body_sec_level), typed_expr_body, _) ->
  if equal_type_expr fn_return_type (expr_body_core_type, expr_body_sec_level)
  then
    Ok
      ( fn_name
      , (fn_type, fn_return_security_level_type)
      , Typed_ast.FunctionDefn
          (fn_name, args, fn_return_type, typed_expr_body) )
  else if equal_core_type fn_return_core_type expr_body_core_type then
    Error
      (Error.of_string
         "The function security level return type does not match the \
          function body security level type" )
  else
    Error
      (Error.of_string
         "The function return type does not match the function body type" )
