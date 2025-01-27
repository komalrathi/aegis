(* open Core *)
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Typed_ast

type class_info =
  { fields: (identifier * type_expr) list
  ; constructor: (identifier * type_expr) list * expr
  ; methods:
      ( security_level_type
      * identifier (* function_name *)
      * (identifier * type_expr) list (* arguments *)
      * expr (* body *) )
      list }

type class_environment = (identifier * class_info) list

let get_class_info (class_name : identifier)
    (class_environment : class_environment) : class_info option =
  Core.List.Assoc.find class_environment ~equal:( = ) class_name

let rec get_method_info method_name methods =
  match methods with
  | [] -> Error (Core.Error.of_string "Method does not exist in the class")
  | (method_sec_level, method_name', arg_types, typed_expr) :: rest ->
      if String.equal method_name method_name' then
        Ok (method_sec_level, arg_types, typed_expr)
      else get_method_info method_name rest
