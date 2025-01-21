open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types

type class_info =
  { fields: identifier list
  ; constructor: identifier list * expr
  ; methods:
      ( security_level_type
      * identifier (* function_name *)
      * identifier list (* arguments *)
      * expr (* body *) )
      list }

type class_environment = (identifier * class_info) list

let interpret_class_defns (class_defns : class_defn list) :
    class_environment Or_error.t =
  let ( >>= ) = Result.( >>= ) in
  let interpret_class_defn
      (ClassDefn (class_name, fields, constructor, methods)) =
    let interpret_fields fields =
      (* can drop the type information from the field definition because we
         have type checked it already *)
      List.map ~f:(fun (FieldDefn (field_name, _)) -> Ok field_name) fields
    in
    let interpret_constructor (Constructor (args, constructor_expr)) =
      let arg_names = List.map ~f:(fun (TArg (name, _)) -> name) args in
      Ok (arg_names, constructor_expr)
    in
    let interpret_methods methods =
      List.map
        ~f:(fun
            (MethodDefn
               ( security_level_type
               , FunctionDefn (method_name, args, _, body) ) )
          ->
          let arg_names = List.map ~f:(fun (TArg (name, _)) -> name) args in
          Ok (security_level_type, method_name, arg_names, body) )
        methods
    in
    Result.all (interpret_fields fields)
    >>= fun interpreted_fields ->
    interpret_constructor constructor
    >>= fun (constructor_arg_names, constructor_body) ->
    Result.all (interpret_methods methods)
    >>= fun interpreted_methods ->
    Ok
      ( class_name
      , { fields= interpreted_fields
        ; constructor= (constructor_arg_names, constructor_body)
        ; methods= interpreted_methods } )
  in
  Result.all (List.map ~f:interpret_class_defn class_defns)
