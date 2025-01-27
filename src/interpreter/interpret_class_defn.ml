(* open Core *)
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

let interpret_class_defns (class_defns : class_defn list) :
    class_environment Core.Or_error.t =
  let ( >>= ) = Core.Result.( >>= ) in
  let interpret_class_defn
      (ClassDefn (class_name, fields, constructor, methods)) =
    let interpret_fields fields =
      (* can drop the type information from the field definition because we
         have type checked it already *)
      Core.List.map
        ~f:(fun (FieldDefn (field_name, _)) -> Ok field_name)
        fields
    in
    let interpret_constructor (Constructor (args, constructor_expr)) =
      let arg_names = Core.List.map ~f:(fun (TArg (name, _)) -> name) args in
      Ok (arg_names, constructor_expr)
    in
    let interpret_methods methods =
      Core.List.map
        ~f:(fun
            (MethodDefn
               ( security_level_type
               , FunctionDefn (method_name, args, _, body) ) )
          ->
          let arg_names =
            Core.List.map ~f:(fun (TArg (name, _)) -> name) args
          in
          Ok (security_level_type, method_name, arg_names, body) )
        methods
    in
    Core.Result.all (interpret_fields fields)
    >>= fun interpreted_fields ->
    interpret_constructor constructor
    >>= fun (constructor_arg_names, constructor_body) ->
    Core.Result.all (interpret_methods methods)
    >>= fun interpreted_methods ->
    Ok
      ( class_name
      , { fields= interpreted_fields
        ; constructor= (constructor_arg_names, constructor_body)
        ; methods= interpreted_methods } )
  in
  Core.Result.all (Core.List.map ~f:interpret_class_defn class_defns)
