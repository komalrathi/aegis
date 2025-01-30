open Core
open Parser_frontend
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Equal_type_expr
open Type_expr
open Type_function_defn

(* Convert a Parsed_ast.class_defn to a Typed_ast.class_defn *)
(* Need to typecheck the constructor, methods and fields *)

(* Typecheck the method definition - similar to type_function_defn *)
let type_method_defn method_defn type_env class_environment =
  let ( >>= ) = Result.( >>= ) in
  let (Parsed_ast.MethodDefn (sec_level, fn_defn)) = method_defn in
  type_function_defn fn_defn type_env class_environment
  >>= fun (_, _, typed_f_defn) ->
  Ok (Typed_ast.MethodDefn (sec_level, typed_f_defn))

(* Typecheck the class definitions *)
let type_class_defns class_defns type_env class_environment =
  let ( >>= ) = Result.( >>= ) in
  let type_class_defn
      (Parsed_ast.ClassDefn
         (class_name, field_defns, constructor, method_defns) ) =
    let field_types_env =
      List.map
        ~f:(fun
            (Parsed_ast.FieldDefn
               (field_name, (field_core_type, field_sec_level)) )
          -> (field_name, (field_core_type, field_sec_level)) )
        field_defns
    in
    let constructor_expr =
      match constructor with Parsed_ast.Constructor (_, expr) -> expr
    in
    let constructor_args =
      match constructor with
      | Parsed_ast.Constructor (args, _) ->
          List.map
            ~f:(fun (TArg (arg_name, (arg_core_type, arg_security_level))) ->
              (arg_name, (arg_core_type, arg_security_level)) )
            args
    in
    let constructor_arg_core_types =
      match constructor with
      | Parsed_ast.Constructor (args, _) ->
          List.map
            ~f:(fun (TArg (_, (arg_core_type, _))) -> arg_core_type)
            args
    in
    let constructor_type = TFunction (constructor_arg_core_types, TEUnit) in
    let constructor_type_env = field_types_env @ type_env in
    type_expr constructor_expr
      (constructor_args @ constructor_type_env)
      class_environment TSLow
    >>= fun ((constructor_core_type, _), typed_constructor, _) ->
    if equal_core_type constructor_type constructor_core_type then
      let type_field_expr (Parsed_ast.FieldDefn (field_name, field_type)) =
        Ok (Typed_ast.FieldDefn (field_name, field_type))
      in
      Result.all (List.map ~f:type_field_expr field_defns)
      >>= fun typed_field_defns ->
      let typed_method_defns =
        List.map
          ~f:(fun method_defn ->
            type_method_defn method_defn constructor_type_env
              class_environment )
          method_defns
      in
      Result.all typed_method_defns
      >>= fun typed_method_defns ->
      Ok
        (Typed_ast.ClassDefn
           ( class_name
           , typed_field_defns
           , Typed_ast.Constructor
               ( ( match constructor with
                 | Parsed_ast.Constructor (args, _) -> args )
               , typed_constructor )
           , typed_method_defns ) )
    else
      Error
        (Error.of_string
           "The constructor type does not match the constructor body type" )
  in
  Result.all (List.map ~f:type_class_defn class_defns)
  >>= fun typed_class_defns -> Ok (typed_class_defns, class_environment)
