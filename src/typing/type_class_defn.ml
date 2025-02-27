open Core
open Parser_frontend
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Type_expr
open Type_function_defn

(* Convert a Parsed_ast.class_defn to a Typed_ast.class_defn *)
(* Need to typecheck the constructor, methods and fields *)

(* Typecheck the class definitions *)
let type_class_defns class_defns type_env row =
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
    let constructor_type_env = field_types_env @ type_env in
    (* add class information to the class environment *)
    type_expr constructor_expr
      (constructor_args @ constructor_type_env)
      class_defns TSLow row
    >>= fun ((_, _), typed_constructor, _, updated_row) ->
    let type_field_expr (Parsed_ast.FieldDefn (field_name, field_type)) =
      Ok (Typed_ast.FieldDefn (field_name, field_type))
    in
    Result.all (List.map ~f:type_field_expr field_defns)
    >>= fun typed_field_defns ->
    Result.all
      (List.map
         ~f:(fun method_defn ->
           type_function_defn method_defn constructor_type_env class_defns
             updated_row )
         method_defns )
    >>= fun typed_methods ->
    let typed_methods =
      List.map ~f:(fun (_, _, typed_f_defn, _) -> typed_f_defn) typed_methods
    in
    Ok
      (Typed_ast.ClassDefn
         ( class_name
         , typed_field_defns
         , Typed_ast.Constructor
             ( ( match constructor with
               | Parsed_ast.Constructor (args, _) -> args )
             , typed_constructor )
         , typed_methods ) )
  in
  Result.all (List.map ~f:type_class_defn class_defns)
  >>= fun typed_class_defns -> Ok typed_class_defns
