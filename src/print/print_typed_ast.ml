open Core
open Typing.Typed_ast
open Print_compiler_types

let rec expr_to_string = function
  | Identifier (_, type_expr, id) ->
      Printf.sprintf "Identifier(%s, %s)" id (type_expr_to_string type_expr)
  | BinOp (_, type_expr, op, e1, e2) ->
      Printf.sprintf "BinOp(%s, %s, %s, %s)" (bin_op_to_string op)
        (type_expr_to_string type_expr)
        (expr_to_string e1) (expr_to_string e2)
  | Boolean (_, b, _) -> Printf.sprintf "Boolean(%b)" b
  | Integer (_, i, _) -> Printf.sprintf "Integer(%d)" i
  | If (_, e1, e2, e3, type_expr) ->
      Printf.sprintf "If(%s, %s, %s, %s)" (expr_to_string e1)
        (expr_to_string e2) (expr_to_string e3)
        (type_expr_to_string type_expr)
  | Let (_, var, var_type_expr, e1, e2, type_expr) ->
      Printf.sprintf "Let(%s, %s, %s, %s, %s)" var
        (type_expr_to_string var_type_expr)
        (expr_to_string e1) (expr_to_string e2)
        (type_expr_to_string type_expr)
  | Assign (_, type_expr, var, e) ->
      Printf.sprintf "Assign(%s, %s, %s)"
        (type_expr_to_string type_expr)
        var (expr_to_string e)
  | CompOp (_, type_expr, op, e1, e2) ->
      Printf.sprintf "CompOp(%s, %s, %s, %s)" (comp_op_to_string op)
        (type_expr_to_string type_expr)
        (expr_to_string e1) (expr_to_string e2)
  | BoolOp (_, type_expr, op, e1, e2) ->
      Printf.sprintf "BoolOp(%s, %s, %s, %s)" (bool_op_to_string op)
        (type_expr_to_string type_expr)
        (expr_to_string e1) (expr_to_string e2)
  | UnaryOp (_, type_expr, op, e) ->
      Printf.sprintf "UnaryOp(%s, %s, %s)" (unary_op_to_string op)
        (type_expr_to_string type_expr)
        (expr_to_string e)
  | Classify (_, e, type_expr) ->
      Printf.sprintf "Classify(%s %s)" (expr_to_string e)
        (type_expr_to_string type_expr)
  | Declassify (_, e, type_expr) ->
      Printf.sprintf "Declassify(%s %s)" (expr_to_string e)
        (type_expr_to_string type_expr)
  | While (_, e1, e2, type_expr) ->
      Printf.sprintf "While(%s, %s, %s)" (expr_to_string e1)
        (expr_to_string e2)
        (type_expr_to_string type_expr)
  | FunctionApp (_, type_expr, id, args) ->
      Printf.sprintf "FunctionApp(%s, %s, [%s])" id
        (type_expr_to_string type_expr)
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string args))
  | Seq (_, e1, e2, type_expr) ->
      Printf.sprintf "Seq(%s, %s, %s)" (expr_to_string e1)
        (expr_to_string e2)
        (type_expr_to_string type_expr)
  | Print (_, args) ->
      Printf.sprintf "Print([%s])"
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string args))
  | SecurePrint (_, args) ->
      Printf.sprintf "SecurePrint([\n%s])"
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string args))
  | Skip _ -> "Skip"
  | Object (_, sec_level, id, fields, type_expr) ->
      Printf.sprintf "Object(%s, %s, [%s], %s)"
        (sec_level_to_string sec_level)
        id
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string fields))
        (type_expr_to_string type_expr)
  | MethodCall (_, type_expr, e, id, args) ->
      Printf.sprintf "MethodCall(%s, %s, %s, [%s])" e id
        (type_expr_to_string type_expr)
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string args))
  | Raise (_, exception_name, var_name, type_expr) ->
      Printf.sprintf "Raise(%s, %s) %s"
        (exception_type_to_string exception_name)
        var_name
        (type_expr_to_string type_expr)
  | ResumableRaise (_, exception_name, var_name, type_expr) ->
      Printf.sprintf "ResumableRaise(%s, %s) %s"
        (exception_type_to_string exception_name)
        var_name
        (type_expr_to_string type_expr)
  | TryCatchFinally
      (_, e1, exception_name, var_name, continuation, e2, e3, type_expr) -> (
    match continuation with
    | None ->
        Printf.sprintf "Try {%s} Catch (%s %s) {%s} Finally {%s} %s"
          (expr_to_string e1)
          (exception_type_to_string exception_name)
          var_name (expr_to_string e2) (expr_to_string e3)
          (type_expr_to_string type_expr)
    | Some continuation ->
        Printf.sprintf "Try {%s} Catch (%s %s %s) {%s} Finally {%s} %s"
          (expr_to_string e1)
          (exception_type_to_string exception_name)
          var_name continuation (expr_to_string e2) (expr_to_string e3)
          (type_expr_to_string type_expr) )
  | Continue (_, k, e, type_expr) ->
      Printf.sprintf "Continue (%s, %s, %s)" k (expr_to_string e)
        (type_expr_to_string type_expr)

let function_defn_to_string (FunctionDefn (name, args, return_type, body)) =
  Printf.sprintf "FunctionDefn(%s, [%s], %s, %s)" name
    (String.concat ~sep:"; " (Stdlib.List.map arg_to_string args))
    (type_expr_to_string return_type)
    (expr_to_string body)

let constructor_to_string (Constructor (args, body)) =
  Printf.sprintf "Constructor([%s], %s)"
    (String.concat ~sep:"; " (Stdlib.List.map arg_to_string args))
    (expr_to_string body)

let class_defn_to_string (ClassDefn (name, fields, constructor, methods)) =
  let fields_string =
    String.concat ~sep:"\n"
      (Stdlib.List.map
         (fun (FieldDefn (field_name, field_type)) ->
           Printf.sprintf "FieldDefn(%s, %s)" field_name
             (type_expr_to_string field_type) )
         fields )
  in
  let methods_string =
    String.concat ~sep:"\n" (Stdlib.List.map function_defn_to_string methods)
  in
  Printf.sprintf "ClassDefn(%s, %s, %s, %s)" name fields_string
    (constructor_to_string constructor)
    methods_string

let program_to_string (Prog (class_defns, function_defns, expr)) =
  let class_names_string =
    String.concat ~sep:"\n"
      (Stdlib.List.map class_defn_to_string class_defns)
  in
  let function_names_string =
    String.concat ~sep:"\n"
      (Stdlib.List.map function_defn_to_string function_defns)
  in
  Printf.sprintf "Program([\n%s\n],[\n%s\n], %s)" class_names_string
    function_names_string (expr_to_string expr)

let print_typed_ast input =
  match Typing.Type_program.type_program input with
  | Ok result -> print_endline (program_to_string result)
  | Error err -> print_endline (Error.to_string_hum err)
