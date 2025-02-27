open Core
open Interpreter.Interpret_expr
open Typing.Typed_ast
open Print_compiler_types

let rec expr_to_string = function
  | Integer (_, i, _) -> Printf.sprintf "Integer(%d)" i
  | Boolean (_, b, _) -> Printf.sprintf "Boolean(%b)" b
  | BinOp (_, _, op, e1, e2) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (bin_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | CompOp (_, _, op, e1, e2) ->
      Printf.sprintf "CompOp(%s, %s, %s)" (comp_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | BoolOp (_, _, op, e1, e2) ->
      Printf.sprintf "BoolOp(%s, %s, %s)" (bool_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | UnaryOp (_, _, op, e) ->
      Printf.sprintf "UnaryOp(%s, %s)" (unary_op_to_string op)
        (expr_to_string e)
  | Identifier (_, id, _) ->
      Printf.sprintf "Identifier(%s)" (type_expr_to_string id)
  | If (_, e1, e2, e3, _) ->
      Printf.sprintf "If(%s, %s, %s)" (expr_to_string e1) (expr_to_string e2)
        (expr_to_string e3)
  | Let (_, var, _, e1, e2, _) ->
      Printf.sprintf "Let(%s, %s, %s)" var (expr_to_string e1)
        (expr_to_string e2)
  | Assign (_, _, var, e) ->
      Printf.sprintf "Assign(%s, %s)" var (expr_to_string e)
  | Classify (_, e, _) -> Printf.sprintf "Classify(%s)" (expr_to_string e)
  | Declassify (_, e, _) ->
      Printf.sprintf "Declassify(%s)" (expr_to_string e)
  | FunctionApp (_, _, id, args) ->
      Printf.sprintf "FunctionApp(%s, [%s])" id
        (String.concat ~sep:"; " (List.map ~f:expr_to_string args))
  | While (_, e1, e2, _) ->
      Printf.sprintf "While(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Seq (_, e1, e2, _) ->
      Printf.sprintf "Seq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Print (_, args) ->
      Printf.sprintf "Print([%s])"
        (String.concat ~sep:"; " (List.map ~f:expr_to_string args))
  | SecurePrint (_, args) ->
      Printf.sprintf "SecurePrint([%s])"
        (String.concat ~sep:"; " (List.map ~f:expr_to_string args))
  | Skip _ -> "Skip"
  | Object (_, sec_level, id, fields, _) ->
      Printf.sprintf "Object(%s, %s, [%s])"
        (sec_level_to_string sec_level)
        id
        (String.concat ~sep:"; " (List.map ~f:expr_to_string fields))
  | MethodCall (_, _, _, id, args) ->
      Printf.sprintf "MethodCall(%s, [%s])" id
        (String.concat ~sep:"; " (List.map ~f:expr_to_string args))
  | Raise (_, exception_name, var_name, type_expr) ->
      Printf.sprintf "Raise(%s, %s %s)"
        (exception_type_to_string exception_name)
        var_name
        (type_expr_to_string type_expr)
  | TryCatchFinally (_, e1, exception_name, var_name, e2, e3, _) ->
      Printf.sprintf "TryCatchFinally(%s, %s, %s, %s, %s)"
        (expr_to_string e1)
        (exception_type_to_string exception_name)
        var_name (expr_to_string e2) (expr_to_string e3)

let rec function_environment_to_string env =
  match env with
  | (name, (args, body)) :: t ->
      Printf.sprintf "%s(%s) -> %s; %s" name
        (String.concat ~sep:", " args)
        (expr_to_string body)
        (function_environment_to_string t)
  | [] -> ""

let rec value_environment_to_string env =
  match env with
  | (var, value) :: t ->
      Printf.sprintf "%s -> %s; %s" var (value_to_string value)
        (value_environment_to_string t)
  | [] -> ""

let rec class_defns_to_string (class_defns : class_defn list) =
  match class_defns with
  | ClassDefn (class_name, fields, constructor, methods) :: t ->
      Printf.sprintf "Class %s: %s; %s; %s; %s" class_name
        (String.concat ~sep:", " (List.map ~f:field_to_string fields))
        (constructor_to_string constructor)
        (String.concat ~sep:"; "
           (List.map ~f:function_defn_to_string methods) )
        (class_defns_to_string t)
  | [] -> ""

(* and field_to_string (name, (type_expr, sec_level)) = Printf.sprintf "%s:
   %s, %s" name (type_expr_to_string (type_expr, sec_level))
   (sec_level_to_string sec_level) *)
and field_to_string field_name =
  match field_name with
  | FieldDefn (name, type_expr) ->
      Printf.sprintf "%s: %s" name (type_expr_to_string type_expr)

and constructor_to_string constructor =
  match constructor with
  | Constructor (args, expr) ->
      Printf.sprintf "Constructor(%s, %s)"
        (String.concat ~sep:", " (List.map ~f:arg_to_string args))
        (expr_to_string expr)

and function_defn_to_string fn_defn =
  match fn_defn with
  | FunctionDefn (name, args, _, body) ->
      Printf.sprintf "%s(%s) -> %s" name
        (String.concat ~sep:", " (List.map ~f:arg_to_string args))
        (expr_to_string body)

let print_interpret_expr expr value_env function_env class_defns =
  match interpret_expr expr value_env function_env class_defns with
  | Ok (IValue (value, new_value_env)) ->
      Printf.printf "Result: %s\n" (value_to_string value) ;
      Printf.printf "Value Environment: %s\n"
        (value_environment_to_string new_value_env) ;
      Printf.printf "Function Environment: %s\n"
        (function_environment_to_string function_env) ;
      Printf.printf "Class Environment: %s\n"
        (class_defns_to_string class_defns)
  | Ok (IException (err, _)) ->
      Printf.printf "Error: %s\n" (exception_type_to_string err)
  | Error err -> Printf.printf "Error: %s\n" (Error.to_string_hum err)
