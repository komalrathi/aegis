open Core
open Interpreter.Interpret_expr
open Interpreter.Interpret_class_defn
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

let rec class_environment_to_string env =
  match env with
  | (name, {fields; constructor; methods}) :: t ->
      Printf.sprintf "%s { %s; %s; %s } %s" name
        (String.concat ~sep:"; " (List.map ~f:field_to_string fields))
        (constructor_to_string constructor)
        (String.concat ~sep:"; " (List.map ~f:method_to_string methods))
        (class_environment_to_string t)
  | [] -> ""

(* and field_to_string (name, (type_expr, sec_level)) = Printf.sprintf "%s:
   %s, %s" name (type_expr_to_string (type_expr, sec_level))
   (sec_level_to_string sec_level) *)
and field_to_string field_name = field_name

and constructor_to_string (args, body) =
  Printf.sprintf "Constructor(%s, %s)"
    (String.concat ~sep:", " args)
    (expr_to_string body)

(* and method_to_string (sec_level, (name, args, body)) = Printf.sprintf
   "%s(%s) -> %s, %s" name (String.concat ~sep:", " (List.map
   ~f:arg_to_string args)) (expr_to_string body) (sec_level_to_string
   sec_level) *)

and method_to_string (sec_level, method_name, arg_names, expr) =
  (* Print method details: security level, name, and arguments *)
  Printf.sprintf "%s %s(%s) -> %s"
    (sec_level_to_string sec_level)
    method_name
    (String.concat ~sep:", " arg_names)
    (expr_to_string expr)

(* and arg_to_string (name, (type_expr, sec_level)) = Printf.sprintf "%s: %s,
   %s" name (type_expr_to_string (type_expr, sec_level)) (sec_level_to_string
   sec_level) *)

let print_interpret_expr expr value_env function_env class_env =
  match interpret_expr expr value_env function_env class_env with
  | Ok (value, new_value_env) ->
      Printf.printf "Result: %s\n" (value_to_string value) ;
      Printf.printf "Value Environment: %s\n"
        (value_environment_to_string new_value_env) ;
      Printf.printf "Function Environment: %s\n"
        (function_environment_to_string function_env) ;
      Printf.printf "Class Environment: %s\n"
        (class_environment_to_string class_env)
  | Error e -> Printf.printf "Error: %s\n" (Error.to_string_hum e)
(* | Ok (value, new_env) -> Printf.sprintf "Function Environment: %s\nResult:
   %s\nValue Environment: [%s]\n" (function_environment_to_string
   function_env) (value_to_string value) (value_environment_to_string
   new_env) |> print_endline | Error err -> Printf.sprintf "Error: %s\n"
   (Error.to_string_hum err) |> print_endline *)
