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

let function_defn_to_string (FunctionDefn (name, args, return_type, body)) =
  Printf.sprintf "FunctionDefn(%s, [%s], %s, %s)" name
    (String.concat ~sep:"; " (Stdlib.List.map arg_to_string args))
    (type_expr_to_string return_type)
    (expr_to_string body)

let program_to_string (Prog (function_definitions, expr)) =
  let function_names_string =
    String.concat ~sep:"\n"
      (Stdlib.List.map function_defn_to_string function_definitions)
  in
  Printf.sprintf "Program([\n%s\n], %s)" function_names_string
    (expr_to_string expr)

let print_typed_ast input =
  match Typing.Type_program.type_program input with
  | Ok result -> print_endline (program_to_string result)
  | Error err -> print_endline (Error.to_string_hum err)
