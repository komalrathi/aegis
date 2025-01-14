open Core
open Interpreter.Interpret_expr
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Typing.Typed_ast

let value_to_string = function
  | VInt i -> Printf.sprintf "VInt(%d)" i
  | VBool b -> Printf.sprintf "VBool(%b)" b
  | VUnit _ -> "VUnit"

and unary_op_to_string = function UnaryOpNot -> "Not"

and bin_op_to_string = function
  | BinOpPlus -> "Plus"
  | BinOpMinus -> "Minus"
  | BinOpMultiply -> "Multiply"
  | BinOpDivide -> "Divide"

and comp_op_to_string = function
  | CompOpLessThan -> "LessThan"
  | CompOpGreaterThan -> "GreaterThan"
  | CompOpLessThanEqual -> "LessThanEqual"
  | CompOpGreaterThanEqual -> "GreaterThanEqual"
  | CompOpEqual -> "Equal"

and bool_op_to_string = function BoolOpAnd -> "And" | BoolOpOr -> "Or"

let rec type_expr_to_string = function
  | TEInt, TSHigh -> "(Int, High)"
  | TEInt, TSLow -> "(Int, Low)"
  | TEBool, TSHigh -> "(Bool, High)"
  | TEBool, TSLow -> "(Bool, Low)"
  | TFunction (args, return_type), sec_level ->
      let args_str =
        String.concat ~sep:"; "
          (Stdlib.List.map
             (fun arg -> type_expr_to_string (arg, sec_level))
             args )
      in
      Printf.sprintf "(Function (%s) -> %s, %s)" args_str
        (type_expr_to_string (return_type, sec_level))
        (sec_level_to_string sec_level)
  | TEUnit, TSHigh -> "(Unit, High)"
  | TEUnit, TSLow -> "(Unit, Low)"

and sec_level_to_string = function TSHigh -> "High" | TSLow -> "Low"

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

let print_interpret_expr expr value_env function_env =
  match interpret_expr expr value_env function_env with
  | Ok (value, new_env) ->
      Printf.sprintf
        "Function Environment: %s\nResult: %s\nValue Environment: [%s]\n"
        (function_environment_to_string function_env)
        (value_to_string value)
        (value_environment_to_string new_env)
      |> print_endline
  | Error err ->
      Printf.sprintf "Error: %s\n" (Error.to_string_hum err) |> print_endline
