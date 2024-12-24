open Core
open Parser_frontend
open Parser_frontend.Parsed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types

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

and sec_level_to_string = function TSHigh -> "High" | TSLow -> "Low"

let rec expr_to_string = function
  | Identifier (_, id) -> Printf.sprintf "Identifier(%s)" id
  | BinOp (_, op, e1, e2) ->
      Printf.sprintf "BinOp(%s, %s, %s)" (bin_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | Boolean (_, b, _) -> Printf.sprintf "Boolean(%b)" b
  | Integer (_, i, _) -> Printf.sprintf "Integer(%d)" i
  | If (_, e1, e2, e3) ->
      Printf.sprintf "If(%s, %s, %s)" (expr_to_string e1) (expr_to_string e2)
        (expr_to_string e3)
  | Let (_, var, typ, e1, e2) ->
      Printf.sprintf "Let(%s, %s, %s, %s)" var (type_expr_to_string typ)
        (expr_to_string e1) (expr_to_string e2)
  | Assign (_, var, e) ->
      Printf.sprintf "Assign(%s, %s)" var (expr_to_string e)
  | CompOp (_, op, e1, e2) ->
      Printf.sprintf "CompOp(%s, %s, %s)" (comp_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | BoolOp (_, op, e1, e2) ->
      Printf.sprintf "BoolOp(%s, %s, %s)" (bool_op_to_string op)
        (expr_to_string e1) (expr_to_string e2)
  | Classify (_, e) -> Printf.sprintf "Classify(%s)" (expr_to_string e)
  | Declassify (_, e) -> Printf.sprintf "Declassify(%s)" (expr_to_string e)
  | For (_, var, e1, e2) ->
      Printf.sprintf "For(%s, [%s], %s)" (expr_to_string var)
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string e1))
        (expr_to_string e2)
  | While (_, e1, e2) ->
      Printf.sprintf "While(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | FunctionApp (_, id, args) ->
      Printf.sprintf "FunctionApp(%s, [%s])" id
        (String.concat ~sep:"; " (Stdlib.List.map expr_to_string args))

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

and bool_op_to_string = function BoolOpAnd -> "And" | BoolOpOr -> "Or"

and arg_to_string (TArg (var, arg_type)) =
  Printf.sprintf "%s: %s" var (type_expr_to_string arg_type)

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

let print_parsed_ast input =
  match Parse_program.parse_program input with
  | Ok result -> print_endline (program_to_string result)
  | Error err -> print_endline (Error.to_string_hum err)
