open Core
open Compiler_types.Language_types
open Compiler_types.Ast_types

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
  | TEUnit, TSLow -> "(Unit, Low)"
  | TEUnit, TSHigh -> "(Unit, High)"
  | TEObject obj, TSHigh -> Printf.sprintf "(Object %s, High)" obj
  | TEObject obj, TSLow -> Printf.sprintf "(Object %s, Low)" obj
  | TException e, TSHigh ->
      Printf.sprintf "(Exception %s, High)" (exception_type_to_string e)
  | TException e, TSLow ->
      Printf.sprintf "(Exception %s, Low)" (exception_type_to_string e)

and sec_level_to_string = function TSHigh -> "High" | TSLow -> "Low"

and bin_op_to_string = function
  | BinOpPlus -> "Plus"
  | BinOpMinus -> "Minus"
  | BinOpMultiply -> "Multiply"
  | BinOpDivide -> "Divide"
  | BinOpExponentiation -> "Exponentiation"
  | BinOpModulus -> "Modulus"

and comp_op_to_string = function
  | CompOpLessThan -> "LessThan"
  | CompOpGreaterThan -> "GreaterThan"
  | CompOpLessThanEqual -> "LessThanEqual"
  | CompOpGreaterThanEqual -> "GreaterThanEqual"
  | CompOpEqual -> "Equality"

and bool_op_to_string = function BoolOpAnd -> "And" | BoolOpOr -> "Or"

and unary_op_to_string = function UnaryOpNot -> "Not"

and arg_to_string (TArg (var, arg_type)) =
  Printf.sprintf "%s: %s" var (type_expr_to_string arg_type)

and value_to_string = function
  | VInt i -> Printf.sprintf "VInt(%d)" i
  | VBool b -> Printf.sprintf "VBool(%b)" b
  | VUnit _ -> "VUnit"
  | VObject (obj, args) ->
      let args_str =
        String.concat ~sep:"; "
          (Stdlib.List.map
             (fun value -> Printf.sprintf "%s" (value_to_string value))
             args )
      in
      Printf.sprintf "VObject(%s, {%s})" obj args_str

and exception_type_to_string = function
  | DivisionByZero -> "DivisionByZero"
  | IntegerOverflow -> "IntegerOverflow"
