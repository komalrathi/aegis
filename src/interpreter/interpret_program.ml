open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment

let apply_int_bin_op bin_op i1 i2 =
  match bin_op with
  | PLUS -> Ok (VInt (i1 + i2))
  | MINUS -> Ok (VInt (i1 - i2))
  | MULTIPLY -> Ok (VInt (i1 * i2))
  | DIVIDE ->
      if phys_equal i2 0 then Error (Error.of_string "Division by zero")
      else Ok (VInt (i1 / i2))

let apply_comp_op comp_op i1 i2 =
  match comp_op with
  | LT -> Ok (VBool (i1 < i2))
  | GT -> Ok (VBool (i1 > i2))
  | LTE -> Ok (VBool (i1 <= i2))
  | GTE -> Ok (VBool (i1 >= i2))

let interpret_bin_op bin_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_int_bin_op bin_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Error.of_string
           "Type error: cannot apply binary operation to boolean values" )

let interpret_comp_op comp_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_comp_op comp_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Error.of_string
           "Type error: cannot apply comparison operation to boolean values" )

let rec interpret_expr expr value_environment =
  let ( >>= ) = Result.( >>= ) in
  match expr with
  | Integer (_, i, _) -> Ok (VInt i)
  | Boolean (_, b, _) -> Ok (VBool b)
  | BinOp (_, _, bin_op, e1, e2) ->
      interpret_expr e1 value_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment
      >>= fun val2 -> interpret_bin_op bin_op val1 val2
  | CompOp (_, _, comp_op, e1, e2) ->
      interpret_expr e1 value_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment
      >>= fun val2 -> interpret_comp_op comp_op val1 val2
  | BoolCompOp (_, _, bool_comp_op, e1, e2) -> (
      interpret_expr e1 value_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment
      >>= fun val2 ->
      match (val1, val2) with
      | VBool b1, VBool b2 -> (
        match bool_comp_op with
        | AND -> Ok (VBool (b1 && b2))
        | OR -> Ok (VBool (b1 || b2)) )
      | VInt _, VBool _ | VBool _, VInt _ | VInt _, VInt _ ->
          Error
            (Error.of_string
               "Type error: cannot apply boolean operation to integer values" )
      )
  (* need to store the value of the identifier in the environment *)
  | Identifier (_, var_type, identifier) -> (
      lookup_var_value value_environment identifier
      |> function
      | None -> Error (Error.of_string "Variable does not have a value")
      | Some var_value -> (
        match (var_type, var_value) with
        | (TEInt, _), VInt i -> Ok (VInt i)
        | (TEBool, _), VBool b -> Ok (VBool b)
        | _ ->
            Error
              (Error.of_string
                 "Type error: variable type does not match value type" ) ) )
  | Let (_, var_name, _, e1, e2, _) ->
      interpret_expr e1 value_environment
      >>= fun val1 ->
      let new_env = (var_name, val1) :: value_environment in
      interpret_expr e2 new_env
  | Assign (_, var_type, var_name, e1) -> (
      interpret_expr e1 value_environment
      >>= fun val1 ->
      match (var_type, val1) with
      | (TEInt, _), VInt _ | (TEBool, _), VBool _ ->
          let _ = (var_name, val1) :: value_environment in
          Ok val1
      | _ ->
          Error
            (Error.of_string
               "Type error: variable type does not match value type" ) )
  | If (_, e1, e2, e3, _) -> (
      interpret_expr e1 value_environment
      >>= fun val1 ->
      match val1 with
      | VBool true -> interpret_expr e2 value_environment
      | VBool false -> interpret_expr e3 value_environment
      | VInt _ ->
          Error
            (Error.of_string
               "Type error: Cannot have int type in the if condition" ) )
  | Classify (_, e1, e1_type_expr) -> (
      interpret_expr e1 value_environment
      >>= fun val1 ->
      match (val1, e1_type_expr) with
      | VInt i, (TEInt, TSHigh) -> Ok (VInt i)
      | VBool b, (TEBool, TSHigh) -> Ok (VBool b)
      | _ ->
          Error
            (Error.of_string
               "Type error: cannot classify value with different type" ) )
  | Declassify (_, e1, e1_type_expr) -> (
      interpret_expr e1 value_environment
      >>= fun val1 ->
      match (val1, e1_type_expr) with
      | VInt i, (TEInt, TSLow) -> Ok (VInt i)
      | VBool b, (TEBool, TSLow) -> Ok (VBool b)
      | _ ->
          Error
            (Error.of_string
               "Type error: cannot classify value with different type" ) )

let interpret_program (Prog expr) =
  let initial_environment = [] in
  interpret_expr expr initial_environment
