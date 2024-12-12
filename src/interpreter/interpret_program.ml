open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment

let apply_int_bin_op bin_op i1 i2 =
  match bin_op with
  | BinOpPlus -> Ok (VInt (i1 + i2))
  | BinOpMinus -> Ok (VInt (i1 - i2))
  | BinOpMultiply -> Ok (VInt (i1 * i2))
  | BinOpDivide ->
      if phys_equal i2 0 then Error (Error.of_string "Division by zero")
      else Ok (VInt (i1 / i2))

let apply_comp_op comp_op i1 i2 =
  match comp_op with
  | CompOpLessThan -> Ok (VBool (i1 < i2))
  | CompOpGreaterThan -> Ok (VBool (i1 > i2))
  | CompOpLessThanEqual -> Ok (VBool (i1 <= i2))
  | CompOpGreaterThanEqual -> Ok (VBool (i1 >= i2))

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

let rec interpret_expr expr value_environment function_environment =
  let ( >>= ) = Result.( >>= ) in
  match expr with
  | Integer (_, i, _) -> Ok (VInt i)
  | Boolean (_, b, _) -> Ok (VBool b)
  | BinOp (_, _, bin_op, e1, e2) ->
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment function_environment
      >>= fun val2 -> interpret_bin_op bin_op val1 val2
  | CompOp (_, _, comp_op, e1, e2) ->
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment function_environment
      >>= fun val2 -> interpret_comp_op comp_op val1 val2
  | BoolOp (_, _, bool_comp_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      interpret_expr e2 value_environment function_environment
      >>= fun val2 ->
      match (val1, val2) with
      | VBool b1, VBool b2 -> (
        match bool_comp_op with
        | BoolOpAnd -> Ok (VBool (b1 && b2))
        | BoolOpOr -> Ok (VBool (b1 || b2)) )
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
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      let new_env = (var_name, val1) :: value_environment in
      interpret_expr e2 new_env function_environment
  | Assign (_, var_type, var_name, e1) -> (
      interpret_expr e1 value_environment function_environment
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
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      match val1 with
      | VBool true ->
          interpret_expr e2 value_environment function_environment
      | VBool false ->
          interpret_expr e3 value_environment function_environment
      | VInt _ ->
          Error
            (Error.of_string
               "Type error: Cannot have int type in the if condition" ) )
  | Classify (_, e1, e1_type_expr) -> (
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      match (val1, e1_type_expr) with
      | VInt i, (TEInt, TSHigh) -> Ok (VInt i)
      | VBool b, (TEBool, TSHigh) -> Ok (VBool b)
      | _ ->
          Error
            (Error.of_string
               "Type error: cannot classify value with different type" ) )
  | Declassify (_, e1, e1_type_expr) -> (
      interpret_expr e1 value_environment function_environment
      >>= fun val1 ->
      match (val1, e1_type_expr) with
      | VInt i, (TEInt, TSLow) -> Ok (VInt i)
      | VBool b, (TEBool, TSLow) -> Ok (VBool b)
      | _ ->
          Error
            (Error.of_string
               "Type error: cannot classify value with different type" ) )
  (* expr_list is the list of function arguments *)
  | FunctionApp (_, _, fn_name, argument_list) ->
      (* type mismatching here - need to append {fn_name : identifier,
         fn_value : interpreter_val} rather than the arguments *)
      get_function_value value_environment fn_name
      >>= fun fn_value ->
      let rec interpret_arg_exprs expr_list value_environment =
        match expr_list with
        | [] -> Ok []
        | expr :: exprs ->
            interpret_expr expr value_environment function_environment
            >>= fun arg_val ->
            interpret_arg_exprs exprs value_environment
            >>= fun arg_vals -> Ok (arg_val :: arg_vals)
      in
      interpret_arg_exprs argument_list value_environment
      >>= fun arg_vals ->
      (* Now we have a list of argument values. We need to evaluate the
         function body with these values, and get the result. All the
         arg_vals are of type VInt or VBool *)
      interpret_expr expr
        ((fn_name, fn_value) :: value_environment)
        ((fn_name, arg_vals) :: function_environment)
(* | _ -> Error (Error.of_string "Not implemented") *)

let rec interpret_fn_defns fn_defns value_environment =
  match fn_defns with
  | [] -> Ok value_environment
  | FunctionDefn (fn_name, _, (core_type, _), _) :: fn_defns ->
      let fn_value =
        if phys_equal core_type TEInt then VInt 0 else VBool false
      in
      let new_env = (fn_name, fn_value) :: value_environment in
      interpret_fn_defns fn_defns new_env

let interpret_program (Prog (typed_fn_defns, typed_expr)) =
  (* let initial_environment = [] in interpret_expr typed_expr
     initial_environment *)
  let ( >>= ) = Result.( >>= ) in
  interpret_fn_defns typed_fn_defns []
  >>= fun value_environment -> interpret_expr typed_expr value_environment []