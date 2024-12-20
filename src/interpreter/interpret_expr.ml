open Core
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment

type function_environment = (identifier * (identifier list * expr)) list

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
  | FunctionApp (_, _, f_name, args) -> (
    match Stdlib.List.assoc_opt f_name function_environment with
    | Some (param_names, body) ->
        let rec eval_args args acc =
          match args with
          | [] -> Ok (List.rev acc)
          | arg :: rest ->
              interpret_expr arg value_environment function_environment
              >>= fun v -> eval_args rest (v :: acc)
        in
        eval_args args []
        >>= fun arg_values ->
        if List.length param_names <> List.length arg_values then
          Error
            (Error.of_string
               (Printf.sprintf
                  "Arity mismatch in function '%s': expected %d arguments \
                   but got %d"
                  f_name (List.length param_names) (List.length arg_values) ) )
        else
          let new_env =
            match List.zip param_names arg_values with
            | Ok pairs -> pairs @ value_environment
            | Unequal_lengths ->
                failwith
                  "Arity mismatch: number of arguments provided does not \
                   match the function's parameter count"
          in
          interpret_expr body new_env function_environment
    | None -> Error (Error.of_string "Function not found") )
  | While (_, e1, e2, _) ->
      let rec loop () =
        interpret_expr e1 value_environment function_environment
        >>= fun val1 ->
        match val1 with
        | VBool true ->
            (* if condition is true, execute the body e2 and repeat the
               loop *)
            interpret_expr e2 value_environment function_environment
            >>= fun _ ->
            loop () (* Recursively call loop to repeat the evaluation *)
        | VBool false ->
            (* if condition is false, the loop terminates and returns
               false *)
            Ok (VBool false)
        | _ ->
            Error
              (Error.of_string
                 "Type error: Condition in while loop must be a boolean" )
      in
      loop ()
  (* | For (_, _, range_expr, body_expr, _) -> ( (* range_expr is a list of
     exprs, so we need to evaluate it to get the range *) let rec
     eval_range_expr range_expr acc = match range_expr with | [] -> Ok
     (List.rev acc) | expr :: rest -> interpret_expr expr value_environment
     function_environment >>= fun v -> eval_range_expr rest (v :: acc) in
     eval_range_expr range_expr [] >>= fun range_values -> match range_values
     with (* one integer value in the range *) | [VInt range] -> let new_env
     = ("e1", VInt 0) :: value_environment in let rec loop i = if i > range
     then Ok (VInt i) else interpret_expr body_expr new_env
     function_environment >>= fun _ -> loop (i + 1) in loop 0 (* two integer
     values in the range, start and end *) | [VInt start_val; VInt end_val]
     -> let new_env = ("e1", VInt start_val) :: value_environment in let rec
     loop i = if i > end_val then Ok (VInt i) else interpret_expr body_expr
     new_env function_environment >>= fun _ -> loop (i + 1) in loop start_val
     (* three integer values in the range, start, end, and step *) | [VInt
     start_val; VInt end_val; VInt step] -> let new_env = ("e1", VInt
     start_val) :: value_environment in let rec loop i = if i > end_val then
     Ok (VInt i) else interpret_expr body_expr new_env function_environment
     >>= fun _ -> loop (i + step) in loop start_val | _ -> Error
     (Error.of_string "Type error: Range must be have a maximum of 3
     integer\n\ \ values" ) ) *)
  | For (_, _, range_expr, body_expr, _) -> (
      (* range_expr is a list of exprs, so we need to evaluate it to get the
         range *)
      let rec eval_range_expr range_expr acc =
        match range_expr with
        | [] -> Ok (List.rev acc)
        | expr :: rest ->
            interpret_expr expr value_environment function_environment
            >>= fun v -> eval_range_expr rest (v :: acc)
      in
      eval_range_expr range_expr []
      >>= fun range_values ->
      let rec loop i stop step =
        if (step > 0 && i > stop) || (step < 0 && i < stop) then Ok (VInt i)
        else
          interpret_expr body_expr
            (("e1", VInt i) :: value_environment)
            function_environment
          >>= fun _ -> loop (i + step) stop step
      in
      match range_values with
      | [VInt range] -> loop 0 range 1
      | [VInt start_val; VInt end_val] -> loop start_val end_val 1
      | [VInt start_val; VInt end_val; VInt step] ->
          if step = 0 then
            Error (Error.of_string "Step value cannot be zero")
          else loop start_val end_val step
      | _ ->
          Error
            (Error.of_string
               "Type error: Range must have 1 to 3 integer values" ) )

let rec interpret_fn_defns fn_defns function_environment =
  match fn_defns with
  | [] -> Ok function_environment
  | FunctionDefn (fn_name, param_list, (_, _), fn_body) :: fn_defns ->
      (* get the list of parameter names from the argument list *)
      let param_names =
        Stdlib.List.map (fun (TArg (id, _)) -> id) param_list
      in
      (* add function to the function_environment *)
      let new_env =
        (fn_name, (param_names, fn_body)) :: function_environment
      in
      interpret_fn_defns fn_defns new_env
