open Core
open Parser_frontend
open Compiler_types.Language_types
open Equal_type_expr
open Type_environment

let check_var_not_shadowed type_environment var_name =
  let ast_var_type = lookup_var_type type_environment var_name in
  match ast_var_type with
  (* If this is none, it means the variable has not been declared yet *)
  | None -> Ok ()
  | Some _ ->
      Error
        (Error.of_string
           "Variable has already been assigned and has another type" )

let max_security_level sec1 sec2 =
  match (sec1, sec2) with
  | TSLow, TSLow -> TSLow
  | TSHigh, TSHigh -> TSHigh
  | TSLow, TSHigh -> TSHigh
  | TSHigh, TSLow -> TSHigh

let check_expr_var_types_match var_type e1_type =
  if phys_equal var_type e1_type then Ok ()
  else Error (Error.of_string "Types of the let and expression do not match")

let rec type_expr :
    Parsed_ast.expr -> ('a * 'b) list -> (type_expr * 'c, Error.t) result =
 fun expr type_environment ->
  let ( >>= ) = Result.( >>= ) in
  match expr with
  | Parsed_ast.Integer (loc, i, security_level) ->
      Ok ((TEInt, security_level), Typed_ast.Integer (loc, i, security_level))
  | Parsed_ast.Boolean (loc, b, security_level) ->
      Ok
        ((TEBool, security_level), Typed_ast.Boolean (loc, b, security_level))
  | Parsed_ast.BinOp (loc, bin_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      if
        equal_core_type e1_core_type e2_core_type
        && equal_core_type e1_core_type TEInt
      then
        Ok
          ( (TEInt, max_security_level e1_sec_level e2_sec_level)
          , Typed_ast.BinOp
              ( loc
              , (TEInt, max_security_level e1_sec_level e2_sec_level)
              , bin_op
              , typed_e1
              , typed_e2 ) )
      else Error (Error.of_string "binary operands type error")
  | Parsed_ast.CompOp (loc, comp_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      if
        equal_core_type e1_core_type e2_core_type
        && equal_core_type e1_core_type TEInt
      then
        Ok
          ( (TEBool, max_security_level e1_sec_level e2_sec_level)
          , Typed_ast.CompOp
              ( loc
              , (TEBool, max_security_level e1_sec_level e2_sec_level)
              , comp_op
              , typed_e1
              , typed_e2 ) )
      else Error (Error.of_string "comparison operands type error")
  | Parsed_ast.BoolOp (loc, bool_comp_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      if
        equal_core_type e1_core_type e2_core_type
        && equal_core_type e1_core_type TEBool
      then
        Ok
          ( (TEBool, max_security_level e1_sec_level e1_sec_level)
          , Typed_ast.BoolOp
              ( loc
              , (TEBool, max_security_level e1_sec_level e2_sec_level)
              , bool_comp_op
              , typed_e1
              , typed_e2 ) )
      else Error (Error.of_string "boolean comparison operands type error")
  | Parsed_ast.Identifier (loc, id) -> (
      lookup_var_type type_environment id
      |> function
      | None -> Error (Error.of_string "Variable does not exist")
      | Some var_type ->
          Ok (var_type, Typed_ast.Identifier (loc, var_type, id)) )
  | Parsed_ast.Let (loc, var_name, var_type, e1, e2) ->
      check_var_not_shadowed type_environment var_name
      >>= fun () ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      check_expr_var_types_match var_type (e1_core_type, e1_sec_level)
      >>= fun () ->
      type_expr e2 ((var_name, var_type) :: type_environment)
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      Ok
        ( (e2_core_type, e2_sec_level)
        , Typed_ast.Let
            ( loc
            , var_name
            , var_type
            , typed_e1
            , typed_e2
            , (e2_core_type, e2_sec_level) ) )
  | Parsed_ast.Assign (loc, var_name, e1) -> (
      lookup_var_type type_environment var_name
      |> function
      | None -> Error (Error.of_string "Variable does not exist")
      | Some var_type ->
          type_expr e1 type_environment
          >>= fun (e1_type, typed_e1) ->
          if equal_type_expr var_type e1_type then
            Ok
              (var_type, Typed_ast.Assign (loc, var_type, var_name, typed_e1))
          else
            Error
              (Error.of_string
                 "Variable type does not match the assigned expression type" )
      )
  | Parsed_ast.If (loc, e1, e2, e3) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      type_expr e3 type_environment
      >>= fun ((e3_core_type, e3_sec_level), typed_e3) ->
      if
        equal_core_type e1_core_type TEBool
        && equal_core_type e2_core_type e3_core_type
      then
        Ok
          ( ( e2_core_type
            , max_security_level e1_sec_level
                (max_security_level e2_sec_level e3_sec_level) )
          , Typed_ast.If
              ( loc
              , typed_e1
              , typed_e2
              , typed_e3
              , ( e2_core_type
                , max_security_level e1_sec_level
                    (max_security_level e2_sec_level e3_sec_level) ) ) )
      else
        Error
          (Error.of_string
             "Expression types in the if statement are not correct" )
  | Parsed_ast.Classify (loc, e1) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      if equal_security_level_type e1_sec_level TSHigh then
        Error
          (Error.of_string "Cannot classify a high security level expression")
      else
        Ok
          ( (e1_core_type, TSHigh)
          , Typed_ast.Classify (loc, typed_e1, (e1_core_type, TSHigh)) )
  | Parsed_ast.Declassify (loc, e1) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      if equal_security_level_type e1_sec_level TSLow then
        Error
          (Error.of_string
             "Cannot declassify a low security level expression" )
      else
        Ok
          ( (e1_core_type, TSLow)
          , Typed_ast.Declassify (loc, typed_e1, (e1_core_type, TSLow)) )
  | Parsed_ast.FunctionApp (loc, fn_name, args) -> (
      (* get the argument types of the function from the type environment,
         which are the typed arguments defined in the function definition *)
      get_function_types type_environment fn_name
      |> function
      | Error _ -> Error (Error.of_string "Function does not exist")
      | Ok arg_types ->
          let arg_types_env = List.zip_exn arg_types args in
          let arg_types_env_result =
            List.map arg_types_env ~f:(fun (arg_type, arg_expr) ->
                type_expr arg_expr type_environment
                >>= fun (arg_expr_type, typed_arg_expr) ->
                if equal_type_expr arg_type arg_expr_type then
                  Ok (arg_expr_type, typed_arg_expr)
                else
                  Error
                    (Error.of_string
                       "Function argument type does not match the function \
                        type" ) )
          in
          Result.all arg_types_env_result
          >>= fun typed_args ->
          let typed_args_exprs = List.map typed_args ~f:snd in
          let return_type = List.hd_exn arg_types in
          Ok
            ( return_type
            , Typed_ast.FunctionApp
                (loc, return_type, fn_name, typed_args_exprs) ) )
  | Parsed_ast.While (loc, e1, e2) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      if equal_core_type e1_core_type TEBool then
        Ok
          ( (e2_core_type, max_security_level e1_sec_level e2_sec_level)
          , Typed_ast.While
              ( loc
              , typed_e1
              , typed_e2
              , (e2_core_type, max_security_level e1_sec_level e2_sec_level)
              ) )
      else
        Error
          (Error.of_string
             "Expression types in the while statement are not correct" )
  | Parsed_ast.For (loc, e1, range_args, e3) ->
    (* type-check the loop variable e1 *)
    type_expr e1 type_environment
    >>= fun ((e1_core_type, _), typed_e1) ->
    if not (equal_core_type e1_core_type TEInt) then
      Error (Error.of_string "For loop variable must be of type int")
    else
      (* type-check the range arguments (e2) *)
      let rec type_check_range_args args acc =
        match args with
        | [] -> Ok (List.rev acc)
        | arg :: rest ->
            type_expr arg type_environment >>= fun typed_arg ->
            type_check_range_args rest (typed_arg :: acc)
      in
      type_check_range_args range_args []
      >>= fun typed_range_args ->
      (match typed_range_args with
      | [((TEInt, sec1), t1)] ->
          Ok ([t1], sec1)
      | [((TEInt, sec1), t1); ((TEInt, sec2), t2)] ->
          Ok ([t1; t2], max_security_level sec1 sec2)
      | [((TEInt, sec1), t1); ((TEInt, sec2), t2); ((TEInt, sec3), t3)] ->
          Ok ([t1; t2; t3], max_security_level (max_security_level sec1 sec2) sec3)
      | _ ->
          Error
            (Error.of_string
                "Invalid range: For loops accept 1 (end), 2 (start, end), or 3 (start, end, step) arguments"))
      >>= fun (typed_range_args, range_sec_level) ->
      (* type-check the body e3 *)
      type_expr e3 type_environment
      >>= fun ((e3_core_type, e3_sec_level), typed_e3) ->
      let overall_sec_level = max_security_level range_sec_level e3_sec_level in
      Ok
        ( (e3_core_type, overall_sec_level)
        , Typed_ast.For
            ( loc
            , typed_e1
            , typed_range_args
            , typed_e3
            , (e3_core_type, overall_sec_level) ) )
          