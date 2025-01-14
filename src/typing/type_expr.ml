open Core
open Parser_frontend
open Compiler_types.Language_types
open Equal_type_expr
open Type_environment
open Subtyping_security_levels_check

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

let rec type_expr expr type_environment pc =
  let ( >>= ) = Result.( >>= ) in
  match expr with
  | Parsed_ast.Integer (loc, i, security_level) ->
      Ok
        ( (TEInt, security_level)
        , Typed_ast.Integer (loc, i, security_level)
        , pc )
  | Parsed_ast.Boolean (loc, b, security_level) ->
      Ok
        ( (TEBool, security_level)
        , Typed_ast.Boolean (loc, b, security_level)
        , pc )
  | Parsed_ast.BinOp (loc, bin_op, e1, e2) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
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
              , typed_e2 )
          , pc )
      else Error (Error.of_string "binary operands type error")
  | Parsed_ast.CompOp (loc, comp_op, e1, e2) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
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
              , typed_e2 )
          , pc )
      else Error (Error.of_string "comparison operands type error")
  | Parsed_ast.BoolOp (loc, bool_comp_op, e1, e2) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
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
              , typed_e2 )
          , pc )
      else Error (Error.of_string "boolean comparison operands type error")
  | Parsed_ast.UnaryOp (loc, unary_op, e1) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      if equal_core_type e1_core_type TEBool then
        Ok
          ( (TEBool, e1_sec_level)
          , Typed_ast.UnaryOp
              (loc, (TEBool, e1_sec_level), unary_op, typed_e1)
          , pc )
      else Error (Error.of_string "unary operand type error")
  | Parsed_ast.Identifier (loc, id) -> (
      lookup_var_type type_environment id
      |> function
      | None -> Error (Error.of_string "Variable does not exist")
      | Some var_type ->
          Ok (var_type, Typed_ast.Identifier (loc, var_type, id), pc) )
  | Parsed_ast.Let (loc, var_name, (var_core_type, var_sec_level), e1, e2) ->
      check_var_not_shadowed type_environment var_name
      >>= fun () ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      if
        subtyping_check pc e1_sec_level var_sec_level
        && equal_core_type var_core_type e1_core_type
      then
        type_expr e2
          ((var_name, (var_core_type, var_sec_level)) :: type_environment)
          pc
        >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
        Ok
          ( (e2_core_type, e2_sec_level)
          , Typed_ast.Let
              ( loc
              , var_name
              , (var_core_type, var_sec_level)
              , typed_e1
              , typed_e2
              , (e2_core_type, e2_sec_level) )
          , pc )
      else if equal_core_type var_core_type e1_core_type then
        Error
          (Error.of_string
             "Variable security level type does not match the assigned \
              security level type" )
      else
        Error
          (Error.of_string
             "Variable type does not match the assigned expression type" )
  | Parsed_ast.Assign (loc, var_name, e1) -> (
      lookup_var_type type_environment var_name
      |> function
      | None -> Error (Error.of_string "Variable does not exist")
      | Some (var_core_type, var_sec_level) ->
          type_expr e1 type_environment pc
          >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
          if
            subtyping_check pc e1_sec_level var_sec_level
            && equal_core_type var_core_type e1_core_type
          then
            Ok
              ( (var_core_type, var_sec_level)
              , Typed_ast.Assign
                  (loc, (var_core_type, var_sec_level), var_name, typed_e1)
              , pc )
          else
            Error
              (Error.of_string
                 "Variable type does not match the assigned expression type" )
      )
  | Parsed_ast.If (loc, e1, e2, e3) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
      type_expr e3 type_environment pc
      >>= fun ((e3_core_type, e3_sec_level), typed_e3, pc) ->
      if
        equal_core_type e1_core_type TEBool
        && equal_core_type e2_core_type e3_core_type
      then
        let new_pc = join pc e1_sec_level in
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
                    (max_security_level e2_sec_level e3_sec_level) ) )
          , new_pc )
      else
        Error
          (Error.of_string
             "Expression types in the if statement are not correct" )
  | Parsed_ast.Classify (loc, e1) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      if equal_security_level_type e1_sec_level TSHigh then
        Error
          (Error.of_string "Cannot classify a high security level expression")
      else
        Ok
          ( (e1_core_type, TSHigh)
          , Typed_ast.Classify (loc, typed_e1, (e1_core_type, TSHigh))
          , pc )
  | Parsed_ast.Declassify (loc, e1) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      if equal_security_level_type e1_sec_level TSLow then
        Error
          (Error.of_string
             "Cannot declassify a low security level expression" )
      else
        Ok
          ( (e1_core_type, TSLow)
          , Typed_ast.Declassify (loc, typed_e1, (e1_core_type, TSLow))
          , pc )
  | Parsed_ast.FunctionApp (loc, fn_name, args) -> (
      get_function_types type_environment fn_name
      |> function
      | Error _ ->
          Error
            (Error.of_string "Function does not exist in type environment")
      | Ok (TFunction (arg_types, return_type), fn_sec_level) ->
          (* Check function's arity matches the provided arguments *)
          let expected_arg_count = List.length arg_types in
          let provided_arg_count = List.length args in
          if expected_arg_count <> provided_arg_count then
            Error
              (Error.of_string
                 (Printf.sprintf
                    "Function %s expects %d arguments, but %d were provided"
                    fn_name expected_arg_count provided_arg_count ) )
          else
            let arg_types_env = List.zip_exn arg_types args in
            (* Type-check each argument *)
            let arg_types_env_result =
              List.map arg_types_env ~f:(fun (arg_type, arg_expr) ->
                  type_expr arg_expr type_environment pc
                  >>= fun ( (arg_expr_core_type, arg_expr_sec_level)
                          , typed_arg_expr
                          , _ )
                      ->
                  if
                    equal_core_type arg_type arg_expr_core_type
                    && subtyping_check pc arg_expr_sec_level fn_sec_level
                  then
                    Ok
                      ( (arg_expr_core_type, arg_expr_sec_level)
                      , typed_arg_expr )
                  else if equal_core_type arg_type arg_expr_core_type then
                    Error
                      (Error.of_string
                         "Function argument security level does not match \
                          the function security level" )
                  else
                    Error
                      (Error.of_string
                         "Function argument type does not match the \
                          function type" ) )
            in
            Result.all arg_types_env_result
            >>= fun typed_args ->
            let typed_args_exprs = List.map typed_args ~f:snd in
            Ok
              ( (return_type, fn_sec_level)
              , Typed_ast.FunctionApp
                  ( loc
                  , (return_type, fn_sec_level)
                  , fn_name
                  , typed_args_exprs )
              , pc )
      | Ok _ ->
          Error (Error.of_string "Function type is not a function type") )
  | Parsed_ast.While (loc, e1, e2) ->
      type_expr e1 type_environment pc
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
      if equal_core_type e1_core_type TEBool then
        let new_pc = join pc e1_sec_level in
        Ok
          ( (e2_core_type, max_security_level e1_sec_level e2_sec_level)
          , Typed_ast.While
              ( loc
              , typed_e1
              , typed_e2
              , (e2_core_type, max_security_level e1_sec_level e2_sec_level)
              )
          , new_pc )
      else
        Error
          (Error.of_string
             "Expression types in the while statement are not correct" )
  | Parsed_ast.Seq (loc, e1, e2) ->
      type_expr e1 type_environment pc
      >>= fun ((_, e1_sec_level), typed_e1, pc) ->
      type_expr e2 type_environment pc
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc) ->
      Ok
        ( (e2_core_type, max_security_level e1_sec_level e2_sec_level)
        , Typed_ast.Seq
            ( loc
            , typed_e1
            , typed_e2
            , (e2_core_type, max_security_level e1_sec_level e2_sec_level) )
        , pc )
  (* normal print is used to print low-security variables. It does not update
     pc *)
  | Parsed_ast.Print (loc, args) ->
      let rec type_args args acc pc =
        match args with
        | [] -> Ok (List.rev acc)
        | expr :: rest ->
            type_expr expr type_environment pc
            >>= fun ((_, security_level), typed_expr, pc) ->
            if equal_security_level_type security_level TSHigh then
              Error
                (Error.of_string
                   "Cannot print high security level data using print" )
            else type_args rest (typed_expr :: acc) pc
      in
      type_args args [] pc
      >>= fun typed_args ->
      Ok ((TEUnit, pc), Typed_ast.Print (loc, typed_args), pc)
  (* securePrint is used to print high-security variables. It updates pc to
     high *)
  | Parsed_ast.SecurePrint (loc, args) ->
      let rec type_args args acc pc =
        match args with
        | [] -> Ok (List.rev acc)
        | expr :: rest ->
            type_expr expr type_environment pc
            >>= fun ((_, _), typed_expr, pc) ->
            type_args rest (typed_expr :: acc) pc
      in
      type_args args [] pc
      >>= fun typed_args ->
      let new_pc = TSHigh in
      Ok ((TEUnit, new_pc), Typed_ast.SecurePrint (loc, typed_args), new_pc)
