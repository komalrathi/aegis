open Core
open Parser_frontend
open Compiler_types.Language_types
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

let rec type_expr expr type_environment =
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
        phys_equal e1_core_type e2_core_type && phys_equal e1_core_type TEInt
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
        phys_equal e1_core_type e2_core_type && phys_equal e1_core_type TEInt
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
        phys_equal e1_core_type e2_core_type
        && phys_equal e1_core_type TEBool
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
          check_expr_var_types_match var_type e1_type
          >>= fun () ->
          Ok (var_type, Typed_ast.Assign (loc, var_type, var_name, typed_e1))
      )
  | Parsed_ast.If (loc, e1, e2, e3) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      type_expr e2 type_environment
      >>= fun ((e2_core_type, e2_sec_level), typed_e2) ->
      type_expr e3 type_environment
      >>= fun ((e3_core_type, e3_sec_level), typed_e3) ->
      if
        phys_equal e1_core_type TEBool
        && phys_equal e2_core_type e3_core_type
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
      if phys_equal e1_sec_level TSHigh then
        Error
          (Error.of_string "Cannot classify a high security level expression")
      else
        Ok
          ( (e1_core_type, TSHigh)
          , Typed_ast.Classify (loc, typed_e1, (e1_core_type, TSHigh)) )
  | Parsed_ast.Declassify (loc, e1) ->
      type_expr e1 type_environment
      >>= fun ((e1_core_type, e1_sec_level), typed_e1) ->
      if phys_equal e1_sec_level TSLow then
        Error
          (Error.of_string
             "Cannot declassify a low security level expression" )
      else
        Ok
          ( (e1_core_type, TSLow)
          , Typed_ast.Declassify (loc, typed_e1, (e1_core_type, TSLow)) )

let type_program (Parsed_ast.Prog (_, expr)) =
  let open Result in
  type_expr expr [] >>= fun (_, typed_expr) -> Ok (Typed_ast.Prog typed_expr)
