(* open Core *)
open Parser_frontend
open Compiler_types.Language_types
open Equal_type_expr
open Type_environment
open Subtyping_security_levels_check
open Get_class

type row = (exception_type * security_level_type) list

let is_constant_row (try_row : row) (catch_row : row) : bool =
  let rec is_constant_row_helper (try_row : row) (catch_row : row) : bool =
    match (try_row, catch_row) with
    | [], [] -> true
    | ( (try_exception, try_sec_level) :: try_row_tl
      , (catch_exception, catch_sec_level) :: catch_row_tl ) ->
        if try_exception = catch_exception && try_sec_level = catch_sec_level
        then is_constant_row_helper try_row_tl catch_row_tl
        else false
    | _ -> false
  in
  is_constant_row_helper try_row catch_row

let rec remove_first_exception exception_name exception_security_level row =
  match row with
  | [] -> []
  | (exception_name', exception_security_level') :: tail ->
      if
        exception_name = exception_name'
        && exception_security_level = exception_security_level'
      then tail
      else
        (exception_name', exception_security_level')
        :: remove_first_exception exception_name exception_security_level
             tail

let check_var_not_shadowed type_environment var_name =
  let ast_var_type = lookup_var_type type_environment var_name in
  match ast_var_type with
  (* If this is none, it means the variable has not been declared yet *)
  | None -> Ok ()
  | Some _ ->
      Error
        (Core.Error.of_string
           "Variable has already been assigned and has another type" )

let max_security_level sec1 sec2 =
  match (sec1, sec2) with
  | TSLow, TSLow -> TSLow
  | TSHigh, TSHigh -> TSHigh
  | TSLow, TSHigh -> TSHigh
  | TSHigh, TSLow -> TSHigh

let exception_type_to_string exception_type =
  match exception_type with
  | DivisionByZero -> "DivisionByZero"
  | IntegerOverflow -> "IntegerOverflow"

let core_type_to_string core_type =
  match core_type with
  | TEInt -> "Int"
  | TEBool -> "Bool"
  | TEUnit -> "Unit"
  | TFunction _ -> "Function"
  | TEObject obj -> Printf.sprintf "Object %s" obj
  | TException e ->
      Printf.sprintf "Exception %s" (exception_type_to_string e)

let rec type_expr expr type_environment class_defns pc row =
  let ( >>= ) = Core.Result.( >>= ) in
  match expr with
  | Parsed_ast.Integer (loc, i, security_level) ->
      Ok
        ( (TEInt, security_level)
        , Typed_ast.Integer (loc, i, security_level)
        , pc
        , row )
  | Parsed_ast.Boolean (loc, b, security_level) ->
      Ok
        ( (TEBool, security_level)
        , Typed_ast.Boolean (loc, b, security_level)
        , pc
        , row )
  | Parsed_ast.BinOp (loc, bin_op, e1, e2) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      type_expr e2 type_environment class_defns pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
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
          , pc
          , row )
      else if equal_core_type e1_core_type e2_core_type then
        Error
          (Core.Error.of_string
             Printf.(
               sprintf
                 "Binary operation operands type error: operand is type %s \
                  instead of integer type"
                 (core_type_to_string e1_core_type) ) )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf "Binary operation operands type error: %s %s"
                (core_type_to_string e1_core_type)
                (core_type_to_string e2_core_type) ) )
  | Parsed_ast.CompOp (loc, comp_op, e1, e2) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      type_expr e2 type_environment class_defns pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
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
          , pc
          , row )
      else if equal_core_type e1_core_type e2_core_type then
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "Comparison operation operands type error: operand is type \
                 %s instead of integer type"
                (core_type_to_string e1_core_type) ) )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "Comparison operation operands type error: %s %s"
                (core_type_to_string e1_core_type)
                (core_type_to_string e2_core_type) ) )
  | Parsed_ast.BoolOp (loc, bool_comp_op, e1, e2) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      type_expr e2 type_environment class_defns pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
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
          , pc
          , row )
      else if equal_core_type e1_core_type e2_core_type then
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "Boolean operation operands type error: operand is type %s \
                 instead of boolean type"
                (core_type_to_string e1_core_type) ) )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf "Boolean operation operands type error: %s %s"
                (core_type_to_string e1_core_type)
                (core_type_to_string e2_core_type) ) )
  | Parsed_ast.UnaryOp (loc, unary_op, e1) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      if equal_core_type e1_core_type TEBool then
        Ok
          ( (TEBool, e1_sec_level)
          , Typed_ast.UnaryOp
              (loc, (TEBool, e1_sec_level), unary_op, typed_e1)
          , pc
          , row )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "Unary operation operand type error: operand is type %s \
                 instead of boolean type"
                (core_type_to_string e1_core_type) ) )
  | Parsed_ast.Identifier (loc, id) -> (
      lookup_var_type type_environment id
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not exist" id) )
      | Some var_type ->
          Ok (var_type, Typed_ast.Identifier (loc, var_type, id), pc, row) )
  | Parsed_ast.Let (loc, var_name, (var_core_type, var_sec_level), e1, e2) ->
      check_var_not_shadowed type_environment var_name
      >>= fun () ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      if
        subtyping_check pc e1_sec_level var_sec_level
        && equal_core_type var_core_type e1_core_type
      then
        type_expr e2
          ((var_name, (var_core_type, var_sec_level)) :: type_environment)
          class_defns pc row
        >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
        Ok
          ( (e2_core_type, e2_sec_level)
          , Typed_ast.Let
              ( loc
              , var_name
              , (var_core_type, var_sec_level)
              , typed_e1
              , typed_e2
              , (e2_core_type, e2_sec_level) )
          , pc
          , row )
      else if equal_core_type var_core_type e1_core_type then
        Error
          (Core.Error.of_string
             "The security level of the environment pc and the security \
              level of the expression do not match" )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "Variable core type (%s) does not match the assigned \
                 expression core type (%s)"
                (core_type_to_string var_core_type)
                (core_type_to_string e1_core_type) ) )
  | Parsed_ast.Assign (loc, var_name, e1) -> (
      lookup_var_type type_environment var_name
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not exist" var_name) )
      | Some (var_core_type, var_sec_level) ->
          type_expr e1 type_environment class_defns pc row
          >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
          if
            subtyping_check pc e1_sec_level var_sec_level
            && equal_core_type var_core_type e1_core_type
          then
            Ok
              ( (var_core_type, var_sec_level)
              , Typed_ast.Assign
                  (loc, (var_core_type, var_sec_level), var_name, typed_e1)
              , pc
              , row )
          else if equal_core_type var_core_type e1_core_type then
            Error
              (Core.Error.of_string
                 (Printf.sprintf
                    "Variable %s security level type does not match the \
                     assigned security level type"
                    var_name ) )
          else
            Error
              (Core.Error.of_string
                 (Printf.sprintf
                    "Variable %s core type (%s) does not match the assigned \
                     core type (%s)"
                    var_name
                    (core_type_to_string var_core_type)
                    (core_type_to_string e1_core_type) ) ) )
  | Parsed_ast.If (loc, e1, e2, e3) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, _, row) ->
      let new_pc = join pc e1_sec_level in
      type_expr e2 type_environment class_defns new_pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, _, row) ->
      type_expr e3 type_environment class_defns new_pc row
      >>= fun ((e3_core_type, e3_sec_level), typed_e3, _, row) ->
      if
        equal_core_type e1_core_type TEBool
        && equal_core_type e2_core_type e3_core_type
      then
        (* let new_pc = join pc e1_sec_level in *)
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
          , new_pc
          , row )
      else if equal_core_type e1_core_type TEBool then
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "The expressions in the if statement do not have the same \
                 type: %s %s"
                (core_type_to_string e2_core_type)
                (core_type_to_string e3_core_type) ) )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "The expression in the if statement is not a boolean\ne1: %s"
                (core_type_to_string e1_core_type) ) )
  | Parsed_ast.Classify (loc, e1) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      if equal_security_level_type e1_sec_level TSHigh then
        Error
          (Core.Error.of_string
             "Cannot classify a high security level expression" )
      else
        Ok
          ( (e1_core_type, TSHigh)
          , Typed_ast.Classify (loc, typed_e1, (e1_core_type, TSHigh))
          , pc
          , row )
  | Parsed_ast.Declassify (loc, e1) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      if equal_security_level_type e1_sec_level TSLow then
        Error
          (Core.Error.of_string
             "Cannot declassify a low security level expression" )
      else
        Ok
          ( (e1_core_type, TSLow)
          , Typed_ast.Declassify (loc, typed_e1, (e1_core_type, TSLow))
          , pc
          , row )
  | Parsed_ast.FunctionApp (loc, fn_name, args) -> (
      get_function_types type_environment fn_name
      |> function
      | Error _ ->
          Error
            (Core.Error.of_string
               Printf.(sprintf "Function %s does not exist" fn_name) )
      | Ok (TFunction (arg_types, return_type), fn_sec_level) ->
          (* Check function's arity matches the provided arguments *)
          let expected_arg_count = List.length arg_types in
          let provided_arg_count = List.length args in
          if expected_arg_count <> provided_arg_count then
            Error
              (Core.Error.of_string
                 (Printf.sprintf
                    "Function %s expects %d arguments, but %d were provided"
                    fn_name expected_arg_count provided_arg_count ) )
          else
            let arg_types_env = Core.List.zip_exn arg_types args in
            (* Type-check each argument *)
            let arg_types_env_result =
              Core.List.map arg_types_env ~f:(fun (arg_type, arg_expr) ->
                  type_expr arg_expr type_environment class_defns pc row
                  >>= fun ( (arg_expr_core_type, arg_expr_sec_level)
                          , typed_arg_expr
                          , _
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
                      (Core.Error.of_string
                         "Function argument security level does not match \
                          the function security level" )
                  else
                    Error
                      (Core.Error.of_string
                         (Printf.sprintf
                            "Function argument type (%s) does not match the \
                             function type (%s)"
                            (core_type_to_string arg_type)
                            (core_type_to_string arg_expr_core_type) ) ) )
            in
            Core.Result.all arg_types_env_result
            >>= fun typed_args ->
            let typed_args_exprs = Core.List.map typed_args ~f:snd in
            Ok
              ( (return_type, fn_sec_level)
              , Typed_ast.FunctionApp
                  ( loc
                  , (return_type, fn_sec_level)
                  , fn_name
                  , typed_args_exprs )
              , pc
              , row )
      | Ok _ ->
          Error (Core.Error.of_string "Function type is not a function type")
      )
  | Parsed_ast.While (loc, e1, e2) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((e1_core_type, e1_sec_level), typed_e1, pc, row) ->
      type_expr e2 type_environment class_defns pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
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
          , new_pc
          , row )
      else
        Error
          (Core.Error.of_string
             (Printf.sprintf
                "The expression in the while loop is %s instead of a boolean"
                (core_type_to_string e1_core_type) ) )
  | Parsed_ast.Seq (loc, e1, e2) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ((_, e1_sec_level), typed_e1, pc, row) ->
      type_expr e2 type_environment class_defns pc row
      >>= fun ((e2_core_type, e2_sec_level), typed_e2, pc, row) ->
      Ok
        ( (e2_core_type, max_security_level e1_sec_level e2_sec_level)
        , Typed_ast.Seq
            ( loc
            , typed_e1
            , typed_e2
            , (e2_core_type, max_security_level e1_sec_level e2_sec_level) )
        , pc
        , row )
  (* normal print is used to print low-security variables. It does not update
     pc *)
  | Parsed_ast.Print (loc, args) ->
      let rec type_args args acc pc =
        match args with
        | [] -> Ok (List.rev acc)
        | expr :: rest ->
            type_expr expr type_environment class_defns pc row
            >>= fun ((_, security_level), typed_expr, pc, _) ->
            if equal_security_level_type security_level TSHigh then
              Error
                (Core.Error.of_string
                   "Cannot print high security level data using normal print" )
            else type_args rest (typed_expr :: acc) pc
      in
      type_args args [] pc
      >>= fun typed_args ->
      Ok ((TEUnit, pc), Typed_ast.Print (loc, typed_args), pc, row)
  (* securePrint is used to print high-security variables. It updates pc to
     high *)
  | Parsed_ast.SecurePrint (loc, args) ->
      let rec type_args args acc pc =
        match args with
        | [] -> Ok (List.rev acc)
        | expr :: rest ->
            type_expr expr type_environment class_defns pc row
            >>= fun ((_, _), typed_expr, pc, _) ->
            type_args rest (typed_expr :: acc) pc
      in
      type_args args [] pc
      >>= fun typed_args ->
      let new_pc = TSHigh in
      Ok
        ( (TEUnit, new_pc)
        , Typed_ast.SecurePrint (loc, typed_args)
        , new_pc
        , row )
  | Parsed_ast.Skip loc -> Ok ((TEUnit, pc), Typed_ast.Skip loc, pc, row)
  (* only need to typecheck arguments and constructor *)
  | Parsed_ast.Object (loc, security_level_type, class_name, args) -> (
      get_class class_name class_defns
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Class %s does not exist" class_name) )
      | Some (Parsed_ast.ClassDefn (class_name, _, constructor, _)) -> (
        match constructor with
        | Parsed_ast.Constructor (constructor_args, _) ->
            let constructor_arg_count = List.length constructor_args in
            let provided_arg_count = List.length args in
            if constructor_arg_count <> provided_arg_count then
              Error
                (Core.Error.of_string
                   (Printf.sprintf
                      "Class %s constructor expects %d arguments, but %d \
                       were provided"
                      class_name constructor_arg_count provided_arg_count ) )
            else
              let arg_types_env = Core.List.zip_exn constructor_args args in
              (* type-check each argument *)
              let arg_types_env_result =
                Core.List.map arg_types_env
                  ~f:(fun
                      ( TArg
                          ( _
                          , ( constructor_arg_core_type
                            , constructor_arg_sec_level ) )
                      , arg_expr )
                    ->
                    type_expr arg_expr type_environment class_defns pc row
                    >>= fun ( (arg_expr_core_type, arg_expr_sec_level)
                            , typed_arg_expr
                            , _
                            , _ )
                        ->
                    if
                      equal_type_expr
                        (constructor_arg_core_type, constructor_arg_sec_level)
                        (arg_expr_core_type, arg_expr_sec_level)
                    then
                      Ok
                        ( (arg_expr_core_type, arg_expr_sec_level)
                        , typed_arg_expr )
                    else if
                      equal_core_type constructor_arg_core_type
                        arg_expr_core_type
                    then
                      Error
                        (Core.Error.of_string
                           "Object argument security level does not match \
                            the constructor security level" )
                    else
                      Error
                        (Core.Error.of_string
                           (Printf.sprintf
                              "Object argument type (%s) does not match the \
                               constructor type (%s)"
                              (core_type_to_string constructor_arg_core_type)
                              (core_type_to_string arg_expr_core_type) ) ) )
              in
              Core.Result.all arg_types_env_result
              >>= fun typed_args ->
              let typed_args_exprs = Core.List.map typed_args ~f:snd in
              Ok
                ( (TEObject class_name, security_level_type)
                , Typed_ast.Object
                    ( loc
                    , security_level_type
                    , class_name
                    , typed_args_exprs
                    , (TEObject class_name, security_level_type) )
                , pc
                , row ) ) )
  | Parsed_ast.MethodCall (loc, obj_name, method_name, args) -> (
      (* type-check the object to get its type and security level *)
      lookup_var_type type_environment obj_name
      |> (function
      | None -> Error (Core.Error.of_string "Object does not exist")
      | Some (obj_core_type, obj_sec_level) ->
          Ok (obj_core_type, obj_sec_level) )
      >>= fun (obj_core_type, obj_sec_level) ->
      match obj_core_type with
      | TEObject class_name -> (
          get_class class_name class_defns
          |> function
          | None ->
              Error
                (Core.Error.of_string
                   (Printf.sprintf "Class %s does not exist" class_name) )
          | Some (Parsed_ast.ClassDefn (_, _, _, methods)) -> (
            match get_method method_name methods with
            | None ->
                Error
                  (Core.Error.of_string
                     (Printf.sprintf "Method %s does not exist" method_name) )
            | Some
                (Parsed_ast.MethodDefn
                   ( method_sec_level
                   , Parsed_ast.FunctionDefn
                       (method_name, fn_args, return_type, _) ) ) ->
                (* TODO: have not updated pc - check where I need to do so *)
                if less_than_or_equal method_sec_level obj_sec_level then
                  let expected_arg_count = List.length fn_args in
                  let provided_arg_count = List.length args in
                  if expected_arg_count <> provided_arg_count then
                    Error
                      (Core.Error.of_string
                         (Printf.sprintf
                            "Method %s\n\
                             expects %d arguments, but %d were\n\
                             provided"
                            method_name expected_arg_count provided_arg_count ) )
                  else
                    (* type-check each argument *)
                    let arg_types_env =
                      Core.List.zip_exn
                        (Core.List.map fn_args
                           ~f:(fun (TArg (_, arg_type)) -> arg_type ) )
                        args
                    in
                    let arg_types_env_result =
                      Core.List.map arg_types_env
                        ~f:(fun ((arg_core_type, _), arg_expr) ->
                          type_expr arg_expr type_environment class_defns pc
                            row
                          >>= fun ( (arg_expr_core_type, arg_expr_sec_level)
                                  , typed_arg_expr
                                  , _
                                  , _ )
                              ->
                          if equal_core_type arg_core_type arg_expr_core_type
                          then
                            Ok
                              ( (arg_expr_core_type, arg_expr_sec_level)
                              , typed_arg_expr )
                          else
                            Error
                              (Core.Error.of_string
                                 (Printf.sprintf
                                    "Method argument type (%s) does not \
                                     match the method type (%s)"
                                    (core_type_to_string arg_core_type)
                                    (core_type_to_string arg_expr_core_type) ) ) )
                    in
                    Core.Result.all arg_types_env_result
                    >>= fun typed_args ->
                    let typed_args_exprs = Core.List.map typed_args ~f:snd in
                    Ok
                      ( return_type
                      , Typed_ast.MethodCall
                          ( loc
                          , (TEUnit, method_sec_level)
                          , obj_name
                          , method_name
                          , typed_args_exprs )
                      , pc
                      , row )
                else
                  Error
                    (Core.Error.of_string
                       (Printf.sprintf
                          "Security level of object is not high enough to \
                           call the method" ) ) ) )
      | _ -> Error (Core.Error.of_string "Method call on non-object type") )
  | Parsed_ast.Raise (loc, exception_name, var_name) ->
      lookup_var_type type_environment var_name
      |> (function
      | None -> Error (Core.Error.of_string "Variable does not exist")
      | Some (var_core_type, var_sec_level) ->
          Ok (var_core_type, var_sec_level) )
      >>= fun (_, var_sec_level) ->
      let updated_pc = join pc var_sec_level in
      let updated_row = (exception_name, var_sec_level) :: row in
      Ok
        ( (TException exception_name, var_sec_level)
        , Typed_ast.Raise
            ( loc
            , exception_name
            , var_name
            , (TException exception_name, var_sec_level) )
        , updated_pc
        , updated_row )
  | Parsed_ast.TryCatchFinally (loc, e1, exception_name, var_name, e2, e3) ->
      type_expr e1 type_environment class_defns pc row
      >>= fun ( (e1_core_type, e1_sec_level)
              , typed_e1
              , pc
              , row_after_try_block )
          ->
      let pc_after_try_block = join pc e1_sec_level in
      type_expr e2
        ( (var_name, (TException exception_name, pc_after_try_block))
        :: type_environment )
        class_defns pc_after_try_block row_after_try_block
      >>= fun ( (e2_core_type, e2_sec_level)
              , typed_e2
              , pc_after_catch_block
              , row_after_catch_block )
          ->
      if not (is_constant_row row_after_catch_block row_after_try_block) then
        Error
          (Core.Error.of_string
             "The catch block has raised new exceptions that were not \
              raised in the try block. This is not permitted." )
      else
        (* TODO: should I return an Error if var_name cannot be found in
           type_environment? *)
        let var_sec_level =
          match lookup_var_type type_environment var_name with
          | Some (_, sec_level) -> sec_level
          | None -> TSLow
        in
        let updated_row_after_catch =
          remove_first_exception exception_name var_sec_level
            row_after_catch_block
        in
        type_expr e3 type_environment class_defns pc_after_catch_block
          updated_row_after_catch
        >>= fun ( (e3_core_type, e3_sec_level)
                , typed_e3
                , pc_after_finally_block
                , row_after_finally_block )
            ->
        if subtyping_check pc e1_sec_level e3_sec_level then
          (* return type is the finally block *)
          Ok
            ( (e3_core_type, e3_sec_level)
            , Typed_ast.TryCatchFinally
                ( loc
                , typed_e1
                , exception_name
                , var_name
                , typed_e2
                , typed_e3
                , (e2_core_type, max_security_level e1_sec_level e2_sec_level)
                )
            , pc_after_finally_block
            , row_after_finally_block )
        else
          Error
            (Core.Error.of_string
               (Printf.sprintf
                  "Try block type (%s) does not match the catch block type \
                   (%s)"
                  (core_type_to_string e1_core_type)
                  (core_type_to_string e2_core_type) ) )
