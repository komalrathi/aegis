open Interpret_ops
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment
open Effect
open Effect.Deep

(* type _ t += | Raise : exception_type * interpreter_val *
   security_level_type -> 'a t *)
type _ t +=
  | Raise :
      exception_type * interpreter_val * security_level_type
      -> interpret_expr_result t

let get_class class_name class_defns =
  Core.List.find
    ~f:(fun (ClassDefn (c_name, _, _, _)) -> String.equal c_name class_name)
    class_defns

let value_to_string = function
  | VInt i -> Printf.sprintf "%d" i
  | VBool b -> Printf.sprintf "%b" b
  | VUnit _ -> "unit"
  | VObject (obj_name, _) -> obj_name
  | VContinuation _ -> "continuation"

let rec remove_var_from_env var_name env =
  match env with
  | [] -> []
  | (name, value) :: t ->
      if var_name = name then t
      else (name, value) :: remove_var_from_env var_name t

let rec update_var_in_env var_name var_value env =
  match env with
  | [] -> [(var_name, var_value)]
  | (name, value) :: t ->
      if var_name = name then (var_name, var_value) :: t
      else (name, value) :: update_var_in_env var_name var_value t

(* return value, value_environment *)
let rec interpret_expr expr value_environment function_environment
    class_defns =
  let ( >>= ) = Core.Result.( >>= ) in
  match expr with
  | Integer (_, i, _) -> Ok (IValue (VInt i, value_environment))
  | Boolean (_, b, _) -> Ok (IValue (VBool b, value_environment))
  | BinOp (_, _, bin_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          interpret_expr e2 val_env_1 function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              interpret_bin_op bin_op val1 val2
              >>= fun output_val -> Ok (IValue (output_val, val_env_2))
          | IException (exception_type, var_name, is_resumable) ->
              Ok (IException (exception_type, var_name, is_resumable)) )
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | CompOp (_, _, comp_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          interpret_expr e2 val_env_1 function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              interpret_comp_op comp_op val1 val2
              >>= fun output_val -> Ok (IValue (output_val, val_env_2))
          | IException (exception_type, var_name, is_resumable) ->
              Ok (IException (exception_type, var_name, is_resumable)) )
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | BoolOp (_, _, bool_comp_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          interpret_expr e2 val_env_1 function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              interpret_bool_comp_op bool_comp_op val1 val2
              >>= fun output_val -> Ok (IValue (output_val, val_env_2))
          | IException (exception_type, var_name, is_resumable) ->
              Ok (IException (exception_type, var_name, is_resumable)) )
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | UnaryOp (_, _, unary_op, e1) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) ->
          interpret_unary_op unary_op val1
          >>= fun output_val -> Ok (IValue (output_val, val_env_1))
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | Identifier (_, var_type, identifier) -> (
      lookup_var_value value_environment identifier
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not have a value" identifier) )
      | Some var_value -> (
        match (var_type, var_value) with
        | (TEInt, _), VInt i -> Ok (IValue (VInt i, value_environment))
        | (TEBool, _), VBool b -> Ok (IValue (VBool b, value_environment))
        | (TEObject _, _), VObject (obj_name, _) ->
            Ok (IValue (VObject (obj_name, []), value_environment))
        | (TException (DivisionByZero, _), _), _ ->
            (* TODO: check what value is_resumable should have here *)
            Ok (IException (DivisionByZero, var_value, false))
        | (TException (IntegerOverflow, _), _), _ ->
            Ok (IException (IntegerOverflow, var_value, false))
        | _ ->
            Error
              (Core.Error.of_string
                 "Type error: variable type does not match value type" ) ) )
  | Let (_, var_name, _, e1, e2, _) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          let new_env = (var_name, val1) :: val_env_1 in
          interpret_expr e2 new_env function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              Ok (IValue (val2, remove_var_from_env var_name val_env_2))
          | IException (exception_type, var_name, is_resumable) ->
              Ok (IException (exception_type, var_name, is_resumable)) )
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | Assign (_, _, var_name, e1) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) ->
          Ok (IValue (val1, update_var_in_env var_name val1 val_env_1))
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | If (_, e1, e2, e3, _) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
        match val1 with
        | VBool true ->
            interpret_expr e2 val_env_1 function_environment class_defns
        | VBool false ->
            interpret_expr e3 val_env_1 function_environment class_defns
        | VInt _ ->
            Error
              (Core.Error.of_string
                 "Type error: Cannot have int type in the if condition" )
        | VUnit _ ->
            Error
              (Core.Error.of_string
                 "Type error: Cannot have unit type in the if condition" )
        | VObject _ ->
            Error
              (Core.Error.of_string
                 "Type error: Cannot have object type in the if condition" )
        | VContinuation _ ->
            Error
              (Core.Error.of_string
                 "Type error: Cannot have continuation type in the if \
                  condition" ) )
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | Classify (_, e1, _) ->
      interpret_expr e1 value_environment function_environment class_defns
  | Declassify (_, e1, _) ->
      interpret_expr e1 value_environment function_environment class_defns
  | FunctionApp (_, _, f_name, args) -> (
    match Stdlib.List.assoc_opt f_name function_environment with
    | Some (param_names, body) ->
        let rec eval_args val_env args acc =
          match args with
          | [] -> Ok (List.rev acc, val_env)
          | arg :: rest -> (
              interpret_expr arg val_env function_environment class_defns
              >>= function
              | IValue (val1, val_env_1) ->
                  eval_args val_env_1 rest (val1 :: acc)
              (* exception should hopefully be handled better elsewhere *)
              | IException (_, _, _) -> Ok ([], value_environment) )
        in
        eval_args value_environment args []
        >>= fun (arg_values, args_updated_value_env) ->
        if List.length param_names <> List.length arg_values then
          Error
            (Core.Error.of_string
               (Printf.sprintf
                  "Arity mismatch in function '%s': expected %d arguments \
                   but got %d"
                  f_name (List.length param_names) (List.length arg_values) ) )
        else
          let new_env =
            match Core.List.zip param_names arg_values with
            | Ok pairs -> pairs @ args_updated_value_env
            | Unequal_lengths ->
                failwith
                  "Arity mismatch: number of arguments provided does not \
                   match the function's parameter count"
          in
          interpret_expr body new_env function_environment class_defns
    | None -> Error (Core.Error.of_string "Function not found") )
  | While (loc, e1, e2, type_expr) ->
      let e =
        If
          ( loc
          , e1
          , Seq (loc, e2, While (loc, e1, e2, type_expr), type_expr)
          , Skip loc
          , type_expr )
      in
      interpret_expr e value_environment function_environment class_defns
  | Seq (_, e1, e2, _) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (_, val_env_1) ->
          interpret_expr e2 val_env_1 function_environment class_defns
      | IException (exception_type, var_name, is_resumable) ->
          Ok (IException (exception_type, var_name, is_resumable)) )
  | Print (_, args) ->
      let rec print_args val_env args =
        match args with
        | [] -> Ok (IValue (VUnit (), val_env))
        | arg :: rest -> (
            interpret_expr arg val_env function_environment class_defns
            >>= function
            | IValue (val1, val_env_1) ->
                print_endline (value_to_string val1) ;
                print_args val_env_1 rest
            | IException (exception_type, var_name, is_resumable) ->
                Ok (IException (exception_type, var_name, is_resumable)) )
      in
      print_args value_environment args
  | SecurePrint (_, args) ->
      let rec print_args val_env args =
        match args with
        | [] -> Ok (IValue (VUnit (), val_env))
        | arg :: rest -> (
            interpret_expr arg val_env function_environment class_defns
            >>= function
            | IValue (val1, val_env_1) ->
                print_endline (value_to_string val1) ;
                print_args val_env_1 rest
            | IException (exception_type, var_name, is_resumable) ->
                Ok (IException (exception_type, var_name, is_resumable)) )
      in
      print_args value_environment args
  | Skip _ -> Ok (IValue (VUnit (), value_environment))
  | Object (_, _, class_name, args, _) -> (
    match get_class class_name class_defns with
    | None ->
        Error
          (Core.Error.of_string
             (Printf.sprintf "Class %s not found" class_name) )
    | Some (ClassDefn (_, _, Constructor (params, constructor_expr), _)) -> (
        let rec eval_args val_env args acc =
          match args with
          | [] -> Ok (List.rev acc, val_env)
          | arg :: rest -> (
              interpret_expr arg val_env function_environment class_defns
              >>= function
              | IValue (val1, val_env_1) ->
                  eval_args val_env_1 rest (val1 :: acc)
              (* exception should hopefully be handled better elsewhere *)
              | IException (_, _, _) -> Ok ([], value_environment) )
        in
        eval_args value_environment args []
        >>= fun (evaluated_args, new_val_env) ->
        let rec bind_args params args acc =
          match (params, args) with
          | [], [] -> Ok acc
          | TArg (name, _) :: param_rest, arg_val :: arg_rest ->
              bind_args param_rest arg_rest ((name, arg_val) :: acc)
          | _ ->
              Error
                (Core.Error.of_string
                   "Mismatched argument count in object instantiation" )
        in
        bind_args params evaluated_args []
        >>= fun arg_bindings ->
        let extended_env = arg_bindings @ new_val_env in
        interpret_expr constructor_expr extended_env function_environment
          class_defns
        >>= function
        | IValue (_, final_val_env) ->
            Ok (IValue (VObject (class_name, evaluated_args), final_val_env))
        | IException (exception_type, var_name, is_resumable) ->
            Ok (IException (exception_type, var_name, is_resumable)) ) )
  | MethodCall (_, _, obj_name, method_name, arg_exprs) -> (
      lookup_var_value value_environment obj_name
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Object %s not found" obj_name) )
      | Some (VObject (class_name, _)) -> (
        match get_class class_name class_defns with
        | None ->
            Error
              (Core.Error.of_string
                 (Printf.sprintf "Class %s not found" class_name) )
        | Some (ClassDefn (_, _, _, methods)) -> (
          match
            Core.List.find
              ~f:(fun (FunctionDefn (name, _, _, _)) -> name = method_name)
              methods
          with
          | None ->
              Error
                (Core.Error.of_string
                   (Printf.sprintf "Method %s not found" method_name) )
          | Some (FunctionDefn (_, params, _, body)) ->
              (* Evaluate method arguments *)
              let rec eval_args val_env args acc =
                match args with
                | [] -> Ok (List.rev acc, val_env)
                | arg :: rest -> (
                    interpret_expr arg val_env function_environment
                      class_defns
                    >>= function
                    | IValue (val1, val_env_1) ->
                        eval_args val_env_1 rest (val1 :: acc)
                    (* exception should hopefully be handled better
                       elsewhere *)
                    | IException (_, _, _) -> Ok ([], value_environment) )
              in
              eval_args value_environment arg_exprs []
              >>= fun (evaluated_args, new_val_env) ->
              (* Bind arguments to parameter names *)
              let rec bind_args params args acc =
                match (params, args) with
                | [], [] -> Ok acc
                | TArg (name, _) :: param_rest, arg_val :: arg_rest ->
                    bind_args param_rest arg_rest ((name, arg_val) :: acc)
                | _ ->
                    Error
                      (Core.Error.of_string
                         "Mismatched argument count in method call" )
              in
              bind_args params evaluated_args []
              >>= fun arg_bindings ->
              let extended_env = arg_bindings @ new_val_env in
              interpret_expr body extended_env function_environment
                class_defns ) )
      | Some _ ->
          Error
            (Core.Error.of_string
               "Type error: variable type does not match value type" ) )
  | Raise (_, exception_type, var_name, _) -> (
      let value = lookup_var_value value_environment var_name in
      match value with
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not have a value" var_name) )
      | Some value -> Ok (IException (exception_type, value, false)) )
  | ResumableRaise (_, exception_type, var_name, (_, security_level)) -> (
      let value = lookup_var_value value_environment var_name in
      match value with
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not have a value" var_name) )
      | Some value ->
          Ok (perform (Raise (exception_type, value, security_level))) )
  | Continue (_, k, resume_expr, _) -> (
    match lookup_var_value value_environment k with
    | None ->
        Error
          (Core.Error.of_string
             (Printf.sprintf "Continuation %s not found" k) )
    | Some v -> (
      match v with
      | VContinuation cont -> (
        (* Evaluate the resume expression to get the resume value. *)
        match
          interpret_expr resume_expr value_environment function_environment
            class_defns
        with
        | Ok result -> Ok (continue cont result)
        | Error err -> Error err )
      | _ ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Identifier %s is not a continuation" k) ) ) )
  | TryCatchFinally
      ( _
      , try_expr
      , exn_type
      , exn_var
      , catch_cont_opt
      , catch_expr
      , finally_expr
      , _result_type ) ->
      let run_finally result =
        match
          interpret_expr finally_expr value_environment function_environment
            class_defns
        with
        | Ok (IValue (_, env_final)) -> (
          match result with
          | IValue (v, _) -> IValue (v, env_final)
          | IException (exn, payload, is_resumable) ->
              IException (exn, payload, is_resumable) )
        | Ok _ -> failwith "Finally block must evaluate to a value"
        | Error err -> failwith (Core.Error.to_string_hum err)
      in
      let result =
        match_with
          (fun () ->
            match
              interpret_expr try_expr value_environment function_environment
                class_defns
            with
            | Ok res -> res
            | Error err -> failwith (Core.Error.to_string_hum err) )
          ()
          { retc= run_finally
          ; exnc= (fun e -> raise e)
          ; effc=
              (fun (type a) (eff : a Effect.t) ->
                match eff with
                | Raise (caught_exn, payload, _sec)
                  when caught_exn = exn_type ->
                    Some
                      (fun (k : (a, interpret_expr_result) continuation) ->
                        let env_catch =
                          (exn_var, payload) :: value_environment
                        in
                        match catch_cont_opt with
                        | Some cont_id -> (
                            let env_catch_with_cont =
                              (cont_id, VContinuation (Obj.magic k))
                              :: env_catch
                            in
                            match
                              interpret_expr catch_expr env_catch_with_cont
                                function_environment class_defns
                            with
                            | Ok res -> run_finally res
                            | Error err ->
                                failwith (Core.Error.to_string_hum err) )
                        | None -> (
                          match
                            interpret_expr catch_expr env_catch
                              function_environment class_defns
                          with
                          | Ok res -> run_finally res
                          | Error err ->
                              failwith (Core.Error.to_string_hum err) ) )
                | Raise (caught_exn, payload, _sec) ->
                    Some
                      (fun (_ : (a, interpret_expr_result) continuation) ->
                        IException (caught_exn, payload, false) )
                | _ -> None ) }
      in
      Ok result
