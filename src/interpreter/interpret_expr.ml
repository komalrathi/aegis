(* open Core *)
open Interpret_ops
open Typing.Typed_ast
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment

type interpret_expr_result =
  | IValue of interpreter_val * value_environment
  | IException of exception_type * interpreter_val

let get_class class_name class_defns =
  Core.List.find
    ~f:(fun (ClassDefn (c_name, _, _, _)) -> String.equal c_name class_name)
    class_defns

let value_to_string = function
  | VInt i -> Printf.sprintf "%d" i
  | VBool b -> Printf.sprintf "%b" b
  | VUnit _ -> "unit"
  | VObject (obj_name, _) -> obj_name

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
          | IException (exception_type, var_name) ->
              Ok (IException (exception_type, var_name)) )
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
  | CompOp (_, _, comp_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          interpret_expr e2 val_env_1 function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              interpret_comp_op comp_op val1 val2
              >>= fun output_val -> Ok (IValue (output_val, val_env_2))
          | IException (exception_type, var_name) ->
              Ok (IException (exception_type, var_name)) )
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
  | BoolOp (_, _, bool_comp_op, e1, e2) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) -> (
          interpret_expr e2 val_env_1 function_environment class_defns
          >>= function
          | IValue (val2, val_env_2) ->
              interpret_bool_comp_op bool_comp_op val1 val2
              >>= fun output_val -> Ok (IValue (output_val, val_env_2))
          | IException (exception_type, var_name) ->
              Ok (IException (exception_type, var_name)) )
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
  | UnaryOp (_, _, unary_op, e1) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) ->
          interpret_unary_op unary_op val1
          >>= fun output_val -> Ok (IValue (output_val, val_env_1))
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
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
          | IException (exception_type, var_name) ->
              Ok (IException (exception_type, var_name)) )
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
  | Assign (_, _, var_name, e1) -> (
      interpret_expr e1 value_environment function_environment class_defns
      >>= function
      | IValue (val1, val_env_1) ->
          Ok (IValue (val1, update_var_in_env var_name val1 val_env_1))
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
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
        )
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
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
              | IException (_, _) -> Ok ([], value_environment) )
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
      | IException (exception_type, var_name) ->
          Ok (IException (exception_type, var_name)) )
  (* | Print (_, args) -> let rec print_args val_env args = match args with |
     [] -> Ok (VUnit (), val_env) | arg :: rest -> ( interpret_expr arg
     val_env function_environment class_defns >>= function | IValue (val1,
     val_env_1) -> print_endline (value_to_string val1) ; print_args
     val_env_1 rest | IException (exception_type, var_name) -> Ok (IException
     (exception_type, var_name)) ) in print_args value_environment args *)
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
            | IException (exception_type, var_name) ->
                Ok (IException (exception_type, var_name)) )
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
            | IException (exception_type, var_name) ->
                Ok (IException (exception_type, var_name)) )
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
              | IException (_, _) -> Ok ([], value_environment) )
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
        | IException (exception_type, var_name) ->
            Ok (IException (exception_type, var_name)) ) )
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
              ~f:(fun (MethodDefn (_, FunctionDefn (name, _, _, _))) ->
                name = method_name )
              methods
          with
          | None ->
              Error
                (Core.Error.of_string
                   (Printf.sprintf "Method %s not found" method_name) )
          | Some (MethodDefn (_, FunctionDefn (_, params, _, body))) ->
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
                    | IException (_, _) -> Ok ([], value_environment) )
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
    match exception_type with
    | DivisionByZero -> (
        let value = lookup_var_value value_environment var_name in
        match value with
        | None ->
            Error
              (Core.Error.of_string
                 (Printf.sprintf "Variable %s does not have a value" var_name) )
        | Some value -> Ok (IException (exception_type, value)) )
    | IntegerOverflow -> (
        let value = lookup_var_value value_environment var_name in
        match value with
        | None ->
            Error
              (Core.Error.of_string
                 (Printf.sprintf "Variable %s does not have a value" var_name) )
        | Some value -> Ok (IException (exception_type, value)) ) )
  | TryCatchFinally
      (_, try_expr, exn_name, exception_variable, catch_expr, finally_expr, _)
    -> (
    match
      interpret_expr try_expr value_environment function_environment
        class_defns
    with
    (* try block executed successfully. now we evaluate finally block *)
    | Ok (IValue (_, env_after_try)) -> (
      match
        interpret_expr finally_expr env_after_try function_environment
          class_defns
      with
      | Ok (IValue (v_after_final, env_after_final)) ->
          Ok (IValue (v_after_final, env_after_final))
      | Ok (IException (_, _)) ->
          Error
            (Core.Error.of_string
               "An exception was raised in the finally block. Finally block \
                must evaluate to a value." )
      | Error _ -> Error (Core.Error.of_string "Error in try block") )
    (* Exception raised in try block - now we check the catch block *)
    | Ok (IException (exn_raised, var_value)) -> (
        if
          (* check if exception raised is the same as the exception name for
             catch block *)
          exn_name = exn_raised
        then
          (* evaluate catch block *)
          let new_value_environment =
            (exception_variable, var_value) :: value_environment
          in
          match
            interpret_expr catch_expr new_value_environment
              function_environment class_defns
          with
          | Error _ -> Error (Core.Error.of_string "Error in catch block")
          (* Catch block expression evaluated successfully - now evaluate
             finally block *)
          | Ok (IValue (v_catch, env_catch)) -> (
            match
              interpret_expr finally_expr env_catch function_environment
                class_defns
            with
            | Ok (IValue (_, env_after_final)) ->
                Ok (IValue (v_catch, env_after_final))
            | Ok (IException (_, _)) ->
                Error
                  (Core.Error.of_string
                     "An exception was raised in the finally block. Finally \
                      block must evaluate to a value." )
            | Error _ ->
                Error (Core.Error.of_string "Error in finally block") )
          (* exception raised in catch block *)
          | Ok (IException (_, _)) -> (
            (* evaluate finally block *)
            match
              interpret_expr finally_expr value_environment
                function_environment class_defns
            with
            | Ok (IValue (val_after_final, env_after_final)) ->
                Ok (IValue (val_after_final, env_after_final))
            | Ok (IException (_, _)) ->
                Error
                  (Core.Error.of_string
                     "An exception was raised in the finally block. Finally \
                      block must evaluate to a value." )
            | Error _ ->
                Error (Core.Error.of_string "Error in finally block") )
        else
          (* exception raised does not match exc_name. evaluate finally
             block *)
          match
            interpret_expr finally_expr value_environment
              function_environment class_defns
          with
          | Ok (IValue (_, env_after_final)) ->
              Ok (IValue (VUnit (), env_after_final))
          | Ok (IException (_, _)) ->
              Error
                (Core.Error.of_string
                   "An exception was raised in the finally block. Finally \
                    block must evaluate to a value." )
          | Error _ -> Error (Core.Error.of_string "Error in finally block")
        )
    | Error _ -> Error (Core.Error.of_string "Error in try block") )
