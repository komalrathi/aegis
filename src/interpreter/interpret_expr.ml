(* open Core *)
open Typing.Typed_ast
open Interpret_class_defn
open Compiler_types.Ast_types
open Compiler_types.Language_types
open Value_environment

let value_to_string = function
  | VInt i -> Printf.sprintf "%d" i
  | VBool b -> Printf.sprintf "%b" b
  | VUnit _ -> "unit"
  | VObject (obj_name, _) -> obj_name

let apply_int_bin_op bin_op i1 i2 =
  match bin_op with
  | BinOpPlus -> Ok (VInt (i1 + i2))
  | BinOpMinus -> Ok (VInt (i1 - i2))
  | BinOpMultiply -> Ok (VInt (i1 * i2))
  | BinOpDivide ->
      if i2 = 0 then Error (Core.Error.of_string "Division by zero")
      else Ok (VInt (i1 / i2))

let apply_comp_op comp_op i1 i2 =
  match comp_op with
  | CompOpLessThan -> Ok (VBool (i1 < i2))
  | CompOpGreaterThan -> Ok (VBool (i1 > i2))
  | CompOpLessThanEqual -> Ok (VBool (i1 <= i2))
  | CompOpGreaterThanEqual -> Ok (VBool (i1 >= i2))
  | CompOpEqual -> Ok (VBool (i1 = i2))

let interpret_bin_op bin_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_int_bin_op bin_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to boolean values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to unit values" )
  | VObject _, _ | _, VObject _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to object values" )

let interpret_comp_op comp_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_comp_op comp_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to boolean values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to unit values" )
  | VObject _, _ | _, VObject _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to object values" )

let interpret_bool_comp_op bool_comp_op b1 b2 =
  match (b1, b2) with
  | VBool b1, VBool b2 -> (
    match bool_comp_op with
    | BoolOpAnd -> Ok (VBool (b1 && b2))
    | BoolOpOr -> Ok (VBool (b1 || b2)) )
  | VInt _, VBool _ | VBool _, VInt _ | VInt _, VInt _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to integer values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to unit values" )
  | _, _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to object values" )

let interpret_unary_op unary_op b =
  match b with
  | VBool b -> ( match unary_op with UnaryOpNot -> Ok (VBool (not b)) )
  | _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply unary operation to integer values" )

let rec remove_var_from_env var_name env =
  match env with
  | [] -> []
  | (name, value) :: t ->
      if var_name = name then t
      else (name, value) :: remove_var_from_env var_name t

let rec update_var_in_env var_name var_value env =
  match env with
  | [] -> []
  | (name, value) :: t ->
      if var_name = name then (var_name, var_value) :: t
      else (name, value) :: update_var_in_env var_name var_value t

(* return value, value_environment *)
let rec interpret_expr expr value_environment function_environment
    class_environment =
  let ( >>= ) = Core.Result.( >>= ) in
  match expr with
  | Integer (_, i, _) -> Ok (VInt i, value_environment)
  | Boolean (_, b, _) -> Ok (VBool b, value_environment)
  | BinOp (_, _, bin_op, e1, e2) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      interpret_expr e2 val_env_1 function_environment class_environment
      >>= fun (val2, val_env_2) ->
      interpret_bin_op bin_op val1 val2
      >>= fun output_val -> Ok (output_val, val_env_2)
  | CompOp (_, _, comp_op, e1, e2) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      interpret_expr e2 val_env_1 function_environment class_environment
      >>= fun (val2, val_env_2) ->
      interpret_comp_op comp_op val1 val2
      >>= fun output_val -> Ok (output_val, val_env_2)
  | BoolOp (_, _, bool_comp_op, e1, e2) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      interpret_expr e2 val_env_1 function_environment class_environment
      >>= fun (val2, val_env_2) ->
      interpret_bool_comp_op bool_comp_op val1 val2
      >>= fun output_val -> Ok (output_val, val_env_2)
  | UnaryOp (_, _, unary_op, e1) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      interpret_unary_op unary_op val1
      >>= fun output_val -> Ok (output_val, val_env_1)
  | Identifier (_, var_type, identifier) -> (
      lookup_var_value value_environment identifier
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Variable %s does not have a value" identifier) )
      | Some var_value -> (
        match (var_type, var_value) with
        | (TEInt, _), VInt i -> Ok (VInt i, value_environment)
        | (TEBool, _), VBool b -> Ok (VBool b, value_environment)
        | _ ->
            Error
              (Core.Error.of_string
                 "Type error: variable type does not match value type" ) ) )
  | Let (_, var_name, _, e1, e2, _) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      let new_env = (var_name, val1) :: val_env_1 in
      interpret_expr e2 new_env function_environment class_environment
      >>= fun (val2, val_env_2) ->
      Ok (val2, remove_var_from_env var_name val_env_2)
  | Assign (_, _, var_name, e1) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      Ok (val1, update_var_in_env var_name val1 val_env_1)
  | If (_, e1, e2, e3, _) -> (
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (val1, val_env_1) ->
      match val1 with
      | VBool true ->
          interpret_expr e2 val_env_1 function_environment class_environment
      | VBool false ->
          interpret_expr e3 val_env_1 function_environment class_environment
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
               "Type error: Cannot have object type in the if condition" ) )
  | Classify (_, e1, _) ->
      interpret_expr e1 value_environment function_environment
        class_environment
  | Declassify (_, e1, _) ->
      interpret_expr e1 value_environment function_environment
        class_environment
  | FunctionApp (_, _, f_name, args) -> (
    match Stdlib.List.assoc_opt f_name function_environment with
    | Some (param_names, body) ->
        let rec eval_args val_env args acc =
          match args with
          | [] -> Ok (List.rev acc, val_env)
          | arg :: rest ->
              interpret_expr arg val_env function_environment
                class_environment
              >>= fun (val1, val_env_1) ->
              eval_args val_env_1 rest (val1 :: acc)
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
          interpret_expr body new_env function_environment class_environment
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
      interpret_expr e value_environment function_environment
        class_environment
  | Seq (_, e1, e2, _) ->
      interpret_expr e1 value_environment function_environment
        class_environment
      >>= fun (_, val_env_1) ->
      interpret_expr e2 val_env_1 function_environment class_environment
  | Print (_, args) ->
      let rec print_args val_env args =
        match args with
        | [] -> Ok (VUnit (), val_env)
        | arg :: rest ->
            interpret_expr arg val_env function_environment class_environment
            >>= fun (val1, val_env_1) ->
            print_endline (value_to_string val1) ;
            print_args val_env_1 rest
      in
      print_args value_environment args
  | SecurePrint (_, args) ->
      let rec print_args val_env args =
        match args with
        | [] -> Ok (VUnit (), val_env)
        | arg :: rest ->
            interpret_expr arg val_env function_environment class_environment
            >>= fun (val1, val_env_1) ->
            print_endline (value_to_string val1) ;
            print_args val_env_1 rest
      in
      print_args value_environment args
  | Skip _ -> Ok (VUnit (), value_environment)
  | Object (_, _, class_name, args, _) ->
      let rec eval_args val_env args acc =
        match args with
        | [] -> Ok (List.rev acc, val_env)
        | field_expr :: rest ->
            interpret_expr field_expr val_env function_environment
              class_environment
            >>= fun (val1, val_env_1) ->
            eval_args val_env_1 rest (val1 :: acc)
      in
      eval_args value_environment args []
      >>= fun (arg_values, updated_value_env) ->
      Ok (VObject (class_name, arg_values), updated_value_env)
  | MethodCall (_, _, obj_name, method_name, arg_exprs) -> (
      lookup_var_value value_environment obj_name
      |> function
      | None ->
          Error
            (Core.Error.of_string
               (Printf.sprintf "Object %s not found" obj_name) )
      | Some obj_val -> (
        match obj_val with
        | VObject (obj_name, _) -> (
          match get_class_info obj_name class_environment with
          | None ->
              Error
                (Core.Error.of_string
                   (Printf.sprintf "Class %s not found" obj_name) )
          | Some {methods; _} ->
              get_method_info method_name methods
              >>= fun (_, param_info, method_body) ->
              let rec eval_args val_env args acc =
                match args with
                | [] -> Ok (List.rev acc, val_env)
                | arg :: rest ->
                    interpret_expr arg val_env function_environment
                      class_environment
                    >>= fun (val1, val_env_1) ->
                    eval_args val_env_1 rest (val1 :: acc)
              in
              eval_args value_environment arg_exprs []
              >>= fun (arg_values, args_updated_value_env) ->
              if List.length param_info <> List.length arg_values then
                Error
                  (Core.Error.of_string
                     (Printf.sprintf
                        "Arity mismatch in method '%s': expected %d \
                         arguments but got %d"
                        method_name (List.length param_info)
                        (List.length arg_values) ) )
              else
                let new_env =
                  match Core.List.zip param_info arg_values with
                  | Ok pairs ->
                      Core.List.map pairs ~f:(fun (param_name, value) ->
                          (param_name, value) )
                      @ args_updated_value_env
                  | Unequal_lengths ->
                      failwith
                        "Arity mismatch: number of arguments provided does \
                         not match the method's parameter count"
                in
                (* Interpret the method body with the updated environment *)
                interpret_expr method_body new_env function_environment
                  class_environment )
        | _ ->
            Error
              (Core.Error.of_string
                 "Type error: cannot call method on non-object value" ) ) )
