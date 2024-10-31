open Core
open Parser_frontend
open Compiler_types.Language_types
open Type_environment

let check_var_shadowing type_environment var_name =
  let ast_var_type = lookup_var_type type_environment var_name in
  match ast_var_type with
  (* If this is none, it means the variable has not been declared yet *)
  | None -> Ok ()
  | Some _ ->
      Error
        (Error.of_string
           "Variable has already been assigned and has another type" )

let rec type_expr expr type_environment =
  let ( >>= ) = Result.( >>= ) in
  match expr with
  | Parsed_ast.Integer (loc, i) -> Ok (TEInt, Typed_ast.Integer (loc, i))
  | Parsed_ast.BinOp (loc, bin_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun (e1_type, typed_e1) ->
      type_expr e2 type_environment
      >>= fun (e2_type, typed_e2) ->
      if phys_equal e1_type e2_type && phys_equal e1_type TEInt then
        Ok (TEInt, Typed_ast.BinOp (loc, TEInt, bin_op, typed_e1, typed_e2))
      else Error (Error.of_string "binary operands type error")
  | Parsed_ast.CompOp (loc, comp_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun (e1_type, typed_e1) ->
      type_expr e2 type_environment
      >>= fun (e2_type, typed_e2) ->
      if phys_equal e1_type e2_type && phys_equal e1_type TEInt then
        Ok
          (TEBool, Typed_ast.CompOp (loc, TEInt, comp_op, typed_e1, typed_e2))
      else Error (Error.of_string "comparison operands type error")
  | Parsed_ast.Boolean (loc, b) -> Ok (TEBool, Typed_ast.Boolean (loc, b))
  | Parsed_ast.BoolCompOp (loc, bool_comp_op, e1, e2) ->
      type_expr e1 type_environment
      >>= fun (e1_type, typed_e1) ->
      type_expr e2 type_environment
      >>= fun (e2_type, typed_e2) ->
      if phys_equal e1_type e2_type && phys_equal e1_type TEBool then
        Ok
          ( TEBool
          , Typed_ast.BoolCompOp
              (loc, TEBool, bool_comp_op, typed_e1, typed_e2) )
      else Error (Error.of_string "boolean comparison operands type error")
  | Parsed_ast.Identifier (loc, id) -> (
      lookup_var_type type_environment id
      |> function
      | None -> Error (Error.of_string "Variable does not exist")
      | Some var_type ->
          Ok (var_type, Typed_ast.Identifier (loc, var_type, id)) )
  | Parsed_ast.Let (loc, var_name, var_type, e1, e2) ->
      check_var_shadowing type_environment var_name
      >>= fun () ->
      type_expr e1 type_environment
      >>= fun (e1_type, typed_e1) ->
      if not (phys_equal var_type e1_type) then
        Error
          (Error.of_string "Types of the let and expression do not match")
      else
        type_expr e2 ((var_name, var_type) :: type_environment)
        >>= fun (e2_type, typed_e2) ->
        Ok
          ( e2_type
          , Typed_ast.Let
              (loc, var_name, var_type, typed_e1, typed_e2, e2_type) )

let type_program (Parsed_ast.Prog expr) =
  let open Result in
  type_expr expr [] >>= fun (_, typed_expr) -> Ok (Typed_ast.Prog typed_expr)
