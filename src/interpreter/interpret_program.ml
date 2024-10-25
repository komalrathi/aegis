open Core
open Typing.Typed_ast
open Ast_types

let apply_int_bin_op bin_op i1 i2 = match bin_op with 
  | PLUS -> Ok (VInt(i1 + i2))
  | MINUS ->  Ok(VInt (i1 - i2))
  | MULTIPLY ->  Ok (VInt(i1 * i2))
  | DIVIDE -> if i2 = 0 then Error (Error.of_string "Division by zero")
  else Ok (VInt(i1 / i2))

let apply_comp_op comp_op i1 i2 = match comp_op with 
  | LT -> Ok (VBool(i1 < i2))
  | GT -> Ok (VBool(i1 > i2))
  | LTE -> Ok (VBool(i1 <= i2))
  | GTE -> Ok (VBool(i1 >= i2))

let interpret_bin_op bin_op i1 i2 =
  match (i1, i2) with 
  | (VInt(i1), VInt(i2)) -> apply_int_bin_op bin_op i1 i2
  | (VInt(_), VBool(_)) | (VBool(_), VInt(_)) | (VBool(_), VBool(_)) ->
    Error (Error.of_string "Type error: cannot apply binary operation to boolean values")

let interpret_comp_op comp_op i1 i2 =
  match (i1, i2) with 
  | (VInt(i1), VInt(i2)) -> apply_comp_op comp_op i1 i2
  | (VInt(_), VBool(_)) | (VBool(_), VInt(_)) | (VBool(_), VBool(_)) ->
    Error (Error.of_string "Type error: cannot apply comparison operation to boolean values")

      
let rec interpret_expr expr = 
  let (>>=) = Result.(>>=) in 
  match expr with 
  | Integer(_, i) -> Ok(VInt(i))
  | BinOp(_, _, bin_op, e1, e2) -> (
    interpret_expr e1 
    >>= fun val1 ->
    interpret_expr e2
    >>= fun val2 ->
    (* match (val1, val2) with 
    | (VInt(i1), VInt(i2)) ->  *)
      interpret_bin_op bin_op val1 val2
      )
  | CompOp(_, _, comp_op, e1, e2) -> (
    interpret_expr e1 
    >>= fun val1 ->
    interpret_expr e2
    >>= fun val2 ->
      interpret_comp_op comp_op val1 val2
      )
  | Boolean(_, b) -> Ok(VBool(b))
  | BoolCompOp(_, _, bool_comp_op, e1, e2) -> (
    interpret_expr e1 
    >>= fun val1 ->
    interpret_expr e2
    >>= fun val2 ->
      match (val1, val2) with 
      | (VBool(b1), VBool(b2)) -> (
        match bool_comp_op with 
        | AND -> Ok(VBool(b1 && b2))
        | OR -> Ok(VBool(b1 || b2))
      )
      | (VInt(_), VBool(_)) | (VBool(_), VInt(_)) | (VInt(_), VInt(_)) ->
        Error (Error.of_string "Type error: cannot apply boolean operation to integer values")
    ) 

let interpret_program (Prog(expr)) = 
  interpret_expr expr