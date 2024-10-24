open Core
open Typing.Typed_ast
open Ast_types

let apply_int_bin_op bin_op i1 i2 = match bin_op with 
| PLUS -> Ok (VInt(i1 + i2))
| MINUS ->  Ok(VInt (i1 - i2))
| MULTIPLY ->  Ok (VInt(i1 * i2))
| DIVIDE -> if i2 = 0 then Error (Error.of_string "Division by zero")
else Ok (VInt(i1 / i2))

let rec interpret_expr expr = 
  let (>>=) = Result.(>>=) in 
  match expr with 
  | Integer(_, i) -> Ok(VInt(i))
  | BinOp(_, _, bin_op, e1, e2) -> (
  interpret_expr e1 
  >>= fun val1 ->
  interpret_expr e2
  >>= fun val2 ->
match (val1, val2) with 
| (VInt(i1), VInt(i2)) -> 
  apply_int_bin_op bin_op i1 i2
  )

let interpret_program (Prog(expr)) = 
interpret_expr expr