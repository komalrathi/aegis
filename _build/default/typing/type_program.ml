open Core
open Parser_frontend
open Ast_types

let rec type_expr expr =   
let (>>=) = Result.(>>=) in 
match expr with  
| Parsed_ast.Integer (loc, i) -> Ok (TEInt, Typed_ast.Integer (loc, i))
| Parsed_ast.BinOp (loc, bin_op, e1, e2) -> (
    type_expr e1
    >>= fun (e1_type, typed_e1) -> 
      type_expr e2
      >>= fun (e2_type, typed_e2) ->
        (
          if (phys_equal e1_type e2_type) && (phys_equal e1_type TEInt) then
              Ok (TEInt, 
            Typed_ast.BinOp(loc, TEInt, bin_op, typed_e1, typed_e2))
          else Error (Error.of_string "binary operands type error")
        )
  )
  | Parsed_ast.CompOp (loc, comp_op, e1, e2) -> (
    type_expr e1
    >>= fun (e1_type, typed_e1) -> 
      type_expr e2
      >>= fun (e2_type, typed_e2) ->
        (
          if (phys_equal e1_type e2_type) && (phys_equal e1_type TEInt) then
              Ok (TEBool, 
            Typed_ast.CompOp(loc, TEInt, comp_op, typed_e1, typed_e2))
          else Error (Error.of_string "comparison operands type error")
        )
  )
  | Parsed_ast.Boolean (loc, b) -> Ok (TEBool, Typed_ast.Boolean (loc, b))
  | Parsed_ast.BoolCompOp (loc, bool_comp_op, e1, e2) -> (
    type_expr e1
    >>= fun (e1_type, typed_e1) -> 
      type_expr e2
      >>= fun (e2_type, typed_e2) ->
        (
          if (phys_equal e1_type e2_type) && (phys_equal e1_type TEBool) then
              Ok (TEBool, 
            Typed_ast.BoolCompOp(loc, TEBool, bool_comp_op, typed_e1, typed_e2))
          else Error (Error.of_string "boolean comparison operands type error")
        )
  )

let type_program (Parsed_ast.Prog(expr)) = 
  let open Result in 
  type_expr expr  
  >>= fun (_, typed_expr) -> 
  Ok(Typed_ast.Prog(typed_expr))