open Core
open Parser_frontend
open Type_expr

let type_program (Parsed_ast.Prog (_, expr)) =
  let open Result in
  type_expr expr [] >>= fun (_, typed_expr) -> Ok (Typed_ast.Prog typed_expr)
