open Core
open Typing.Typed_ast
open Interpret_expr

(* let interpret_program (Prog (typed_fn_defns, typed_expr)) = (* let
   initial_environment = [] in interpret_expr typed_expr initial_environment
   *) let ( >>= ) = Result.( >>= ) in interpret_fn_defns typed_fn_defns []
   >>= fun value_environment -> interpret_expr typed_expr
   value_environment *)
let interpret_program (Prog (typed_fn_defns, typed_expr)) =
  let ( >>= ) = Result.( >>= ) in
  interpret_fn_defns typed_fn_defns []
  >>= fun function_environment ->
  interpret_expr typed_expr [] function_environment
