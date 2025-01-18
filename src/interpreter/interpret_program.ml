open Core
open Typing.Typed_ast
open Interpret_expr

let interpret_program (Prog (typed_class_defns, typed_fn_defns, typed_expr))
    =
  let ( >>= ) = Result.( >>= ) in
  interpret_class_defns typed_class_defns []
  >>= fun class_environment ->
  interpret_fn_defns typed_fn_defns []
  >>= fun function_environment ->
  interpret_expr typed_expr [] function_environment class_environment
  >>= fun (output_val, _) -> Ok output_val