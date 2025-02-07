open Core
open Typing.Typed_ast
open Interpret_expr
open Interpret_fn_defn

let interpret_program (Prog (typed_class_defns, typed_fn_defns, typed_expr))
    =
  let ( >>= ) = Result.( >>= ) in
  interpret_fn_defns typed_fn_defns []
  >>= fun function_environment ->
  interpret_expr typed_expr [] function_environment typed_class_defns
  >>= fun (output_val, _) -> Ok output_val
