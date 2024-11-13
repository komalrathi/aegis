open Core
open Compiler_types.Language_types
open Typing

val interpret_program : Typed_ast.program -> interpreter_val Or_error.t
(* val interpret_program :
     Typing.Typed_ast.program
  -> Value_environment.value_environment
  -> Compiler_types.Language_types.interpreter_val Core.Or_error.t *)
