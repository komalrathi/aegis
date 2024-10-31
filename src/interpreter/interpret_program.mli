open Core
open Compiler_types.Language_types
open Typing

val interpret_program : Typed_ast.program -> interpreter_val Or_error.t
