open Core
open Ast_types
open Typing


val interpret_program : Typed_ast.program -> interpreter_val Or_error.t