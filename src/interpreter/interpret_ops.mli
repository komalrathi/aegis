open Compiler_types.Ast_types
open Compiler_types.Language_types
open Core

val apply_int_bin_op :
  bin_op -> int -> int -> (interpreter_val, Error.t) result

val apply_comp_op :
  comp_op -> int -> int -> (interpreter_val, Error.t) result

val interpret_bin_op :
     bin_op
  -> interpreter_val
  -> interpreter_val
  -> (interpreter_val, Error.t) result

val interpret_comp_op :
     comp_op
  -> interpreter_val
  -> interpreter_val
  -> (interpreter_val, Error.t) result

val interpret_bool_comp_op :
     bool_op
  -> interpreter_val
  -> interpreter_val
  -> (interpreter_val, Error.t) result

val interpret_unary_op :
  unary_op -> interpreter_val -> (interpreter_val, Error.t) result
