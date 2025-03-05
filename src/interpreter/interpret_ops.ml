open Compiler_types.Ast_types
open Compiler_types.Language_types

let apply_int_bin_op bin_op i1 i2 =
  match bin_op with
  | BinOpPlus -> Ok (VInt (i1 + i2))
  | BinOpMinus -> Ok (VInt (i1 - i2))
  | BinOpMultiply -> Ok (VInt (i1 * i2))
  | BinOpDivide ->
      if i2 = 0 then Error (Core.Error.of_string "Division by zero")
      else Ok (VInt (i1 / i2))
  | BinOpExponentiation ->
      Ok (VInt (int_of_float (float_of_int i1 ** float_of_int i2)))
  | BinOpModulus ->
      if i2 = 0 then Error (Core.Error.of_string "Division by zero")
      else Ok (VInt (i1 mod i2))

let apply_comp_op comp_op i1 i2 =
  match comp_op with
  | CompOpLessThan -> Ok (VBool (i1 < i2))
  | CompOpGreaterThan -> Ok (VBool (i1 > i2))
  | CompOpLessThanEqual -> Ok (VBool (i1 <= i2))
  | CompOpGreaterThanEqual -> Ok (VBool (i1 >= i2))
  | CompOpEqual -> Ok (VBool (i1 = i2))

let interpret_bin_op bin_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_int_bin_op bin_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to boolean values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to unit values" )
  | VObject _, _ | _, VObject _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply binary operation to object values" )

let interpret_comp_op comp_op i1 i2 =
  match (i1, i2) with
  | VInt i1, VInt i2 -> apply_comp_op comp_op i1 i2
  | VInt _, VBool _ | VBool _, VInt _ | VBool _, VBool _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to boolean values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to unit values" )
  | VObject _, _ | _, VObject _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply comparison operation to object values" )

let interpret_bool_comp_op bool_comp_op b1 b2 =
  match (b1, b2) with
  | VBool b1, VBool b2 -> (
    match bool_comp_op with
    | BoolOpAnd -> Ok (VBool (b1 && b2))
    | BoolOpOr -> Ok (VBool (b1 || b2)) )
  | VInt _, VBool _ | VBool _, VInt _ | VInt _, VInt _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to integer values" )
  | VUnit _, _ | _, VUnit _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to unit values" )
  | _, _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply boolean operation to object values" )

let interpret_unary_op unary_op b =
  match b with
  | VBool b -> ( match unary_op with UnaryOpNot -> Ok (VBool (not b)) )
  | _ ->
      Error
        (Core.Error.of_string
           "Type error: cannot apply unary operation to integer values" )
