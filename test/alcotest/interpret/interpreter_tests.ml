open Compiler_types.Language_types
open Compiler_types.Ast_types
open Interpreter.Interpret_ops
open Interpreter.Interpret_expr

let test_apply_int_plus_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_int_plus_bin_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpPlus))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 + i2
      | _ -> false )

let test_apply_int_minus_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_int_minus_bin_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpMinus))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 - i2
      | _ -> false )

let test_apply_int_multiply_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_int_multiply_bin_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpMultiply))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 * i2
      | _ -> false )

let test_apply_int_divide_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_int_divide_bin_op"
    QCheck.(
      triple (QCheck.make (Gen.return BinOpDivide)) small_signed_int pos_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 / i2
      | _ -> false )

let test_apply_lt_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_lt_comp_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpLessThan))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 < i2)
      | _ -> false )

let test_apply_gt_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_gt_comp_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpGreaterThan))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 > i2)
      | _ -> false )

let test_apply_lte_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_lte_comp_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpLessThanEqual))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 <= i2)
      | _ -> false )

let test_apply_gte_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_gte_comp_op"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpGreaterThanEqual))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 >= i2)
      | _ -> false )

(* Testing the interpret_expr function *)
let test_binop_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"binop_intepret_expr"
    QCheck.(
      quad
        (QCheck.make (Gen.return BinOpPlus))
        small_signed_int small_signed_int
        (QCheck.make (Gen.return TSLow)) )
    (fun (op, i1, i2, sl) ->
      match
        interpret_expr
          (BinOp
             ( Lexing.dummy_pos
             , (TEInt, TSLow)
             , op
             , Integer (Lexing.dummy_pos, i1, sl)
             , Integer (Lexing.dummy_pos, i2, sl) ) )
          [] [] []
      with
      | Ok (VInt i, _) -> i = i1 + i2
      | _ -> false )

let test_comp_op_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"compop_intepret_expr"
    QCheck.(
      quad
        (QCheck.make (Gen.return CompOpLessThan))
        small_signed_int small_signed_int
        (QCheck.make (Gen.return TSLow)) )
    (fun (op, i1, i2, sl) ->
      match
        interpret_expr
          (CompOp
             ( Lexing.dummy_pos
             , (TEInt, TSLow)
             , op
             , Integer (Lexing.dummy_pos, i1, sl)
             , Integer (Lexing.dummy_pos, i2, sl) ) )
          [] [] []
      with
      | Ok (VBool b, _) -> b = (i1 < i2)
      | _ -> false )

let test_boolean_compop_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"compop_intepret_expr"
    QCheck.(
      quad
        (QCheck.make (Gen.return BoolOpAnd))
        bool bool
        (QCheck.make (Gen.return TSLow)) )
    (fun (op, b1, b2, sl) ->
      match
        interpret_expr
          (BoolOp
             ( Lexing.dummy_pos
             , (TEBool, TSLow)
             , op
             , Boolean (Lexing.dummy_pos, b1, sl)
             , Boolean (Lexing.dummy_pos, b2, sl) ) )
          [] [] []
      with
      | Ok (VBool b, _) -> b = (b1 && b2)
      | _ -> false )

let test_unary_op_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"unaryop_intepret_expr"
    QCheck.(
      triple
        (QCheck.make (Gen.return UnaryOpNot))
        bool
        (QCheck.make (Gen.return TSLow)) )
    (fun (op, b, sl) ->
      match
        interpret_expr
          (UnaryOp
             ( Lexing.dummy_pos
             , (TEBool, TSLow)
             , op
             , Boolean (Lexing.dummy_pos, b, sl) ) )
          [] [] []
      with
      | Ok (VBool result, _) -> result = not b
      | _ -> false )

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest
      [ test_apply_int_plus_bin_op ()
      ; test_apply_int_minus_bin_op ()
      ; test_apply_int_multiply_bin_op ()
      ; test_apply_int_divide_bin_op ()
      ; test_apply_lt_comp_op ()
      ; test_apply_gt_comp_op ()
      ; test_apply_lte_comp_op ()
      ; test_apply_gte_comp_op ()
      ; test_binop_interpret_expr ()
      ; test_comp_op_interpret_expr ()
      ; test_boolean_compop_interpret_expr ()
      ; test_unary_op_interpret_expr () ]
  in
  Alcotest.run "interpreter_tests.ml\n Tests" [("qcheck", qcheck_tests)]
