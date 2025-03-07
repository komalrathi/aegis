open Compiler_types.Language_types
open Compiler_types.Ast_types
open Interpreter.Interpret_ops
open Interpreter.Interpret_expr

let test_int_plus_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Addition of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpPlus))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 + i2
      | _ -> false )

let test_int_minus_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Subtraction of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpMinus))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 - i2
      | _ -> false )

let test_int_multiply_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Multiplication of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpMultiply))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 * i2
      | _ -> false )

let test_int_divide_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Division of two integers"
    QCheck.(
      triple (QCheck.make (Gen.return BinOpDivide)) small_signed_int pos_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 / i2
      | _ -> false )

let test_lt_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"Less Than comparison of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpLessThan))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 < i2)
      | _ -> false )

let test_gt_comp_op () =
  QCheck.Test.make ~count:1000
    ~name:"Greater Than comparison of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpGreaterThan))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 > i2)
      | _ -> false )

let test_lte_comp_op () =
  QCheck.Test.make ~count:1000
    ~name:"Less Than Equal comparison of two integers"
    QCheck.(
      triple
        (QCheck.make (Gen.return CompOpLessThanEqual))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 <= i2)
      | _ -> false )

let test_gte_comp_op () =
  QCheck.Test.make ~count:1000
    ~name:"Greater Than Equal comparison of two integers"
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
      | Ok (IValue (VInt i, _)) -> i = i1 + i2
      | _ -> false )

let test_comp_op_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"Integer compop_intepret_expr"
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
      | Ok (IValue (VBool b, _)) -> b = (i1 < i2)
      | _ -> false )

let test_boolean_compop_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"Boolean compop_intepret_expr"
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
      | Ok (IValue (VBool b, _)) -> b = (b1 && b2)
      | _ -> false )

let test_unary_op_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"Unary op interpret_expr"
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
      | Ok (IValue (VBool result, _)) -> result = not b
      | _ -> false )

let test_exponentiation_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Integer exponentiation"
    QCheck.(
      triple
        (QCheck.make (Gen.return BinOpExponentiation))
        small_signed_int pos_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = int_of_float (float i1 ** float i2)
      | _ -> false )

let test_modulus_bin_op () =
  QCheck.Test.make ~count:1000 ~name:"Integer modulus"
    QCheck.(
      triple (QCheck.make (Gen.return BinOpModulus)) small_signed_int pos_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 mod i2
      | _ -> false )

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest
      [ test_int_plus_bin_op ()
      ; test_int_minus_bin_op ()
      ; test_int_multiply_bin_op ()
      ; test_int_divide_bin_op ()
      ; test_lt_comp_op ()
      ; test_gt_comp_op ()
      ; test_lte_comp_op ()
      ; test_gte_comp_op ()
      ; test_binop_interpret_expr ()
      ; test_comp_op_interpret_expr ()
      ; test_boolean_compop_interpret_expr ()
      ; test_unary_op_interpret_expr ()
      ; test_exponentiation_bin_op ()
      ; test_modulus_bin_op () ]
  in
  Alcotest.run "interpreter_tests.ml\n Tests" [("qcheck", qcheck_tests)]
