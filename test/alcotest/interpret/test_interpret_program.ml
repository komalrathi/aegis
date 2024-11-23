open Compiler_types.Language_types
open Compiler_types.Ast_types
open Interpreter.Interpret_program
(* open Lexing *)

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
      triple
        (QCheck.make (Gen.return BinOpDivide))
        small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_int_bin_op op i1 i2 with
      | Ok (VInt i) -> i = i1 / i2
      | _ -> false )

let test_apply_lt_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_lt_comp_op"
    QCheck.(
      triple (QCheck.make (Gen.return CompOpLessThan)) small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 < i2)
      | _ -> false )

let test_apply_gt_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_gt_comp_op"
    QCheck.(
      triple (QCheck.make (Gen.return CompOpGreaterThan)) small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 > i2)
      | _ -> false )

let test_apply_lte_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_lte_comp_op"
    QCheck.(
      triple (QCheck.make (Gen.return CompOpLessThanEqual)) small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 <= i2)
      | _ -> false )

let test_apply_gte_comp_op () =
  QCheck.Test.make ~count:1000 ~name:"apply_gte_comp_op"
    QCheck.(
      triple (QCheck.make (Gen.return CompOpGreaterThanEqual)) small_signed_int small_signed_int )
    (fun (op, i1, i2) ->
      match apply_comp_op op i1 i2 with
      | Ok (VBool b) -> b = (i1 >= i2)
      | _ -> false )

(* Security level tests *)

let test_integer_low_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"integer_intepret_expr"
    QCheck.(pair small_signed_int (QCheck.make (Gen.return TSLow)))
    (fun (i, sl) ->
      match interpret_expr (Integer (Lexing.dummy_pos, i, sl)) [] with
      | Ok (VInt i') -> i = i'
      | _ -> false )

let test_integer_high_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"integer_intepret_expr"
    QCheck.(pair small_signed_int (QCheck.make (Gen.return TSHigh)))
    (fun (i, sl) ->
      match interpret_expr (Integer (Lexing.dummy_pos, i, sl)) [] with
      | Ok (VInt i') -> i = i'
      | _ -> false )

let test_boolean_low_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"boolean_intepret_expr"
    QCheck.(pair bool (QCheck.make (Gen.return TSLow)))
    (fun (b, sl) ->
      match interpret_expr (Boolean (Lexing.dummy_pos, b, sl)) [] with
      | Ok (VBool b') -> b = b'
      | _ -> false )

let test_boolean_high_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"boolean_intepret_expr"
    QCheck.(pair bool (QCheck.make (Gen.return TSHigh)))
    (fun (b, sl) ->
      match interpret_expr (Boolean (Lexing.dummy_pos, b, sl)) [] with
      | Ok (VBool b') -> b = b'
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
          []
      with
      | Ok (VInt i) -> i = i1 + i2
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
          []
      with
      | Ok (VBool b) -> b = (i1 < i2)
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
          []
      with
      | Ok (VBool b) -> b = (b1 && b2)
      | _ -> false )

let test_identifier_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"identifier_intepret_expr"
    QCheck.(
      pair (QCheck.make (Gen.return TSLow)) (QCheck.make Gen.small_string) )
    (fun (sl, id) ->
      match
        interpret_expr
          (Identifier (Lexing.dummy_pos, (TEInt, sl), id))
          [(id, VInt 5)]
      with
      | Ok (VInt i) -> i = 5
      | _ -> false )

let test_let_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"let_intepret_expr"
    QCheck.(
      quad
        (QCheck.make Gen.small_string)
        (QCheck.make (Gen.return TSLow))
        small_signed_int small_signed_int )
    (fun (id, sl, i1, i2) ->
      match
        interpret_expr
          (Let
             ( Lexing.dummy_pos
             , id
             , (TEInt, sl)
             , Integer (Lexing.dummy_pos, i1, sl)
             , Integer (Lexing.dummy_pos, i2, sl)
             , (TEInt, sl) ) )
          []
      with
      | Ok (VInt i) -> i = i2
      | _ -> false )

let test_assign_intepret_expr () =
  QCheck.Test.make ~count:1000 ~name:"assign_intepret_expr"
    QCheck.(
      quad
        (QCheck.make Gen.small_string)
        (QCheck.make (Gen.return TSLow))
        small_signed_int small_signed_int )
    (fun (id, sl, i1, i2) ->
      match
        interpret_expr
          (Assign
             ( Lexing.dummy_pos
             , (TEInt, sl)
             , id
             , Integer (Lexing.dummy_pos, i1, sl) ) )
          [(id, VInt i2)]
      with
      | Ok (VInt i) -> i = i2
      | _ -> false )

let test_if_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"if_intepret_expr"
    QCheck.(
      quad
        (QCheck.make (Gen.return TSLow))
        bool small_signed_int small_signed_int )
    (fun (sl, b, i1, i2) ->
      match
        interpret_expr
          (If
             ( Lexing.dummy_pos
             , Boolean (Lexing.dummy_pos, b, sl)
             , Integer (Lexing.dummy_pos, i1, sl)
             , Integer (Lexing.dummy_pos, i2, sl)
             , (TEInt, sl) ) )
          []
      with
      | Ok (VInt i) -> if b then i = i1 else i = i2
      | _ -> false )

let test_classify_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"classify_intepret_expr"
    QCheck.(
      triple
        (QCheck.make (Gen.return TSLow))
        small_signed_int
        (QCheck.make (Gen.return TSHigh)) )
    (fun (sl1, i, sl2) ->
      match
        interpret_expr
          (Classify
             ( Lexing.dummy_pos
             , Integer (Lexing.dummy_pos, i, sl1)
             , (TEInt, sl2) ) )
          []
      with
      | Ok (VInt i') -> i = i'
      | _ -> false )

let test_declassify_interpret_expr () =
  QCheck.Test.make ~count:1000 ~name:"declassify_intepret_expr"
    QCheck.(
      triple
        (QCheck.make (Gen.return TSHigh))
        small_signed_int
        (QCheck.make (Gen.return TSLow)) )
    (fun (sl1, i, sl2) ->
      match
        interpret_expr
          (Declassify
             ( Lexing.dummy_pos
             , Integer (Lexing.dummy_pos, i, sl1)
             , (TEInt, sl2) ) )
          []
      with
      | Ok (VInt i') -> i = i'
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
      ; test_integer_low_interpret_expr ()
      ; test_integer_high_interpret_expr ()
      ; test_boolean_low_interpret_expr ()
      ; test_boolean_high_interpret_expr ()
      ; test_binop_interpret_expr ()
      ; test_comp_op_interpret_expr ()
      ; test_boolean_compop_interpret_expr ()
      ; test_identifier_interpret_expr ()
      ; test_let_interpret_expr ()
      ; test_assign_intepret_expr ()
      ; test_if_interpret_expr ()
      ; test_classify_interpret_expr ()
      ; test_declassify_interpret_expr () ]
  in
  Alcotest.run "interpret_program.ml Tests" [("qcheck", qcheck_tests)]
