open Core
open Print.Print_interpret_expr
open Compiler_types.Language_types
open Compiler_types.Ast_types
open Typing.Typed_ast

let%expect_test "Integer Plus Operation" =
  let i1 = Integer (Lexing.dummy_pos, 1, TSLow) in
  let i2 = Integer (Lexing.dummy_pos, 5, TSLow) in
  let bin_op = BinOpPlus in
  print_interpret_expr
    (BinOp (Lexing.dummy_pos, (TEInt, TSLow), bin_op, i1, i2))
    [] [] [] ;
  [%expect
    {|
    Result: VInt(6)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]

let%expect_test "Less than comparison" =
  let i1 = Integer (Lexing.dummy_pos, 1, TSLow) in
  let i2 = Integer (Lexing.dummy_pos, 5, TSLow) in
  let comp_op = CompOpLessThan in
  print_interpret_expr
    (CompOp (Lexing.dummy_pos, (TEInt, TSLow), comp_op, i1, i2))
    [] [] [] ;
  [%expect
    {|
    Result: VBool(true)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]

let%expect_test "Boolean And Operation" =
  let b1 = Boolean (Lexing.dummy_pos, true, TSLow) in
  let b2 = Boolean (Lexing.dummy_pos, false, TSLow) in
  let bool_op = BoolOpAnd in
  print_interpret_expr
    (BoolOp (Lexing.dummy_pos, (TEBool, TSLow), bool_op, b1, b2))
    [] [] [] ;
  [%expect
    {|
    Result: VBool(false)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]

let%expect_test "Unary Operation" =
  let b = Boolean (Lexing.dummy_pos, true, TSLow) in
  let unary_op = UnaryOpNot in
  print_interpret_expr
    (UnaryOp (Lexing.dummy_pos, (TEBool, TSLow), unary_op, b))
    [] [] [] ;
  [%expect
    {|
    Result: VBool(false)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]

let%expect_test "If statement" =
  let i1 = Boolean (Lexing.dummy_pos, true, TSLow) in
  let i2 = Integer (Lexing.dummy_pos, 5, TSLow) in
  let i3 = Integer (Lexing.dummy_pos, 10, TSLow) in
  let if_expr = If (Lexing.dummy_pos, i1, i2, i3, (TEInt, TSLow)) in
  print_interpret_expr if_expr [] [] [] ;
  [%expect
    {|
    Result: VInt(5)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]

let%expect_test "Function application" =
  let function_defn =
    FunctionDefn
      ( "example"
      , [TArg ("x", (TEInt, TSLow)); TArg ("y", (TEInt, TSLow))]
      , (TEInt, TSLow)
      , BinOp
          ( Lexing.dummy_pos
          , (TEInt, TSLow)
          , BinOpPlus
          , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "x")
          , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "y") ) )
  in
  let function_app =
    FunctionApp
      ( Lexing.dummy_pos
      , (TEInt, TSLow)
      , "example"
      , [ Integer (Lexing.dummy_pos, 5, TSLow)
        ; Integer (Lexing.dummy_pos, 10, TSLow) ] )
  in
  match
    Interpreter.Interpret_fn_defn.interpret_fn_defns [function_defn] []
  with
  | Ok function_env ->
      print_interpret_expr function_app [] function_env [] ;
      [%expect
        {|
        Result: VInt(15)
        Value Environment: x -> VInt(5); y -> VInt(10);
        Function Environment: example(x, y) -> BinOp(Plus, Identifier((Int, Low)), Identifier((Int, Low)));
        Class Environment:
        |}]
  | Error err -> Printf.printf "Error: %s\n" (Error.to_string_hum err)

let%expect_test "Let statement" =
  let id = Identifier (Lexing.dummy_pos, (TEInt, TSLow), "test_var") in
  let i = Integer (Lexing.dummy_pos, 72, TSLow) in
  let let_expr =
    Let
      ( Lexing.dummy_pos
      , "test_var"
      , (TEInt, TSLow)
      , i
      , BinOp
          ( Lexing.dummy_pos
          , (TEInt, TSLow)
          , BinOpMinus
          , id
          , Integer (Lexing.dummy_pos, 5, TSLow) )
      , (TEInt, TSLow) )
  in
  print_interpret_expr let_expr [] [] [] ;
  [%expect
    {|
    Result: VInt(67)
    Value Environment:
    Function Environment:
    Class Environment:
    |}]
