open Core
open Print_parsed_ast

let%expect_test "Binary Operation" =
  print_parsed_ast (Lexing.from_string "5 * 6 / 2 + 7 - 1") ;
  [%expect
    {|
    Program([

    ], BinOp(Minus, BinOp(Plus, BinOp(Divide, BinOp(Multiply, Integer(5), Integer(6)), Integer(2)), Integer(7)), Integer(1)))
    |}]

let%expect_test "Binary Operation with Parentheses" =
  print_parsed_ast (Lexing.from_string "(5 * 6) / (2 + 7) - 1") ;
  [%expect
    {|
    Program([

    ], BinOp(Minus, BinOp(Divide, BinOp(Multiply, Integer(5), Integer(6)), BinOp(Plus, Integer(2), Integer(7))), Integer(1)))
    |}]
