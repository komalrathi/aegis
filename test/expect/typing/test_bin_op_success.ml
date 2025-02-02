open Core
open Print.Print_typed_ast

let%expect_test "Binary Operation" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "5 * 6 / 2 + 7 - 1")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], BinOp(Minus, (Int, Low), BinOp(Plus, (Int, Low), BinOp(Divide, (Int, Low), BinOp(Multiply, (Int, Low), Integer(5), Integer(6)), Integer(2)), Integer(7)), Integer(1)))
    |}]

let%expect_test "Binary Operation with Parentheses" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "(5 * 6) / (2 + 7) - 1")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], BinOp(Minus, (Int, Low), BinOp(Divide, (Int, Low), BinOp(Multiply, (Int, Low), Integer(5), Integer(6)), BinOp(Plus, (Int, Low), Integer(2), Integer(7))), Integer(1)))
    |}]
