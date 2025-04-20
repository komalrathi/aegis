open Core
open Print.Print_parsed_ast

let%expect_test "Simple let binding" =
  print_parsed_ast (Lexing.from_string "let x:(int, Low) = 42 in { x }") ;
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(42), Identifier(x)))
    |}]
