open Core
open Print.Print_parsed_ast

let%expect_test "Nested let bindings" =
  print_parsed_ast
    (Lexing.from_string
       "let x:(int, Low) = 1 in { let y:(int, Low) = x + 2 in { y * 3 } }" ) ;
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(1), Let(y, (Int, Low), BinOp(Plus, Identifier(x), Integer(2)), BinOp(Multiply, Identifier(y), Integer(3)))))
    |}]
