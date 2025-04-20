open Core
open Print.Print_parsed_ast

let%expect_test "Boolean operations in While Loop" =
  print_parsed_ast
    (Lexing.from_string "while ((x < 5) && (y > 2)) { x := x + 1 }") ;
  [%expect
    {|
    Program([

    ],[

    ], While(BoolOp(And, CompOp(LessThan, Identifier(x), Integer(5)), CompOp(GreaterThan, Identifier(y), Integer(2))), Assign(x, BinOp(Plus, Identifier(x), Integer(1)))))
    |}]
