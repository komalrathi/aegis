open Core
open Print.Print_parsed_ast

let%expect_test "Boolean operations in While Loop" =
  print_parsed_ast
    (Lexing.from_string
       "let x:(int, Low) = 0 in {let y:(int, High) = 3 in\n\
       \    { while ((x < 5) && (y > 2)) { x := x + 1 }}}" ) ;
  [%expect
    {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(0), Let(y, (Int, High), Integer(3), While(BoolOp(And, CompOp(LessThan, Identifier(x), Integer(5)), CompOp(GreaterThan, Identifier(y), Integer(2))), Assign(x, BinOp(Plus, Identifier(x), Integer(1)))))))
    |}]
