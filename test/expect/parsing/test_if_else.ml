open Core
open Print.Print_parsed_ast

let%expect_test "If-else statement" =
  print_parsed_ast
    (Lexing.from_string "if (x < 5) then { y := 1 } else { y := 2 }") ;
  [%expect
    {|
    Program([

    ],[

    ], If(CompOp(LessThan, Identifier(x), Integer(5)), Assign(y, Integer(1)), Assign(y, Integer(2))))
    |}]

let%expect_test "Unary operation (not)" =
  print_parsed_ast
    (Lexing.from_string "if (!x) then { y := 1 } else { y := 2 }") ;
  [%expect
    {|
    Program([

    ],[

    ], If(UnaryOp(Not, Identifier(x)), Assign(y, Integer(1)), Assign(y, Integer(2))))
    |}]
