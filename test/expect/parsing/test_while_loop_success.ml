open Core
open Print.Print_parsed_ast

let%expect_test "Successful While loop parsing" =
  print_parsed_ast (Lexing.from_string "while (x < 5) { x:= x + 1 }") ;
  [%expect
    {|
    Program([

    ], While(CompOp(LessThan, Identifier(x), Integer(5)), Assign(x, BinOp(Plus, Identifier(x), Integer(1)))))
    |}]

let%expect_test "Successful While loop parsing with let statement" =
  print_parsed_ast
    (Lexing.from_string
       "let x:(int, Low) = 5 in while (x < 10) { x:= x + 1 }" ) ;
  [%expect
    {|
    Program([

    ], Let(x, (Int, Low), Integer(5), While(CompOp(LessThan, Identifier(x), Integer(10)), Assign(x, BinOp(Plus, Identifier(x), Integer(1))))))
    |}]
