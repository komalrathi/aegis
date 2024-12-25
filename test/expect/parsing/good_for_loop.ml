open Core
open Print_parsed_ast

let%expect_test "1 argument for range" =
  print_parsed_ast (Lexing.from_string "for x in range(10) do {x + 6}") ;
  [%expect
    {|
    Program([

    ], For(Identifier(x), [Integer(10)], BinOp(Plus, Identifier(x), Integer(6))))
    |}]

let%expect_test "2 arguments for range" =
  print_parsed_ast (Lexing.from_string "for x in range(0, 10) do {x + 6}") ;
  [%expect
    {|
    Program([

    ], For(Identifier(x), [Integer(0); Integer(10)], BinOp(Plus, Identifier(x), Integer(6))))
    |}]

let%expect_test "3 arguments for range" =
  print_parsed_ast (Lexing.from_string "for x in range(0, 10, 2) do {x + 6}") ;
  [%expect
    {|
    Program([

    ], For(Identifier(x), [Integer(0); Integer(10); Integer(2)], BinOp(Plus, Identifier(x), Integer(6))))
    |}]
