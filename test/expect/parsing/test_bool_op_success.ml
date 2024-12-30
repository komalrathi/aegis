open Core
open Print_parsed_ast

let%expect_test "Boolean Operation" =
  print_parsed_ast (Lexing.from_string "True && False || !True && False") ;
  [%expect
    {|
    Program([

    ], BoolOp(And, BoolOp(Or, BoolOp(And, Boolean(true), Boolean(false)), UnaryOp(Not, Boolean(true))), Boolean(false)))
    |}]

let%expect_test "Boolean Operation with Parentheses" =
  print_parsed_ast (Lexing.from_string "(True && False) || (!True && False)") ;
  [%expect
    {|
    Program([

    ], BoolOp(Or, BoolOp(And, Boolean(true), Boolean(false)), BoolOp(And, UnaryOp(Not, Boolean(true)), Boolean(false))))
    |}]
