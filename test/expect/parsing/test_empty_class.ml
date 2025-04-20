open Core
open Print.Print_parsed_ast

let%expect_test "Class with no fields or methods" =
  print_parsed_ast
    (Lexing.from_string
       {|
    class Empty {
      constructor() { 0 }
    }
    1
  |} ) ;
  [%expect
    {|
    Program([
    ClassDefn(Empty, , Constructor([], Integer(0)), )
    ],[

    ], Integer(1))
    |}]
