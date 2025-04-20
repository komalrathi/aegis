open Core
open Print.Print_parsed_ast

let%expect_test "Function application with no arguments" =
  print_parsed_ast (Lexing.from_string "foo()") ;
  [%expect {|
    Program([

    ],[

    ], FunctionApp(foo, []))
    |}]
