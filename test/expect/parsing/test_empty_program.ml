open Core
open Print.Print_parsed_ast

let%expect_test "Empty program error" =
  print_parsed_ast (Lexing.from_string "") ;
  [%expect {| Line:1, position:1: Parser error |}]
