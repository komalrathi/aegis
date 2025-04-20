open Core
open Print.Print_parsed_ast

let%expect_test "Invalid syntax: missing in" =
  print_parsed_ast (Lexing.from_string "let x:(int, Low) = 5 { x }") ;
  [%expect {| Line:1, position:23: Parser error |}]
