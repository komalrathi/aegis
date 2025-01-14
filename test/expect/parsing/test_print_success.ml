open Core
open Print_parsed_ast

let%expect_test "Normal Print Statement" =
  print_parsed_ast (Lexing.from_string "let x:(int,Low) = 54 in print(x)") ;
  [%expect
    {|
    Program([

    ], Let(x, (Int, Low), Integer(54), Print([Identifier(x)])))
    |}]

let%expect_test "Secure Print Statement" =
  print_parsed_ast (Lexing.from_string "let x:(int,High) = 5 in securePrint(x)") ;
  [%expect
    {|
    Program([

    ], Let(x, (Int, High), Integer(5), SecurePrint([Identifier(x)])))
    |}]