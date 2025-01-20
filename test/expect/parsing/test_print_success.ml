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
  print_parsed_ast
    (Lexing.from_string "let x:(int,High) = 5 in securePrint(x)") ;
  [%expect
    {|
    Program([

    ], Let(x, (Int, High), Integer(5), SecurePrint([Identifier(x)])))
    |}]

let%expect_test "Print with while" =
  print_parsed_ast
    (Lexing.from_string
       "let test_var:(int,Low) = 3 in \n\
       \        while (test_var < 6) {\n\
       \            test_var := test_var + 1\n\
       \        };\n\
       \               print(test_var)\n\
       \    " ) ;
  [%expect
    {|
    Program([

    ], Let(test_var, (Int, Low), Integer(3), Seq(While(CompOp(LessThan, Identifier(test_var), Integer(6)), Assign(test_var, BinOp(Plus, Identifier(test_var), Integer(1)))), Print([Identifier(test_var)]))))
    |}]

let%expect_test "Print with while" =
  print_parsed_ast
    (Lexing.from_string
       "let x:(int,Low) = 6 in (let y:(int,Low) = 0 in while (y < 5) { \
        print(x); y := y+1 })" ) ;
  [%expect
    {|
    Program([

    ], Let(x, (Int, Low), Integer(6), Let(y, (Int, Low), Integer(0), While(CompOp(LessThan, Identifier(y), Integer(5)), Seq(Print([Identifier(x)]), Assign(y, BinOp(Plus, Identifier(y), Integer(1))))))))
    |}]
