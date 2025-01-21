open Core
open Print.Print_parsed_ast

let%expect_test "fib" =
  print_parsed_ast
    (Lexing.from_string
       "fn fib(x:(int,Low)):(int, Low) {\n\
       \    if ((x == 1)|| (x == 0)) then {\n\
       \        1\n\
       \    }\n\
       \    else {\n\
       \        fib(x - 1) + fib(x - 2)\n\
       \    }\n\
        };\n\
        fib(3)" ) ;
  [%expect
    {|
    Program([
    FunctionDefn(fib, [x: (Int, Low)], (Int, Low), If(BoolOp(Or, CompOp(Equality, Identifier(x), Integer(1)), CompOp(Equality, Identifier(x), Integer(0))), Integer(1), BinOp(Plus, FunctionApp(fib, [BinOp(Minus, Identifier(x), Integer(1))]), FunctionApp(fib, [BinOp(Minus, Identifier(x), Integer(2))]))))
    ], FunctionApp(fib, [Integer(3)]))
    |}]
