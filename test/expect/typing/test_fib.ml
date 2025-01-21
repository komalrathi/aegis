open Core
open Print.Print_typed_ast

let%expect_test "fib" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn fib(x:(int,Low)):(int, Low) {\n\
         \    if ((x == 1)|| (x == 0)) then {\n\
         \        1\n\
         \    }\n\
         \    else {\n\
         \        fib(x - 1) + fib(x - 2)\n\
         \    }\n\
          };\n\
          fib(3)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect
        {|
    Program([
    FunctionDefn(fib, [x: (Int, Low)], (Int, Low), If(BoolOp(Or, (Bool, Low), CompOp(Equality, (Bool, Low), Identifier(x, (Int, Low)), Integer(1)), CompOp(Equality, (Bool, Low), Identifier(x, (Int, Low)), Integer(0))), Integer(1), BinOp(Plus, (Int, Low), FunctionApp(fib, (Int, Low), [BinOp(Minus, (Int, Low), Identifier(x, (Int, Low)), Integer(1))]), FunctionApp(fib, (Int, Low), [BinOp(Minus, (Int, Low), Identifier(x, (Int, Low)), Integer(2))])), (Int, Low)))
    ], FunctionApp(fib, (Int, Low), [Integer(3)]))
    |}]
