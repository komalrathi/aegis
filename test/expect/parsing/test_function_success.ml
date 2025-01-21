open Core
open Print.Print_parsed_ast

let%expect_test "Successful Low Function Parsing" =
  print_parsed_ast
    (Lexing.from_string
       "fn example (x :(int, Low), y:(int, Low)):(int, Low) { (x + y) }; \
        example(5, 6)" ) ;
  [%expect
    {|
    Program([
    FunctionDefn(example, [x: (Int, Low); y: (Int, Low)], (Int, Low), BinOp(Plus, Identifier(x), Identifier(y)))
    ], FunctionApp(example, [Integer(5); Integer(6)]))
    |}]

let%expect_test "Successful High Function Parsing" =
  print_parsed_ast
    (Lexing.from_string
       "fn example2 (x :(int, High), y:(int, High)):(int, High) { (x / y) \
        }; example2(25, 46)" ) ;
  [%expect
    {|
    Program([
    FunctionDefn(example2, [x: (Int, High); y: (Int, High)], (Int, High), BinOp(Divide, Identifier(x), Identifier(y)))
    ], FunctionApp(example2, [Integer(25); Integer(46)]))
    |}]

let%expect_test "Successful High and Low Function Parsing" =
  print_parsed_ast
    (Lexing.from_string
       "fn example3 (x :(int, Low), y:(int, Low)):(int, High) { (x * y) }; \
        example3(5, 6)" ) ;
  [%expect
    {|
     Program([
     FunctionDefn(example3, [x: (Int, Low); y: (Int, Low)], (Int, High), BinOp(Multiply, Identifier(x), Identifier(y)))
     ], FunctionApp(example3, [Integer(5); Integer(6)]))
     |}]
