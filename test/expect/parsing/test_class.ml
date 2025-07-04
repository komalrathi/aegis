open Core
open Print.Print_parsed_ast

let%expect_test "Simple class definition" =
  print_parsed_ast
    (Lexing.from_string
       {|
    class Example {
      x: (int, Low);
      y: (bool, High);
      constructor(x: (int, Low), y: (bool, High)) { x }
      fn getX(): (int, Low) { x }
    }
    42
  |} ) ;
  [%expect {|
    Program([
    ClassDefn(Example, FieldDefn(x, (Int, Low))
    FieldDefn(y, (Bool, High)), Constructor([x: (Int, Low); y: (Bool, High)], Identifier(x)), FunctionDefn(getX, [], (Int, Low), Identifier(x)))
    ],[

    ], Integer(42))
    |}]
