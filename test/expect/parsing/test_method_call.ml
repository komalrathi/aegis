open Core
open Print.Print_parsed_ast

let%expect_test "Method call on object" =
  print_parsed_ast
    (Lexing.from_string
       {|
        class Example {
      x: (int, Low);
      y: (bool, High);
      constructor(x: (int, Low), y: (bool, High)) { x }
      fn getX(): (int, Low) { x }
    }
obj.getX(1, 2)
  |} ) ;
  [%expect {|
    Program([
    ClassDefn(Example, FieldDefn(x, (Int, Low))
    FieldDefn(y, (Bool, High)), Constructor([x: (Int, Low); y: (Bool, High)], Identifier(x)), FunctionDefn(getX, [], (Int, Low), Identifier(x)))
    ],[

    ], MethodCall(getX, [Integer(1); Integer(2)], obj))
    |}]
