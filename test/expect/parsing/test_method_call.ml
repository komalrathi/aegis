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
let obj:(Example, Low) = new Low Example(1, True) in {obj.getX()}
  |} ) ;
  [%expect
    {|
    Program([
    ClassDefn(Example, FieldDefn(x, (Int, Low))
    FieldDefn(y, (Bool, High)), Constructor([x: (Int, Low); y: (Bool, High)], Identifier(x)), FunctionDefn(getX, [], (Int, Low), Identifier(x)))
    ],[

    ], Let(obj, (Object Example, Low), Object(Low, Example, [Integer(1); Boolean(true)]), MethodCall(getX, [], obj)))
    |}]
