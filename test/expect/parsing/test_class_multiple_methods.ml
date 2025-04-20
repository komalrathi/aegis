open Core
open Print.Print_parsed_ast

let%expect_test "Counter Class" =
  print_parsed_ast
    (Lexing.from_string
       {|
    class Counter {
      value: (int, Low);
      constructor(value: (int, Low)) { value }
      fn inc(): (int, Low) { value := value + 1 }
      fn get(): (int, Low) { value }
    }
    0
  |} ) ;
  [%expect
    {|
    Program([
    ClassDefn(Counter, FieldDefn(value, (Int, Low)), Constructor([value: (Int, Low)], Identifier(value)), FunctionDefn(inc, [], (Int, Low), Assign(value, BinOp(Plus, Identifier(value), Integer(1))))
    FunctionDefn(get, [], (Int, Low), Identifier(value)))
    ],[

    ], Integer(0))
    |}]

let%expect_test "Object creation and method call" =
  print_parsed_ast
    (Lexing.from_string
       {|
      class Counter {
        value: (int, Low);
        constructor(value: (int, Low)) { value }
        fn inc(): (int, Low) { value := value + 1 }
        fn get(): (int, Low) { value }
      }

        let c: (Counter, Low) = new Low Counter(0) in {
          c.inc();
          c.get()
        }
      |} ) ;
  [%expect
    {|
    Program([
    ClassDefn(Counter, FieldDefn(value, (Int, Low)), Constructor([value: (Int, Low)], Identifier(value)), FunctionDefn(inc, [], (Int, Low), Assign(value, BinOp(Plus, Identifier(value), Integer(1))))
    FunctionDefn(get, [], (Int, Low), Identifier(value)))
    ],[

    ], Let(c, (Object Counter, Low), Object(Low, Counter, [Integer(0)]), Seq(MethodCall(inc, [], c), MethodCall(get, [], c))))
    |}]
