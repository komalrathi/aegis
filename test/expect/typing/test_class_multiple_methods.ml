open Core
open Print.Print_typed_ast

let%expect_test "Counter Class" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         {|
    class Counter {
      value: (int, Low);
      constructor(value: (int, Low)) { value }
      fn inc(): (int, Low) { value := value + 1 }
      fn get(): (int, Low) { value }
    }
    0
  |} )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([
    ClassDefn(Counter, FieldDefn(value, (Int, Low)), Constructor([value: (Int, Low)], Identifier(value, (Int, Low))), FunctionDefn(inc, [], (Int, Low), Assign((Int, Low), value, BinOp(Plus, (Int, Low), Identifier(value, (Int, Low)), Integer(1))))
    FunctionDefn(get, [], (Int, Low), Identifier(value, (Int, Low))))
    ],[

    ], Integer(0))
    |}]

let%expect_test "Object creation and method call" =
  match
    Parser_frontend.Parse_program.parse_program
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
       |} )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([
    ClassDefn(Counter, FieldDefn(value, (Int, Low)), Constructor([value: (Int, Low)], Identifier(value, (Int, Low))), FunctionDefn(inc, [], (Int, Low), Assign((Int, Low), value, BinOp(Plus, (Int, Low), Identifier(value, (Int, Low)), Integer(1))))
    FunctionDefn(get, [], (Int, Low), Identifier(value, (Int, Low))))
    ],[

    ], Let(c, (Object Counter, Low), Object(Low, Counter, [Integer(0)], (Object Counter, Low)), Seq(MethodCall(c, inc, (Unit, Low), []), MethodCall(c, get, (Unit, Low), []), (Int, Low)), (Int, Low)))
    |}]
