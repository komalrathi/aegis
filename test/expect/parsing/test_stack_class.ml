open Core
open Print.Print_parsed_ast

let%expect_test "Stack Class" =
  print_parsed_ast
    (Lexing.from_string
       {|
    class Stack {
      top: (int, Low);
      constructor(top: (int, Low)) { top }
      fn push(x: (int, Low)): (int, Low) { top := x }
      fn pop(): (int, Low) { top }
    }
    let newStack: (int, Low) = 0 in { newStack.push(1); newStack.pop() }
  |} ) ;
  [%expect
    {|
    Program([
    ClassDefn(Stack, FieldDefn(top, (Int, Low)), Constructor([top: (Int, Low)], Identifier(top)), FunctionDefn(push, [x: (Int, Low)], (Int, Low), Assign(top, Identifier(x)))
    FunctionDefn(pop, [], (Int, Low), Identifier(top)))
    ],[

    ], Let(newStack, (Int, Low), Integer(0), Seq(MethodCall(push, [Integer(1)], newStack), MethodCall(pop, [], newStack))))
    |}]
