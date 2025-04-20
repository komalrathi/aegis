open Core
open Print.Print_typed_ast

let%expect_test "Stack Class with wrong object security level" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         {|
  class Stack {
    top: (int, High);
    constructor(top: (int, High)) { top }
    fn push(x: (int, High)): (int, High) { top := x }
    fn pop(): (int, Low) { top }
  }
  let newStack: (int, Low) = 0 in { newStack.push(1); newStack.pop() }
|} )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| The function security level return type does not match the function body security level type |}]

let%expect_test "Stack Class with wrong method security level" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         {|
  class Stack {
    top: (int, Low);
    constructor(top: (int, Low)) { top }
    fn push(x: (int, High)): (int, High) { top := x }
    fn pop(): (int, High) { top }
  }
  let newStack: (int, Low) = 0 in { newStack.push(1); newStack.pop() }
|} )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| Variable top security level type does not match the assigned security level type |}]
