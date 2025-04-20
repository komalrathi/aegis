open Core
open Print.Print_typed_ast

let%expect_test "Method call on object with incompatible security level" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         {|
        class Example {
      x: (int, Low);
      y: (bool, High);
      constructor(x: (int, Low), y: (bool, High)) { x }
      fn getX(): (int, Low) { x }
    }
let obj:(Example, Low) = new Low Example(1, True) in {obj.getX()}
  |} )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect
        {| Object argument security level does not match the constructor security level |}]
