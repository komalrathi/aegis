open Core
open Print.Print_typed_ast

let%expect_test "Bad Low Function Parsing" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn example (x :(int, Low), y:(int, High)):(int, Low) { (x + y) } \
          example(5, 6)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| The function security level return type does not match the function body security level type |}]

let%expect_test "Bad High Function Parsing" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn example (x :(bool, High), y:(bool, High)):(bool, High) { (x && \
          y) } example(3, 4)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| Function argument type (Bool) does not match the function type (Int) |}]
