open Core
open Print.Print_typed_ast

let%expect_test "Normal Print Statement with High Security Level Variable" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,High) = 54 in print(x)")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect {| Cannot print high security level data using print |}]
