open Core
open Print.Print_typed_ast

let%expect_test "Variable Shadowing" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int,Low) = 5 in let x :(int,Low) = 6 in x + x" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect {| Variable has already been assigned and has another type |}]
