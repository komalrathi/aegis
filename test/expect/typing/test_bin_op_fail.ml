open Core
open Print_typed_ast

let%expect_test "Binary Operation with Boolean" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "5 * 6 / 2 + 7 - True")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect {| binary operands type error |}]
