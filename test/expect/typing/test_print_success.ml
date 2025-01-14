open Core
open Print_typed_ast

let%expect_test "Normal Print Statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,Low) = 54 in print(x)")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect
        {|
    Program([

    ], Let(x, (Int, Low), Integer(54), Print([Identifier(x, (Int, Low))]), (Unit, Low)))
    |}]

let%expect_test "Secure Print Statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,High) = 5 in securePrint(x)")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect
        {|
    Program([

    ], Let(x, (Int, High), Integer(5), SecurePrint([
    Identifier(x, (Int, High))]), (Unit, High)))
    |}]
