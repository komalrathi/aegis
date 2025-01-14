open Core
open Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "Normal Print Statement" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,Low) = 54 in print(x)")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        54
        Function Environment:
        Result: VUnit
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Secure Print Statement" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int,High) = 5 in (let y:(int,Low) = 67 in securePrint(x, y))" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        5
        67
        Function Environment:
        Result: VUnit
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
