open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "Binary Operation" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "5 * 6 / 2 + 7 - 1")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        Result: VInt(21)
        Value Environment:
        Function Environment:
        Class Environment:
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Binary Operation with Parentheses" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "(5 * 6) / (2 + 7) - 1")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        Result: VInt(2)
        Value Environment:
        Function Environment:
        Class Environment:
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Binary Operation with Exponentiation" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "5 ^ 6 / 8")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        Result: VInt(1953)
        Value Environment:
        Function Environment:
        Class Environment:
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Binary Operation with Modulus" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "(525 % 6) / 2")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        Result: VInt(1)
        Value Environment:
        Function Environment:
        Class Environment:
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
