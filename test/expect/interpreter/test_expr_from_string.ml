open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "assign" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let test_var:(int,Low) = 72 in (test_var - 5)")
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        Function Environment:
        Result: VInt(67)
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Print Statement" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var:(int,Low) = 54 in (print (test_var - 5))" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        49
        Function Environment:
        Result: VUnit
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Print statement with sequence" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var_int:(int,Low) = 54 in (let test_var_bool:(bool,Low) \
          = True in (print (test_var_int); print (test_var_bool)))" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        54
        true
        Function Environment:
        Result: VUnit
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
