open Core
open Print.Print_typed_ast

let%expect_test "Low Security Level Variable Creation using Let" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let test_var:(int,Low) = 5 in {test_var + 2}")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(test_var, (Int, Low), Integer(5), BinOp(Plus, (Int, Low), Identifier(test_var, (Int, Low)), Integer(2)), (Int, Low)))
    |}]

let%expect_test "2 Low Security Level Variables Creation using Let" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var:(int,Low) = 5 in {let test_var_2:(int,Low) = 6 in \
          {test_var + test_var_2}}" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(test_var, (Int, Low), Integer(5), Let(test_var_2, (Int, Low), Integer(6), BinOp(Plus, (Int, Low), Identifier(test_var, (Int, Low)), Identifier(test_var_2, (Int, Low))), (Int, Low)), (Int, Low)))
    |}]

let%expect_test "High Security Level Variable Creation using Let" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let test_var:(int,High) = 34 in {test_var + 2}")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(test_var, (Int, High), Integer(34), BinOp(Plus, (Int, High), Identifier(test_var, (Int, High)), Integer(2)), (Int, High)))
    |}]

let%expect_test "1 High 1 Low Security Level Variables Creation using Let" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var:(int,High) = 5 in {let test_var_2:(int,Low) = 6 in \
          {test_var + test_var_2}}" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(test_var, (Int, High), Integer(5), Let(test_var_2, (Int, Low), Integer(6), BinOp(Plus, (Int, High), Identifier(test_var, (Int, High)), Identifier(test_var_2, (Int, Low))), (Int, High)), (Int, High)))
    |}]
