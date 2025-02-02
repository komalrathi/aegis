open Core
open Print.Print_typed_ast

let%expect_test "Successful Low Function Parsing" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn example (x :(int, Low), y:(int, Low)):(int, Low) { (x + y) } \
          example(5, 6)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([

    ],[
    FunctionDefn(example, [x: (Int, Low); y: (Int, Low)], (Int, Low), BinOp(Plus, (Int, Low), Identifier(x, (Int, Low)), Identifier(y, (Int, Low))))
    ], FunctionApp(example, (Int, Low), [Integer(5); Integer(6)]))
    |}]

let%expect_test "Successful High Function Parsing" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn example (x :(bool, High), y:(bool, High)):(bool, High) { (x && \
          y) } example(True, False)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([

    ],[
    FunctionDefn(example, [x: (Bool, High); y: (Bool, High)], (Bool, High), BoolOp(And, (Bool, High), Identifier(x, (Bool, High)), Identifier(y, (Bool, High))))
    ], FunctionApp(example, (Bool, High), [Boolean(true); Boolean(false)]))
    |}]
