open Core
open Print.Print_typed_ast

let%expect_test "while loop with let statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int, Low) = 5 in {while (x < 5) { x:= x + 1 }}" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(5), While(CompOp(LessThan, (Bool, Low), Identifier(x, (Int, Low)), Integer(5)), Assign((Int, Low), x, BinOp(Plus, (Int, Low), Identifier(x, (Int, Low)), Integer(1))), (Int, Low)), (Int, Low)))
    |}]
