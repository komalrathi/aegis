open Core
open Print.Print_typed_ast

let%expect_test "Boolean operations in While Loop" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int, Low) = 0 in {let y:(int, High) = 3 in\n\
         \    { while ((x < 5) && (y > 2)) { x := x + 1 }}}" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect
        {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(0), Let(y, (Int, High), Integer(3), While(BoolOp(And, (Bool, High), CompOp(LessThan, (Bool, Low), Identifier(x, (Int, Low)), Integer(5)), CompOp(GreaterThan, (Bool, High), Identifier(y, (Int, High)), Integer(2))), Assign((Int, Low), x, BinOp(Plus, (Int, Low), Identifier(x, (Int, Low)), Integer(1))), (Int, Low)), (Int, Low)), (Int, Low)))
    |}]
