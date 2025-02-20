open Core
open Print.Print_typed_ast

let%expect_test "Normal Print Statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int, Low) = 7 in {\n\
         \    let y:(int, Low) = 8 in {\n\
         \        y := 0;\n\
         \        try {\n\
         \            if (y == 0) then {\n\
         \                raise (DivisionByZero y)\n\
         \            }\n\
         \            else {\n\
         \                x := x/y\n\
         \            }\n\
         \        }\n\
         \        catch (DivisionByZero y) {\n\
         \            print(y)\n\
         \        }\n\
         \        finally {\n\
         \            y := 101;\n\
         \            print(y)\n\
         \        }\n\
         \    }\n\
          }" )
  with
  | Ok program -> print_typed_ast program
  | Error _ -> print_endline "Error: could not parse program";
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(7), Let(y, (Int, Low), Integer(8), Seq(Assign((Int, Low), y, Integer(0)), Try {If(CompOp(Equality, (Bool, Low), Identifier(y, (Int, Low)), Integer(0)), Raise(DivisionByZero, y) (Exception DivisionByZero, Low), Assign((Int, Low), x, BinOp(Divide, (Int, Low), Identifier(x, (Int, Low)), Identifier(y, (Int, Low)))), (Exception DivisionByZero, Low))} Catch (DivisionByZero y) {Print([Identifier(y, (Exception DivisionByZero, Low))])} Finally Seq(Assign((Int, Low), y, Integer(101)), Print([Identifier(y, (Int, Low))]), (Unit, Low)) {(Unit, Low)}, (Unit, Low)), (Unit, Low)), (Unit, Low)))
    |}]
