open Core
open Print.Print_typed_ast

let%expect_test "Resumable Exception with DivisionByZero" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let numerator : (int, Low) = 10 in {\n\
         \      let denominator : (int, Low) = 0 in {\n\
         \        try {\n\
         \            if (denominator == 0) then {\n\
         \                raise!(DivisionByZero denominator)\n\
         \            } \n\
         \            else {\n\
         \                numerator / denominator\n\
         \            }\n\
         \        } \n\
         \        catch (DivisionByZero ex k) {\n\
         \            print(numerator);\n\
         \            continue (k, 5)\n\
         \        }\n\
         \        finally {\n\
         \            numerator := 49;\n\
         \            print(numerator)\n\
         \        }\n\
         \      }\n\
         \    }" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([

    ],[

    ], Let(numerator, (Int, Low), Integer(10), Let(denominator, (Int, Low), Integer(0), Try {If(CompOp(Equality, (Bool, Low), Identifier(denominator, (Int, Low)), Integer(0)), ResumableRaise(DivisionByZero, denominator) (Exception DivisionByZero, Low), BinOp(Divide, (Int, Low), Identifier(numerator, (Int, Low)), Identifier(denominator, (Int, Low))), (Exception DivisionByZero, Low))} Catch (DivisionByZero ex k) {Seq(Print([Identifier(numerator, (Int, Low))]), Continue (k, Integer(5), (Unit, Low)), (Unit, Low))} Finally {Seq(Assign((Int, Low), numerator, Integer(49)), Print([Identifier(numerator, (Int, Low))]), (Unit, Low))} (Unit, Low), (Unit, Low)), (Unit, Low)))
    |}]
