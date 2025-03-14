open Core
open Print.Print_parsed_ast

let%expect_test "Resumable Exception with no continuation" =
  print_parsed_ast
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
       \        catch (DivisionByZero ex) {\n\
       \            print(numerator);\n\
       \            continue (k, 5)\n\
       \        }\n\
       \        finally {\n\
       \            numerator := 49;\n\
       \            print(numerator)\n\
       \        }\n\
       \      }\n\
       \    }" ) ;
  [%expect {|
    Program([

    ],[

    ], Let(numerator, (Int, Low), Integer(10), Let(denominator, (Int, Low), Integer(0), Try {If(CompOp(Equality, Identifier(denominator), Integer(0)), ResumableRaise(DivisionByZero, denominator), BinOp(Divide, Identifier(numerator), Identifier(denominator)))} Catch (DivisionByZero ex) {Seq(Print([Identifier(numerator)]), Continue (k, Integer(5)))} Finally {Seq(Assign(numerator, Integer(49)), Print([Identifier(numerator)]))})))
    |}]
