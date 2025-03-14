open Core
open Print.Print_parsed_ast

let%expect_test "Normal Exception with Continuation" =
  print_parsed_ast
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
       \        catch (DivisionByZero y k) {\n\
       \            print(y);\n\
       \            continue (k, 5)  \n\
       \               }\n\
       \        finally {\n\
       \            y := 101;\n\
       \            print(y)\n\
       \        }\n\
       \    }\n\
        }" ) ;
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(7), Let(y, (Int, Low), Integer(8), Seq(Assign(y, Integer(0)), Try {If(CompOp(Equality, Identifier(y), Integer(0)), Raise(DivisionByZero, y), Assign(x, BinOp(Divide, Identifier(x), Identifier(y))))} Catch (DivisionByZero y k) {Seq(Print([Identifier(y)]), Continue (k, Integer(5)))} Finally {Seq(Assign(y, Integer(101)), Print([Identifier(y)]))} ))))
    |}]
