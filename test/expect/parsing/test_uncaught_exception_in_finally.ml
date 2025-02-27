open Core
open Print.Print_parsed_ast

let%expect_test "Uncaught exception in finally block" =
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
       \                y := x/y\n\
       \            }\n\
       \        }\n\
       \        catch (DivisionByZero y) {\n\
       \            x := 55;\n\
       \            print(x)\n\
       \        }\n\
       \        finally {\n\
       \            y := 101;\n\
       \            print(y);\n\
       \            let z:(int, Low) = 9 in {\n\
       \                if (z == 0) then {\n\
       \                    raise (DivisionByZero z)\n\
       \                }\n\
       \                else {\n\
       \                    z := x/z\n\
       \                }\n\
       \            }\n\
       \        }\n\
       \    }\n\
        }" ) ;
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(7), Let(y, (Int, Low), Integer(8), Seq(Assign(y, Integer(0)), Try {If(CompOp(Equality, Identifier(y), Integer(0)), Raise(DivisionByZero, y), Assign(y, BinOp(Divide, Identifier(x), Identifier(y))))} Catch (DivisionByZero y) {Seq(Assign(x, Integer(55)), Print([Identifier(x)]))} Finally {Seq(Assign(y, Integer(101)), Seq(Print([Identifier(y)]), Let(z, (Int, Low), Integer(9), If(CompOp(Equality, Identifier(z), Integer(0)), Raise(DivisionByZero, z), Assign(z, BinOp(Divide, Identifier(x), Identifier(z)))))))}))))
    |}]
