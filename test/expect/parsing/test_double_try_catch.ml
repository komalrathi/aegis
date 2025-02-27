open Core
open Print.Print_parsed_ast

let%expect_test "2 TryCatchFinally Blocks" =
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
       \            let z: (int, Low)= 7 in {\n\
       \                try {\n\
       \                    z := 10\n\
       \                }\n\
       \                catch (IntegerOverflow z) {\n\
       \                    print(z)\n\
       \                }\n\
       \                finally {\n\
       \                    x := 105;\n\
       \                    print(x)\n\
       \                }\n\
       \            }\n\
       \        }\n\
       \        finally {\n\
       \            y := 101;\n\
       \            print(y)\n\
       \        }\n\
       \    }\n\
        }" ) ;
  [%expect {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(7), Let(y, (Int, Low), Integer(8), Seq(Assign(y, Integer(0)), Try {If(CompOp(Equality, Identifier(y), Integer(0)), Raise(DivisionByZero, y), Assign(y, BinOp(Divide, Identifier(x), Identifier(y))))} Catch (DivisionByZero y) {Let(z, (Int, Low), Integer(7), Try {Assign(z, Integer(10))} Catch (IntegerOverflow z) {Print([Identifier(z)])} Finally {Seq(Assign(x, Integer(105)), Print([Identifier(x)]))})} Finally {Seq(Assign(y, Integer(101)), Print([Identifier(y)]))}))))
    |}]
