open Core
open Print.Print_parsed_ast

let%expect_test "first for loop" =
  print_parsed_ast
    (Lexing.from_string
       "for (let x : (int, High) = 2; (x<6); (x:=x+1)) {if (x>4) then \
        {x:=x+1} else {x:=x+2}}" ) ;
  [%expect
    {|
    Program([

    ],[

    ], Let(x, (Int, High), Integer(2), While(CompOp(LessThan, Identifier(x), Integer(6)), Seq(If(CompOp(GreaterThan, Identifier(x), Integer(4)), Assign(x, BinOp(Plus, Identifier(x), Integer(1))), Assign(x, BinOp(Plus, Identifier(x), Integer(2)))), Assign(x, BinOp(Plus, Identifier(x), Integer(1)))))))
    |}]

let%expect_test "second for loop" =
  print_parsed_ast
    (Lexing.from_string
       "for (let x : (int, Low) = 10; (x>1); (x:=(x/2))) {if (x<4) then \
        {y:=(x*4)} else {y:=(x*2)}}" ) ;
  [%expect
    {|
    Program([

    ],[

    ], Let(x, (Int, Low), Integer(10), While(CompOp(GreaterThan, Identifier(x), Integer(1)), Seq(If(CompOp(LessThan, Identifier(x), Integer(4)), Assign(y, BinOp(Multiply, Identifier(x), Integer(4))), Assign(y, BinOp(Multiply, Identifier(x), Integer(2)))), Assign(x, BinOp(Divide, Identifier(x), Integer(2)))))))
    |}]
