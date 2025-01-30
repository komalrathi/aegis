open Core
open Print.Print_parsed_ast

let%expect_test "Example Class" =
  print_parsed_ast
    (Lexing.from_string
       "class Example {\n\
       \    test_var:(int, High);\n\
       \    test_bool:(bool,Low)\n\n\
       \    constructor (x:(int,Low), y:(int, Low)) {\n\
       \        test_bool:= True;\n\
       \        test_var := x+y\n\
       \    };\n\
       \    Low fn sum(z:(int, Low)) : (int, Low) {\n\
       \        if (test_bool) then{\n\
       \            x := z+6\n\
       \        }\n\
       \        else\n\
       \        {\n\
       \            x := test_var +6\n\
       \        }\n\
       \    };\n\
        };\n\
        let y:(int,Low) = 72 in (y - 5)" ) ;
  [%expect
    {|
    Program([
    ClassDefn(Example, FieldDefn(test_var, (Int, High))
    FieldDefn(test_bool, (Bool, Low)), Constructor([x: (Int, Low); y: (Int, Low)], Assign(test_bool, Seq(Boolean(true), Assign(test_var, BinOp(Plus, Identifier(x), Identifier(y)))))), MethodDefn(Low, FunctionDefn(sum, [z: (Int, Low)], (Int, Low), If(Identifier(test_bool), Assign(x, BinOp(Plus, Identifier(z), Integer(6))), Assign(x, BinOp(Plus, Identifier(test_var), Integer(6)))))))
    ],[

    ], Let(y, (Int, Low), Integer(72), BinOp(Minus, Identifier(y), Integer(5))))
    |}]
