open Core
open Print.Print_parsed_ast

let%expect_test "Object Instantiation" =
  print_parsed_ast
    (Lexing.from_string
       "class Example {\n\
       \    test_var:(int, High);\n\
       \    test_bool:(bool,Low);\n\n\
       \    constructor (x:(int,Low), y:(int, Low)) {\n\
       \        test_bool:= True;\n\
       \        test_var := x+y\n\
       \    }\n\
       \    fn sum(z:(int, Low)) : (int, High) {\n\
       \        if (test_bool) then{\n\
       \            test_var := z+6\n\
       \        }\n\
       \        else\n\
       \        {\n\
       \            test_var := test_var +6\n\
       \        }\n\
       \    }\n\
       \    fn test() : (int, High) {\n\
       \        let a:(int,Low) = 7 in {test_var + 5 + a}\n\
       \    }\n\
        }\n\
        let obj:(Example,High) = new High Example(72, 8) in {obj.sum(9)}" ) ;
  [%expect
    {|
    Program([
    ClassDefn(Example, FieldDefn(test_var, (Int, High))
    FieldDefn(test_bool, (Bool, Low)), Constructor([x: (Int, Low); y: (Int, Low)], Seq(Assign(test_bool, Boolean(true)), Assign(test_var, BinOp(Plus, Identifier(x), Identifier(y))))), FunctionDefn(sum, [z: (Int, Low)], (Int, High), If(Identifier(test_bool), Assign(test_var, BinOp(Plus, Identifier(z), Integer(6))), Assign(test_var, BinOp(Plus, Identifier(test_var), Integer(6)))))
    FunctionDefn(test, [], (Int, High), Let(a, (Int, Low), Integer(7), BinOp(Plus, BinOp(Plus, Identifier(test_var), Integer(5)), Identifier(a)))))
    ],[

    ], Let(obj, (Object Example, High), Object(High, Example, [Integer(72); Integer(8)]), MethodCall(sum, [Integer(9)], obj)))
    |}]
