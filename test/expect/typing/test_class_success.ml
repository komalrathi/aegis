open Core
open Print.Print_typed_ast

let%expect_test "Example Class" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "class Example {\n\
         \    test_var:(int, High);\n\
         \    test_bool:(bool,Low)\n\n\
         \    constructor (x:(int,Low), y:(int, Low)) {\n\
         \        test_bool := True;\n\
         \        test_var := x+y\n\
         \    }\n\
         \    High fn sum(z:(int, Low)) : (int, High) {\n\
         \        if (test_bool) then{\n\
         \            test_var := z+6\n\
         \        }\n\
         \        else\n\
         \        {\n\
         \            test_var := test_var + 6\n\
         \        }\n\
         \    };\n\
          };\n\
          let y:(int,Low) = 72 in (y - 5)" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    Program([
    ClassDefn(Example, FieldDefn(test_var, (Int, High))
    FieldDefn(test_bool, (Bool, Low)), Constructor([x: (Int, Low); y: (Int, Low)], Seq(Assign((Bool, Low), test_bool, Boolean(true)), Assign((Int, High), test_var, BinOp(Plus, (Int, Low), Identifier(x, (Int, Low)), Identifier(y, (Int, Low)))), (Int, High))), MethodDefn(High, FunctionDefn(sum, [z: (Int, Low)], (Int, High), If(Identifier(test_bool, (Bool, Low)), Assign((Int, High), test_var, BinOp(Plus, (Int, Low), Identifier(z, (Int, Low)), Integer(6))), Assign((Int, High), test_var, BinOp(Plus, (Int, High), Identifier(test_var, (Int, High)), Integer(6))), (Int, High)))))
    ],[

    ], Let(y, (Int, Low), Integer(72), BinOp(Minus, (Int, Low), Identifier(y, (Int, Low)), Integer(5)), (Int, Low)))
    |}]
