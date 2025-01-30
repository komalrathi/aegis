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
      [%expect.unreachable];
  [%expect {| Variable test_bool core type (Bool) does not match the assigned core type (Int) |}]
