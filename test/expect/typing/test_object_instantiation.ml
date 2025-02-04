open Core
open Print.Print_typed_ast

let%expect_test "Object Instantiation" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "class Example {\n\
         \    test_var:(int, High);\n\
         \    test_bool:(bool,Low);\n\n\
         \    constructor (x:(int,Low), y:(int, Low)) {\n\
         \        test_bool:= True;\n\
         \        test_var := x+y\n\
         \    }\n\
         \    High fn sum(z:(int, Low)) : (int, High) {\n\
         \        if (test_bool) then{\n\
         \            test_var := z+6\n\
         \        }\n\
         \        else\n\
         \        {\n\
         \            test_var := test_var +6\n\
         \        }\n\
         \    }\n\
         \    High fn test() : (int, High) {\n\
         \        let a:(int,Low) = 7 in {test_var + 5 + a}\n\
         \    }\n\
          }\n\
          let y:(Example,High) = new High Example(72, True) in {y.sum(5)}" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      [%expect.unreachable] ; [%expect.unreachable];
  [%expect.unreachable];
  [%expect {| Object argument type (Int) does not match the constructor type (Bool) |}]
