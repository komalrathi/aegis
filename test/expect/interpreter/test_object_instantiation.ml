open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "Object Instantiation" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
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
          let obj:(Example,High) = new High Example(72, 8) in {obj.sum(9)}" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {| Error: Class Example not found |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
