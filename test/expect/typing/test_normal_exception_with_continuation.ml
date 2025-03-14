open Core
open Print.Print_typed_ast

let%expect_test "Normal Exception with Continuation" =
  match
    Parser_frontend.Parse_program.parse_program
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
          }" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| A non-resumable exception was raised but a continuation was provided. |}]
