open Core
open Print.Print_typed_ast

let%expect_test "Resumable Exception with no continuation" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let numerator : (int, Low) = 10 in {\n\
         \      let denominator : (int, Low) = 0 in {\n\
         \        try {\n\
         \            if (denominator == 0) then {\n\
         \                raise!(DivisionByZero denominator)\n\
         \            } \n\
         \            else {\n\
         \                numerator / denominator\n\
         \            }\n\
         \        } \n\
         \        catch (DivisionByZero ex) {\n\
         \            print(numerator);\n\
         \            continue (k, 5)\n\
         \        }\n\
         \        finally {\n\
         \            numerator := 49;\n\
         \            print(numerator)\n\
         \        }\n\
         \      }\n\
         \    }" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| Continuation function k does not exist |}]
