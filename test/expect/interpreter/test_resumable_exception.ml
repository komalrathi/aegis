open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "Resumable Exception with DivisionByZero" =
  let parsed_program =
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
         \        catch (DivisionByZero denominator k) {\n\
         \            print(numerator);\n\
         \            continue (k, 5)\n\
         \        }\n\
         \        finally {\n\
         \            numerator := 49;\n\
         \            print(numerator)\n\
         \        }\n\
         \      }\n\
         \    }" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        10
        49
        Result: VInt(5)
        Value Environment:
        Function Environment:
        Class Environment:
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
