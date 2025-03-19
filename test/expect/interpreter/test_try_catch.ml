open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "DivisionByZero exception raised" =
  let parsed_program =
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
         \        catch (DivisionByZero y) {\n\
         \            print(y)\n\
         \        }\n\
         \        finally {\n\
         \            y := 101;\n\
         \            print(y)\n\
         \        }\n\
         \    }\n\
          }" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {|
        101
        Error: DivisionByZero
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
