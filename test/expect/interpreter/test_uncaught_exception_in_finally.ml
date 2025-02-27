open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "Uncaught exception in finally block" =
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
         \                y := x/y\n\
         \            }\n\
         \        }\n\
         \        catch (DivisionByZero y) {\n\
         \            x := 55;\n\
         \            print(x)\n\
         \        }\n\
         \        finally {\n\
         \            y := 101;\n\
         \            print(y);\n\
         \            let z:(int, Low) = 9 in {\n\
         \                if (z == 0) then {\n\
         \                    raise (DivisionByZero z)\n\
         \                }\n\
         \                else {\n\
         \                    z := x/z\n\
         \                }\n\
         \            }\n\
         \        }\n\
         \    }\n\
          }" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect {| Error: could not type program |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
