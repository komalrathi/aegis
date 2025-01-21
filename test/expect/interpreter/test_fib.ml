open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program

let%expect_test "fib" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "fn fib(x:(int,Low)):(int, Low) {\n\
         \    if ((x == 1)|| (x == 0)) then {\n\
         \        1\n\
         \    }\n\
         \    else {\n\
         \        fib(x - 1) + fib(x - 2)\n\
         \    }\n\
          };\n\
          fib(3)" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, _, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        FunctionApp: fib
        Function Environment:
        Error: Function not found
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]
