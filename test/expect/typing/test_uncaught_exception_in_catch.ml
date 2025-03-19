open Core
open Print.Print_typed_ast

let%expect_test "Uncaught exception in catch block" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int, Low) = 7 in {\n\
         \  let y:(int, Low) = 8 in {\n\
         \      y := 0;\n\
         \      try {\n\
         \          if (y == 0) then {\n\
         \              raise (DivisionByZero y)\n\
         \          }\n\
         \          else {\n\
         \              y := x/y\n\
         \          }\n\
         \      }\n\
         \      catch (DivisionByZero y) {\n\
         \          x := 55;\n\
         \          let z:(int, Low) = 9 in {\n\
         \              if (z == 0) then {\n\
         \                  raise (DivisionByZero z)\n\
         \              }\n\
         \              else {\n\
         \                  z := x/z\n\
         \              }\n\
         \          }\n\
         \      }\n\
         \      finally {\n\
         \          y := 101;\n\
         \          print(y)\n\
         \      }\n\
         \  }\n\
          }" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {| The catch block has raised new exceptions that were not raised in the try block. This is not permitted. |}]
