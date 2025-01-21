open Core
open Print_typed_ast

let%expect_test "Normal Print Statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,Low) = 54 in print(x)")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    var_core_type: Int
    e1_core_type: Int
    var_sec_level: Low
    e1_sec_level: Low
    pc: Low
    Program([

    ], Let(x, (Int, Low), Integer(54), Print([Identifier(x, (Int, Low))]), (Unit, Low)))
    |}]

let%expect_test "Secure Print Statement" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string "let x:(int,High) = 5 in securePrint(x)")
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    var_core_type: Int
    e1_core_type: Int
    var_sec_level: High
    e1_sec_level: Low
    pc: Low
    Program([

    ], Let(x, (Int, High), Integer(5), SecurePrint([
    Identifier(x, (Int, High))]), (Unit, High)))
    |}]

let%expect_test "Normal Print with Sequence" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var_int:(int,Low) = 54 in (let test_var_bool:(bool,Low) \
          = True in (print (test_var_int); print (test_var_bool)))" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    var_core_type: Int
    e1_core_type: Int
    var_sec_level: Low
    e1_sec_level: Low
    pc: Low
    var_core_type: Bool
    e1_core_type: Bool
    var_sec_level: Low
    e1_sec_level: Low
    pc: Low
    Program([

    ], Let(test_var_int, (Int, Low), Integer(54), Let(test_var_bool, (Bool, Low), Boolean(true), Seq(Print([Identifier(test_var_int, (Int, Low))]), Print([Identifier(test_var_bool, (Bool, Low))]), (Unit, Low)), (Unit, Low)), (Unit, Low)))
    |}]

let%expect_test "Secure Print in While Loop" =
  match
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let x:(int, High) = 3 in (\n\
         \    let y:(int, High) = 10 in (\n\
         \        while (y > x) {\n\
         \            (x := x + 1);\n\
         \            securePrint(x)\n\
         \        }\n\
         \    )\n\
          )" )
  with
  | Ok program -> print_typed_ast program
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable] ;
      [%expect.unreachable];
  [%expect {|
    var_core_type: Int
    e1_core_type: Int
    var_sec_level: High
    e1_sec_level: Low
    pc: Low
    var_core_type: Int
    e1_core_type: Int
    var_sec_level: High
    e1_sec_level: Low
    pc: Low
    Program([

    ], Let(x, (Int, High), Integer(3), Let(y, (Int, High), Integer(10), While(CompOp(GreaterThan, (Bool, High), Identifier(y, (Int, High)), Identifier(x, (Int, High))), Seq(Assign((Int, High), x, BinOp(Plus, (Int, High), Identifier(x, (Int, High)), Integer(1))), SecurePrint([
    Identifier(x, (Int, High))]), (Unit, High)), (Unit, High)), (Unit, High)), (Unit, High)))
    |}]
