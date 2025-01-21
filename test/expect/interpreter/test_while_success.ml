open Core
open Print.Print_interpret_expr
open Typing.Typed_ast
open Typing.Type_program
open Compiler_types.Ast_types
open Compiler_types.Language_types

(* does not terminate *)
let%expect_test "While Loop from String" =
  let parsed_program =
    Parser_frontend.Parse_program.parse_program
      (Lexing.from_string
         "let test_var:(int,Low) = 5 in while (test_var < 6) {test_var := \
          test_var + 1}" )
  in
  match parsed_program with
  | Ok program ->
      ( match type_program program with
      | Ok (Prog (_, expr)) -> print_interpret_expr expr [] []
      | Error _ -> print_endline "Error: could not type program" ) ;
      [%expect
        {|
        Function Environment:
        Result: VUnit
        Value Environment: []
        |}]
  | Error _ ->
      print_endline "Error: could not parse program" ;
      [%expect.unreachable]

let%expect_test "Let and While Loop" =
  (let expr =
     Let
       ( Lexing.dummy_pos
       , "test_var"
       , (TEInt, TSLow)
       , Integer (Lexing.dummy_pos, 5, TSLow)
       , While
           ( Lexing.dummy_pos
           , CompOp
               ( Lexing.dummy_pos
               , (TEInt, TSLow)
               , CompOpLessThan
               , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "test_var")
               , Integer (Lexing.dummy_pos, 6, TSLow) )
           , Assign
               ( Lexing.dummy_pos
               , (TEInt, TSLow)
               , "test_var"
               , BinOp
                   ( Lexing.dummy_pos
                   , (TEInt, TSLow)
                   , BinOpPlus
                   , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "test_var")
                   , Integer (Lexing.dummy_pos, 1, TSLow) ) )
           , (TEInt, TSLow) )
       , (TEInt, TSLow) )
   in
   print_interpret_expr expr [] [] )
  <<<<<<< HEAD
            [%expect
              {|
=======
  [%expect
    {|
>>>>>>> main
    Function Environment:
    Result: VUnit
    Value Environment: []
    |}]

let%expect_test "While Loop" =
  let env = [("test_var", VInt 5)] in
  let expr =
    While
      ( Lexing.dummy_pos
      , CompOp
          ( Lexing.dummy_pos
          , (TEInt, TSLow)
          , CompOpLessThan
          , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "test_var")
          , Integer (Lexing.dummy_pos, 6, TSLow) )
      , Assign
          ( Lexing.dummy_pos
          , (TEInt, TSLow)
          , "test_var"
          , BinOp
              ( Lexing.dummy_pos
              , (TEInt, TSLow)
              , BinOpPlus
              , Identifier (Lexing.dummy_pos, (TEInt, TSLow), "test_var")
              , Integer (Lexing.dummy_pos, 1, TSLow) ) )
      , (TEInt, TSLow) )
  in
  print_interpret_expr expr env [] ;
  [%expect
    {|
    Function Environment:
    Result: VUnit
    Value Environment: [test_var -> VInt(6); ]
    |}]
