open Parser_frontend.Lexer
open Parser_frontend.Parser
open Core

let pp_print_token fmt token =
  Format.fprintf fmt "%s"
    ( match token with
    | LEFT_PAREN -> "LEFT_PAREN"
    | RIGHT_PAREN -> "RIGHT_PAREN"
    | LEFT_BRACE -> "LEFT_BRACE"
    | RIGHT_BRACE -> "RIGHT_BRACE"
    | COMMA -> "COMMA"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MULTIPLY -> "MULTIPLY"
    | DIVIDE -> "DIVIDE"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | LT -> "LT"
    | GT -> "GT"
    | LTE -> "LTE"
    | GTE -> "GTE"
    | EQUALITY -> "EQUALITY"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | ASSIGN -> "ASSIGN"
    | CLASSIFY -> "CLASSIFY"
    | DECLASSIFY -> "DECLASSIFY"
    | IF -> "IF"
    | THEN -> "THEN"
    | ELSE -> "ELSE"
    | LET -> "LET"
    | WHILE -> "WHILE"
    | FOR -> "FOR"
    | COLON -> "COLON"
    | TYPE_INT -> "TYPE_INT"
    | TYPE_BOOL -> "TYPE_BOOL"
    | EQUAL -> "EQUAL"
    | IN -> "IN"
    | HIGH_SEC_LEVEL -> "HIGH_SEC_LEVEL"
    | LOW_SEC_LEVEL -> "LOW_SEC_LEVEL"
    | FN -> "FN"
    | SEMICOLON -> "SEMICOLON"
    | INT i -> Printf.sprintf "INT(%d)" i
    | IDENTIFIER id -> Printf.sprintf "IDENTIFIER(%s)" id
    | PRINT -> "PRINT"
    | SECUREPRINT -> "SECUREPRINT"
    | CLASS -> "CLASS"
    | CONSTRUCTOR -> "CONSTRUCTOR"
    | NEW -> "NEW"
    | DOT -> "DOT"
    | EOF -> "EOF" )

let parser_token_testable = Alcotest.testable pp_print_token Stdlib.( = )

let test_illegal_character () =
  Alcotest.check_raises "Syntax Error" (SyntaxError "Invalid character ?")
    (fun () -> ignore (read_token (Lexing.from_string "?") : token) )

let test_lex_token (input_str, expected_token) =
  Alcotest.(check parser_token_testable)
    ("Token for input: " ^ input_str)
    expected_token
    (read_token (Lexing.from_string input_str))

(* Define specific test cases *)
let lex_token_test_cases =
  [ ("(", LEFT_PAREN)
  ; (")", RIGHT_PAREN)
  ; ("{", LEFT_BRACE)
  ; ("}", RIGHT_BRACE)
  ; (",", COMMA)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", MULTIPLY)
  ; ("/", DIVIDE)
  ; ("True", TRUE)
  ; ("False", FALSE)
  ; ("<", LT)
  ; (">", GT)
  ; ("<=", LTE)
  ; (">=", GTE)
  ; ("&&", AND)
  ; ("||", OR)
  ; ("!", NOT)
  ; (":=", ASSIGN)
  ; ("classify", CLASSIFY)
  ; ("declassify", DECLASSIFY)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ; ("let", LET)
  ; ("while", WHILE)
  ; ("for", FOR)
  ; (":", COLON)
  ; ("int", TYPE_INT)
  ; ("bool", TYPE_BOOL)
  ; ("=", EQUAL)
  ; ("in", IN)
  ; ("High", HIGH_SEC_LEVEL)
  ; ("Low", LOW_SEC_LEVEL)
  ; ("fn", FN)
  ; (";", SEMICOLON)
  ; ("print", PRINT)
  ; ("securePrint", SECUREPRINT)
  ; ("class", CLASS)
  ; ("constructor", CONSTRUCTOR)
  ; ("new", NEW)
  ; (".", DOT) ]

let test_lex_int =
  QCheck.Test.make ~count:100 ~name:"Lex integers" QCheck.int (fun i ->
      match read_token (Lexing.from_string (Int.to_string i)) with
      | INT j -> j = i
      | _ -> false )

let test_lex_whitespace () = test_lex_token (" ", EOF)

let test_lex_eof () = test_lex_token ("", EOF)

let test_lex_newline () = test_lex_token ("\n", EOF)

let () =
  let qcheck_lex = List.map ~f:QCheck_alcotest.to_alcotest [test_lex_int] in
  Alcotest.run "Lexer Tests"
    [ ( "Syntax Errors"
      , [ Alcotest.test_case "Illegal characters" `Quick
            test_illegal_character ] )
    ; ( "Lexing Tokens"
      , List.map
          ~f:(fun (input, token) ->
            Alcotest.test_case ("Lexing " ^ input) `Quick (fun () ->
                test_lex_token (input, token) ) )
          lex_token_test_cases )
    ; ("Lexer Tests with Randomised Inputs", qcheck_lex)
    ; ( "Whitespace, EOF and New Line Tests"
      , [ Alcotest.test_case "Lex whitespace" `Quick test_lex_whitespace
        ; Alcotest.test_case "Lex EOF" `Quick test_lex_eof
        ; Alcotest.test_case "Lex newline" `Quick test_lex_newline ] ) ]
