(* open Parsing open Parser_frontend.Lexer open Parser_frontend.Parser open
   Pprint_parser_tokens open Core

   let parser_token_testable = Alcotest.testable (fun token1 token2 -> token1
   = token2)

   let test_illegal_character () = Alcotest.check_raises "Syntax Error"
   (SyntaxError "Lexer - Illegal character: ?") (fun () -> ignore (read_token
   (Lexing.from_string "?") : token) )

   let test_lex_token (input_str, token) = Alcotest.(check
   parser_token_testable) "same token" token (read_token (Lexing.from_string
   input_str))

   let lex_token_test_cases = let open Alcotest in List.map ~f:(fun
   (input_str, expected_token) -> test_case ("Lex token " ^ input_str) `Quick
   (fun () -> test_lex_token (input_str, expected_token) ) ) [ ("(",
   LEFT_PAREN) ; (")", RIGHT_PAREN) ; ("{", LEFT_BRACE) ; ("}", RIGHT_BRACE)
   ; (",", COMMA) ; ("+", PLUS) ; ("-", MINUS) ; ("*", MULTIPLY) ; ("/",
   DIVIDE) ; ("True", TRUE) ; ("False", FALSE) ; ("<", LT) ; (">", GT) ;
   ("<=", LTE) ; (">=", GTE) ; ("&&", AND) ; ("||", OR) ; (":=", ASSIGN) ;
   ("classify", CLASSIFY) ; ("declassify", DECLASSIFY) ; ("if", IF) ;
   ("then", THEN) ; ("else", ELSE) ; ("let", LET) ; (":", COLON) ; ("int",
   TYPE_INT) ; ("bool", TYPE_BOOL) ; ("=", EQUAL) ; ("in", IN) ; ("High",
   HIGH_SEC_LEVEL) ; ("Low", LOW_SEC_LEVEL) ; ("fn", FN) ; ("->", ARROW) ;
   (",", COMMA) ; (";", SEMICOLON) ]

   let test_lex_int = QCheck.Test.make ~count:100 ~name:"Lex integers"
   QCheck.(int) (fun i -> INT i = read_token (Lexing.from_string
   (Int.to_string i)))

   let test_lex_string_newline () = test_lex_token ("\"\n\"", STRING "\n")

   let test_lex_whitespace () = test_lex_token ("\" \"", STRING " ")

   let test_lex_eof () = test_lex_token ("", EOF)

   let test_lex_newline () = test_lex_token ("\n", EOF)

   let () = let qcheck_lex = List.map ~f:QCheck_alcotest.to_alcotest
   [test_lex_int] in let open Alcotest in run "Lexer" [ ( "Syntax Errors" ,
   [test_case "Illegal characters" `Quick test_illegal_character] ) ; (
   "Accepted\n Tokens" , List.concat [ lex_token_test_cases ; [ test_case
   "Lex\n whitespace" `Quick test_lex_whitespace ; test_case "Lex eof" `Quick
   test_lex_eof ; test_case "Lex new line" `Quick test_lex_newline ] ;
   qcheck_lex ] ) ] *)