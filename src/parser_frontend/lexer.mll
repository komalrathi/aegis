{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }

}

(* Regular Expressions *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let int_regex = '-'? digit+
let id_regex = (alpha) (alpha|digit|'_')*


rule read_token = parse 
    | "(" { LEFT_PAREN }
    | ")" { RIGHT_PAREN }
    | "{" { LEFT_BRACE }
    | "}" { RIGHT_BRACE }
    | "," { COMMA }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULTIPLY }
    | "/" { DIVIDE }
    | "^" { EXPONETIATION }
    | "%" { MODULUS }
    | "True" { TRUE }
    | "False" { FALSE }
    | "<" { LT }
    | ">" { GT }
    | "<=" { LTE }
    | ">=" { GTE }
    | "==" { EQUALITY }
    | "&&" { AND }
    | "||" { OR }
    | "!" { NOT }
    | ":=" { ASSIGN }
    | "classify" { CLASSIFY }
    | "declassify" { DECLASSIFY }
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "let" {LET}
    | "while" {WHILE}
    | "for" {FOR}
    | ":" {COLON}
    | "int" {TYPE_INT}
    | "bool" {TYPE_BOOL}
    | "=" {EQUAL}
    | "in" {IN}
    | "High" {HIGH_SEC_LEVEL}
    | "Low" {LOW_SEC_LEVEL}
    | "fn" {FN}
    | "," {COMMA}
    | ";" {SEMICOLON}
    | "print" {PRINT}
    |"securePrint" {SECUREPRINT}
    | "class" {CLASS}
    | "constructor" {CONSTRUCTOR}
    | "new" {NEW}
    | "." {DOT}
    | "raise" {RAISE}
    | "raise!" {RESUMABLE_RAISE}
    | "try" {TRY}
    | "catch" {CATCH}
    | "finally" {FINALLY}
    | "DivisionByZero" {DIVISION_BY_ZERO}
    | "IntegerOverflow" {INTEGER_OVERFLOW}
    | "continue" {CONTINUE}
    | "//" { comment lexbuf }
    | whitespace { read_token lexbuf }
    | newline { next_line lexbuf; read_token lexbuf }
    | int_regex { INT (int_of_string (lexeme lexbuf)) }
    | id_regex {IDENTIFIER (lexeme lexbuf)}
    | eof { EOF }
    | _ { raise (SyntaxError ("Invalid character " ^ Lexing.lexeme lexbuf)) }

and comment = parse
    | newline { next_line lexbuf; read_token lexbuf }
    | _ { comment lexbuf }
    | eof { EOF }