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

let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*

rule read_token = parse 
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULTIPLY }
    | "/" { DIVIDE }
    | "True" { TRUE }
    | "False" { FALSE }
    | whitespace { read_token lexbuf }
    | newline { next_line lexbuf; read_token lexbuf }
    | int { INT (int_of_string (lexeme lexbuf)) }
    | eof { EOF }
    | _ { raise (SyntaxError ("Invalid character " ^ Lexing.lexeme lexbuf)) }