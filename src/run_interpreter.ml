open Parser_frontend.Parse_program
open Core
open Typing.Type_program
open Interpreter.Interpret_program
open Compiler_types.Language_types

let string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VUnit _ -> "unit"

let print_result = function
  | Ok v -> printf "Success: %s" (string_of_val v)
  | Error e -> eprintf "%s" (Error.to_string_hum e)

(* let run_interpreter lexbuf = let ( >>= ) = Result.( >>= ) in parse_program
   lexbuf >>= type_program >>= interpret_program |> function | Ok v -> printf
   "Success: %s" (string_of_val v) | Error e -> eprintf "%s"
   (Error.to_string_hum e) *)

let run_interpreter lexbuf =
  let ( >>= ) = Result.( >>= ) in
  match parse_program lexbuf >>= type_program with
  | Ok typed_program -> interpret_program typed_program |> print_result
  | Error e -> print_result (Error e)
