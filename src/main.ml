(* open Core *)
open Run_interpreter

(* let run_program_file file = In_channel.with_file file ~f:(fun channel ->
   let lexbuf = Lexing.from_channel channel in run_interpreter lexbuf )

   (* From RealWorld OCaml *) let command = Command.basic ~summary:"Run
   program" ~readme:(fun () -> "Takes in a file containing program")
   Command.Param.( map (anon ("Program filename" %: string)) ~f:(fun filename
   () -> run_program_file filename) )

   let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)

let run_program_file file =
  let channel = Core.In_channel.create file in
  try
    let lexbuf = Lexing.from_channel channel in
    let result = run_interpreter lexbuf in
    Core.In_channel.close channel ;
    result
  with e ->
    Core.In_channel.close channel ;
    raise e

let parse_args () =
  let filename = ref "" in
  let usage_msg = "Usage: ./program [options] <filename>\nOptions:" in
  let spec = [] in
  (* Add any flags here if needed *)
  let anon_fun arg =
    if !filename = "" then filename := arg
    else raise (Arg.Bad "Only one filename allowed")
  in
  Arg.parse spec anon_fun usage_msg ;
  if !filename = "" then (Arg.usage spec usage_msg ; exit 1) ;
  !filename

let () =
  let filename = parse_args () in
  run_program_file filename
