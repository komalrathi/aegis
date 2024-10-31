open Core
open Run_interpreter

let run_program_file file =
  In_channel.with_file file ~f:(fun channel ->
      let lexbuf = Lexing.from_channel channel in
      run_interpreter lexbuf )

(* From RealWorld OCaml *)
let command =
  Command.basic ~summary:"Run program"
    ~readme:(fun () -> "Takes in a file containing program")
    Command.Param.(
      map
        (anon ("Program filename" %: string))
        ~f:(fun filename () -> run_program_file filename) )

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
