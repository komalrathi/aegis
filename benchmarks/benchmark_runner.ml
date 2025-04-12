open Core_bench
open Run_interpreter
open Core

(* Aegis programs as in-memory strings *)
let simple_aes_code =
  {| 
  fn aes_encrypt(plaintext: (int, Low), key: (int, High)) : (int, High) {
      (plaintext * key) % 5 
      } 
      let secret_key: (int, High) = 4 in { 
        let message:(int, Low) = 7 in { 
          let encrypted: (int, High) = aes_encrypt(message,secret_key) in { 
            encrypted := encrypted + 1 
            } 
          } 
        } 
    |}

let simple_dh_code =
  {| 
  fn diffie_hellman(private_key: (int, High), public_base: (int, Low), public_mod: (int, Low)) : (int, High) {
    (public_base ^ private_key) % public_mod 
  } 
  let alice_private: (int, High) = 3 in { 
    let bob_private: (int, High) = 4 in {
      let g: (int, Low) = 5 in {
        let p: (int, Low) = 7 in { 
          let alice_public: (int, High) = diffie_hellman(alice_private, g, p) in { 
            let bob_public: (int, High) = diffie_hellman(bob_private, g, p) in { 
              alice_public := alice_public + 1;
              bob_public := bob_public + 1 
            } 
          } 
        } 
      } 
    } 
  } 
  |}

let aegis_no_exception_raised_code =
  {|
fn division (x :(int, Low), y:(int, Low)):(int, Low) { 
    try {
        if (y == 0) then {
            raise (DivisionByZero y)
        }
        else {
            x := x/y
        }
    }   
    catch (DivisionByZero y) {
        x := 1003
    }
    finally {
        y := 508;
        y := 60
    }
}
division(7, 4)
|}

let aegis_exception_raised_code =
  {|
fn division (x :(int, Low), y:(int, Low)):(int, Low) { 
    try {
        if (y == 0) then {
            raise (DivisionByZero y)
        }
        else {
            x := x/y
        }
    }   
    catch (DivisionByZero y) {
        x := 1003
    }
    finally {
        y := 508;
        y := 60
    }
}
division(8, 0)
|}

let run_aegis_aes () = run_interpreter (Lexing.from_string simple_aes_code)

let run_aegis_dh () = run_interpreter (Lexing.from_string simple_dh_code)

let run_aegis_no_exception_raised () =
  run_interpreter (Lexing.from_string aegis_no_exception_raised_code)

let run_aegis_exception_raised () =
  run_interpreter (Lexing.from_string aegis_exception_raised_code)

(* OCaml implementations of AES and Diffie-Hellman for benchmarking *)
let simple_aes_encrypt plaintext key = plaintext * key mod 5

let run_ocaml_aes () =
  let plaintext = 7 in
  let key = 4 in
  let result = simple_aes_encrypt plaintext key + 1 in
  (* For the sake of the benchmark, we ignore the result *) ignore result

(* Fast exponentiation modulo m *)
let pow_mod base exponent modulo =
  let rec aux b e result =
    if e = 0 then result
    else if e mod 2 = 1 then
      aux (b * b mod modulo) (e / 2) (result * b mod modulo)
    else aux (b * b mod modulo) (e / 2) result
  in
  aux base exponent 1

let diffie_hellman private_key public_base public_mod =
  pow_mod public_base private_key public_mod

let run_ocaml_diffie_hellman () =
  let alice_private_key = 3 in
  let bob_private_key = 4 in
  (* In a real scenario, these would be public keys exchanged over a
     network *)
  let public_base = 5 in
  let public_mod = 7 in
  let alice_public_key =
    diffie_hellman alice_private_key public_base public_mod + 1
  in
  let bob_public_key =
    diffie_hellman bob_private_key public_base public_mod + 1
  in
  (* For the sake of the benchmark, we ignore the results *)
  ignore alice_public_key ; ignore bob_public_key

let ocaml_no_exception_raised () =
  let safe_div x y = if y = 0 then raise Division_by_zero else x / y in
  try
    let _ = safe_div 7 4 in
    ()
  with Division_by_zero -> ()

let ocaml_exception_raised () =
  let safe_div x y = if y = 0 then raise Division_by_zero else x / y in
  try
    let _ = safe_div 8 0 in
    ()
  with Division_by_zero -> ()

let () =
  let tests =
    [ Bench.Test.create ~name:"Aegis AES" (fun () -> run_aegis_aes ())
    ; Bench.Test.create ~name:"Aegis Diffie-Hellman" (fun () ->
          run_aegis_dh () )
    ; Bench.Test.create ~name:"Aegis No Exception Raised" (fun () ->
          run_aegis_no_exception_raised () )
    ; Bench.Test.create ~name:"Aegis Exception Raised" (fun () ->
          run_aegis_exception_raised () )
    ; Bench.Test.create ~name:"OCaml AES" (fun () -> run_ocaml_aes ())
    ; Bench.Test.create ~name:"OCaml Diffie-Hellman" (fun () ->
          run_ocaml_diffie_hellman () )
    ; Bench.Test.create ~name:"OCaml Exception Raised" (fun () ->
          ocaml_exception_raised () )
    ; Bench.Test.create ~name:"OCaml No Exception Raised" (fun () ->
          ocaml_no_exception_raised () ) ]
  in
  Command_unix.run (Bench.make_command tests)
