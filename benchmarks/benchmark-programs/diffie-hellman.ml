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
