let simple_aes_encrypt plaintext key = plaintext * key mod 5

let run_ocaml_aes () =
  let plaintext = 7 in
  let key = 4 in
  let result = simple_aes_encrypt plaintext key + 1 in
  (* For the sake of the benchmark, we ignore the result *) ignore result
