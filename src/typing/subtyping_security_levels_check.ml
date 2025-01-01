open Compiler_types.Language_types

(* ordering relation for security levels *)
let less_than_or_equal l1 l2 =
  match (l1, l2) with
  | TSLow, _ -> true
  | TSHigh, TSHigh -> true
  | _ -> false

(* join operation for pc and security levels *)
let join l1 l2 =
  match (l1, l2) with
  | TSHigh, _ | _, TSHigh -> TSHigh
  | TSLow, TSLow -> TSLow

(* perform the subtyping check pc ⊔ l1 ⊑ l *)
let subtyping_check pc l1 l =
  let joined_level = join pc l1 in
  less_than_or_equal joined_level l
