open Core 
open Compiler_types.Language_types

let rec equal_core_type a b =
  match (a, b) with
  | TEInt, TEInt -> true
  | TEBool, TEBool -> true
  | TFunction (args_a, ret_a), TFunction (args_b, ret_b) ->
      List.length args_a = List.length args_b &&
      (match List.for_all2 ~f:(fun x y -> equal_core_type x y) args_a args_b with
      | Ok result -> result
      | Unequal_lengths -> false) &&
      equal_core_type ret_a ret_b
  | _, _ -> false

let equal_security_level_type a b =
  match (a, b) with
  | TSLow, TSLow -> true
  | TSHigh, TSHigh -> true
  | _, _ -> false

let equal_type_expr a b =
  let (core_a, sec_a) = a in
  let (core_b, sec_b) = b in
  equal_core_type core_a core_b && equal_security_level_type sec_a sec_b
