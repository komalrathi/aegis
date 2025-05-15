let ocaml_exception_raised () =
  let safe_div x y = if y = 0 then raise Division_by_zero else x / y in
  try
    let _ = safe_div 8 0 in
    ()
  with Division_by_zero -> ()
