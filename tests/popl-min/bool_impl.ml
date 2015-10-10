type bool =
  | True
  | False

let bool_impl : bool -> bool -> bool |>
/\(True  -> True  -> True,
   True  -> False -> False,
   False -> bool  -> True) = ?
