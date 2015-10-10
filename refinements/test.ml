type bool =
  | True
  | False

let bool_band : bool -> bool -> bool |>
/\(True  -> True  -> True,
   False -> bool  -> False,
   bool  -> False -> False) = ?
