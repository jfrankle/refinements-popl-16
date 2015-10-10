type nat =
  | Z
  | S of nat

type bool =
  | True
  | False

let rec nat_iseven : nat -> bool |>
  /\(0 -> True,
     1 -> False,
     2 -> True,
     3 -> False) = ?
