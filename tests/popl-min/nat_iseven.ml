type nat =
  | Z
  | S of nat

type bool =
  | True
  | False

let rec nat_iseven : nat -> bool |>
  /\(\/(0, 2) -> True,
     \/(1, 3) -> False) = ?
