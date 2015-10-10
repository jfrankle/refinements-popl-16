type nat =
  | Z
  | S of nat

let nat_dec : nat -> nat |>
  /\(0 -> 0, 1 -> 0, 2 -> 1) = ?
