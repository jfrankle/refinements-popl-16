type nat =
  | Z
  | S of nat

let rec nat_div2 : nat -> nat |>
  /\(0 -> 0, 1 -> 0,
     2 -> 1, 3 -> 1,
     4 -> 2) = ?
