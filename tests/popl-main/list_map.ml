type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_map : (nat -> nat) -> list -> list |>
/\(/\(0 -> 0, 1 -> 0) -> /\([] -> [], [0] -> [0], [1] -> [0]),
   /\(0 -> 1, 1 -> 2) -> /\([] -> [], [1] -> [2], [1; 1] -> [2; 2])
  ) = ?
