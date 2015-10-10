type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_tl : list -> list |>
 /\(\/([], [nat]) -> [],
    [0; 0] -> [0]) = ?
