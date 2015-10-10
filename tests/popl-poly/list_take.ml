type nat =
  | Z
  | S of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_take : nat -> 'a list -> 'a list |>
/\(nat -> []   -> [],
   Z   -> list -> [],
   1 -> /\([a] -> [a], [b; a] -> [b]),
   2 -> [b; a] -> [b; a]
) = ?
