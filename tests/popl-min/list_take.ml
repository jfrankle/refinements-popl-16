type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_take : nat -> list -> list |>
/\(0        -> list -> [],
   nat      -> []   -> [],
   S nat    -> [1]  -> [1],
   1 -> /\([0; 1] -> [0],    [1; 0; 1] -> [1]),
   2 -> /\([0; 1] -> [0; 1], [1; 0; 1] -> [1; 0])) = ?
