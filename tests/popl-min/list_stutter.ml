type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_stutter : list -> list |>
/\([] -> [],
   [nat] -> [nat; nat],
   [S nat; nat] -> [S nat; S nat; nat; nat]) = ?
