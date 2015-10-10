type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_drop : list -> nat -> list |>
/\([ ]    -> nat -> [ ],
   Cons(nat, Nil) -> S nat -> [],
   Cons(nat, Cons(nat, Nil)) -> 2 -> [],
   [0]    -> 0 -> [0],
   [1; 0] -> 1 -> [0]) = ?
