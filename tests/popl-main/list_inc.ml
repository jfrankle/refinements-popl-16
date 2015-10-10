type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec map (l:list) (f:nat -> nat) : list =
  match l with
  | Nil u -> Nil
  | Cons t -> Cons (f (#1 t), map (#2 t) f)
;;

let list_inc : list -> list |>
/\([] -> [], [0] -> [1], [1] -> [2], [0; 1] -> [1; 2], [1; 0] -> [2; 1]) = ?
