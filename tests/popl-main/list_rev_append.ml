type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil u -> l2
  | Cons t -> Cons (#1 t, append (#2 t) l2)
;;

let rec list_rev_append : list -> list |>
/\([]        -> [],
   [0]       -> [0],
   [1]       -> [1],
   [0; 1]    -> [1; 0],
   [1; 0]    -> [0; 1],
   [0; 0; 1] -> [1; 0; 0]) = ?
