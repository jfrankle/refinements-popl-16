type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec snoc (l:list) (n:nat) : list =
      match l with
      | Nil u -> Cons (n, Nil)
      | Cons t -> Cons (#1 t, snoc(#2 t) n)
;;

let rec list_rev_snoc : list -> list |>
/\([]        -> [],
   [0]       -> [0],
   [1]       -> [1],
   [0; 1]    -> [1; 0],
   [0; 0; 1] -> [1; 0; 0]) = ?
