type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec fold (l:list) (f:list -> nat -> list) (acc:list) : list =
  match l with
  | Nil u -> acc
  | Cons t -> fold (#2 t) f (f acc (#1 t))
;;

let rec snoc (l:list) (n:nat) : list =
      match l with
      | Nil u -> Cons (n, Nil)
      | Cons t -> Cons (#1 t, snoc(#2 t) n)
;;

let list_rev_fold : list -> list |>
/\([]        -> [],
   [0]       -> [0],
   [1]       -> [1],
   [0; 1]    -> [1; 0],
   [0; 0; 1] -> [1; 0; 0]) = ?
