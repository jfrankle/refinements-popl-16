type bool =
  | True
  | False

type tree =
  | L
  | N of tree * bool * tree

type list =
  | Nil
  | Cons of bool * list

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil u -> l2
  | Cons t -> Cons (#1 t, append (#2 t) l2)
;;

let rec tree_collect_leaves : tree -> list |>
  /\( L -> []
  , N (L, True, L) -> [True]
  , N (L, False, L) -> [False]
  , N (N (L, True, L), False, L) -> [True; False]
  , N (N (L, False, L), True, L) -> [False; True]
  , N (L, False, N (L, True, L)) -> [False; True]
  ) = ?
