type bool =
  | True
  | False

type tree =
  | Leaf
  | Node of tree * bool * tree

type nat =
  | Z
  | S of nat

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | Z u -> n2
  | S n1 -> S (sum n1 n2)
;;

let rec tree_count_leaves : tree -> nat |>
  /\( Leaf -> 1
  , Node (Leaf, bool, Leaf) -> 2
  , Node \/((Node (Leaf, bool, Leaf), bool, Leaf)
           ,(Leaf, bool, Node (Leaf, bool, Leaf))) -> 3
  , Node \/((Node (Node (Leaf, bool, Leaf), bool, Leaf), bool, Leaf)
           ,(Node (Leaf, bool, Leaf), bool, Node (Leaf, bool, Leaf))) -> 4
  , Node (Node (Leaf, bool, Leaf), bool,
      Node (Node (Leaf, bool, Leaf), bool, Node (Leaf, bool, Leaf))) -> 6
  ) = ?
