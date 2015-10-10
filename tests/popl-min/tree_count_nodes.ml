type nat =
  | Z
  | S of nat

type tree =
  | Leaf
  | Node of tree * nat * tree

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | Z u -> n2
  | S n1 -> S (sum n1 n2)
;;

let rec tree_count_nodes : tree -> nat |>
  /\( Leaf -> 0
  , Node(Leaf, nat, Leaf) -> 1
  , Node\/((Node(Leaf, nat, Leaf), nat, Leaf)
          ,(Leaf, nat, Node(Leaf, nat, Leaf))) -> 2
  , Node \/((Node(Leaf, nat, Node(Leaf, nat, Leaf)), nat, Leaf)
           ,(Leaf, nat, Node(Leaf, nat, Node(Leaf, nat, Leaf)))) -> 3
  ) = ?
