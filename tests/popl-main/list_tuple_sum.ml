type nat =
  | Z
  | S of nat

type listp =
  | PNil
  | PCons of (nat * nat) * listp

let rec add (n1:nat) (n2:nat) : nat =
  match n1 with
  | Z u -> n2
  | S n1 -> S (add n1 n2)
;;

let rec list_tuple_sum : listp -> (nat * nat) |>
  /\(PNil    -> (0, 0),
     PCons((1, 0), PNil) -> (1, 0),
     PCons((2, 1), PCons((1, 0), PNil)) -> (3, 1),
     PCons((1, 1), PCons((2, 1), PCons((1, 0), PNil))) -> (4, 2)) = ?
