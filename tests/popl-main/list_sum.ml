type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec fold (l:list) (f:nat -> nat -> nat) (acc:nat) : nat =
  match l with
  | Nil u -> acc
  | Cons t -> fold (#2 t) f (f acc (#1 t))
;;

let rec add (n1:nat) (n2:nat) : nat =
  match n1 with
  | Z u -> n2
  | S n1 -> S (add n1 n2)
;;

let list_sum : list -> nat |>
/\([] -> 0, [1] -> 1, [2; 1] -> 3, [2] -> 2) = ?
