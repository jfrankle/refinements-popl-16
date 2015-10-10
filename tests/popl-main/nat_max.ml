type nat =
  | Z
  | S of nat

type cmp =
  | LT
  | EQ
  | GT

let rec compare (n1 : nat) (n2 :nat) : cmp =
  match n1 with
  | Z u -> ( match n2 with 
           | Z u -> EQ
           | S m -> LT
         )
  | S m1 -> 
      ( match n2 with
      | Z u -> GT
      | S m2 -> (compare m1 m2) )
;;

let nat_max : nat -> nat -> nat |>
 /\(0 -> /\(0 -> 0, 1 -> 1, 2 -> 2),
    1 -> /\(0 -> 1, 1 -> 1, 2 -> 2),
    2 -> /\(0 -> 2, 1 -> 2, 2 -> 2)) =?
