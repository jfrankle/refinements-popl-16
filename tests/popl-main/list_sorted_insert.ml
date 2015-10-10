type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

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

let rec list_sorted_insert : list -> nat -> list |>
/\([ ]   -> /\(0 -> [0],   1 ->   [1], 2 -> [2]),
   [0]   -> /\(0 -> [0],   1 -> [0;1]),
   [1]   -> /\(0 -> [0;1], 1 ->   [1], 2 -> [1;2]),
   [2]   -> /\(0 -> [0;2], 1 -> [1;2], 2 ->   [2]),
   [0;1] -> /\(0 -> [0;1], 1 -> [0;1], 2 -> [0;1;2])) = ?
