type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_length : list -> nat |>
/\([ ]    -> 0,
   [1]    -> 1,
   [2; 1] -> 2,
   [0] -> not(0)) = ?
