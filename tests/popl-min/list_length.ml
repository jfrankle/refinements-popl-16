type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec list_length : list -> nat |>
/\([ ]    -> 0,
   [nat]  -> 1,
   [nat; nat] -> 2) = ?
