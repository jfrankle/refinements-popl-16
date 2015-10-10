type 'a list =
  | Nil
  | Cons of 'a * 'a list

type nat =
  | Z
  | S of nat

let rec list_length : 'a list -> nat |>
/\([ ]      -> 0,
   ['a]     -> 1,
   ['a; 'a] -> 2) = ?
