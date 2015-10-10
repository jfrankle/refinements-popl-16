type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_hd : list -> nat |>
/\([]  -> 0,
   [1] -> not(0)) = ?
