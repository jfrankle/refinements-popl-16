type nat =
  | Z
  | S of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_drop : 'a list -> nat -> 'a list |>
/\([ ]    ->   nat -> [ ],
   ['a]   -> S nat -> [ ],
   [a1]   -> 0     -> [a1],
   [a2; a1] -> /\(1 -> [a1], 2 -> [])) = ?
