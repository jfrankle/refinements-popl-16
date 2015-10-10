type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_append  : 'a list -> 'a list -> 'a list |>
/\([ ]      -> /\([] -> [],       [a1] -> [a1]),
   [a1]     -> /\([] -> [a1],     [a1] -> [a1; a1]),
   [a2;a1]  -> /\([] -> [a2; a1], [a1] -> [a2; a1; a1])) = ?
