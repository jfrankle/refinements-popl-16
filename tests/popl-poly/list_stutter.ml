type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_stutter : 'a list -> 'a list |>
/\([] -> [],
   [a] -> [a; a],
   [b; a] -> [b; b; a; a]) = ?
