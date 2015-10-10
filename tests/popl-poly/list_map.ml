type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_map : ('a -> 'b) -> 'a list -> 'b list |>
    (a1 -> b1) -> /\([] -> [], [a1] -> [b1], [a1; a1] -> [b1; b1]) = ?
