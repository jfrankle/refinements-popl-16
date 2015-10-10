type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_unzip : ('a * 'b) list -> ('a list * 'b list) |>
  /\([] -> ([], []),
     [(a, b)] -> ([a], [b]),
     [(c, d); (a, b)] -> ([c; a], [d; b])) = ?
