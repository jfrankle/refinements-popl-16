type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_tuple_swap : ('a * 'b) list -> ('b * 'a) list |>
  /\([] -> [],
     [(a1, b1)] -> [(b1, a1)],
     [(a2, b2); (a1, b1)] -> [(b2, a2); (b1, a1)]) = ?
