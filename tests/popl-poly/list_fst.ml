type 'a list =
| Nil
| Cons of 'a * 'a list

let rec list_fst : ('x * 'y) list -> 'x list |>
  /\([]    -> [],
     [(a2, 'y)]-> [a2],
     [('x, 'y); (a2, 'y)] -> ['x; a2]) = ?
