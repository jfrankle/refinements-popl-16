type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_snoc : 'a -> 'a list -> 'a list |>
/\(a -> /\([ ] -> [a], ['a] -> ['a; a], ['a; 'a] -> ['a; 'a; a])) = ?
