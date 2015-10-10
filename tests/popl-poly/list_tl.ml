type 'a list =
  | Nil
  | Cons of 'a * 'a list

let list_tl : 'a list -> 'a list |>
 /\(\/([], ['a]) -> [],
    ['a; a] -> [a]) = ?
