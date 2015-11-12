type 'a list =
  | Nil
  | Cons of 'a * 'a list

type bool =
  | True
  | False

let rec list_dict_contains : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool |>
  /\(a1 -> a1 -> True,
     a1 -> a2 -> False,
     a2 -> a1 -> False) ->
  a1 ->
  /\(
        [] -> False,
        [(a1, b1)] -> True,
        [(a2, b1)] -> False,
        [(a2, b1); (a1, b1)] -> True
    )
= ?
