type nat = 
  | Z
  | S of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

type bool =
  | False
  | True

type 'a option =
  | Some of 'a
  | None

let rec list_dict_replace : ('a -> 'a -> bool) -> 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list |>
  /\(a1 -> a1 -> True,
     a1 -> a2 -> False,
     a2 -> a1 -> False) ->
  a1 -> b2 ->
  /\(
        [] -> [],
        [(a1, 'b)] -> [(a1, b2)],
        [(a2, b1)] -> [(a2, b1)],
        [(a2, b1); (a1, 'b)] -> [(a2, b1); (a1, b2)],
        [(a1, 'b); (a2, b1)] -> [(a1, b2); (a2, b1)]
    )
= ?
