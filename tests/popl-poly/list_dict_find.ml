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

let rec list_dict_find : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option |>
  /\(a1 -> a1 -> True,
     a1 -> a2 -> False,
     a2 -> a1 -> False) ->
  a1 ->
  /\(
        [] -> None,
        [(a1, b1)] -> Some b1,
        [(a2, b1)] -> None,
        [(a2, b1); (a1, b1)] -> Some b1
    )
= ?
