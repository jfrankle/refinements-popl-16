type nat = 
  | Z
  | S of nat

type list =
  | Nil
  | Cons of (nat * nat) * list

type bool =
  | False
  | True

type option =
  | Some of nat
  | None

let rec list_dict_find : (nat -> nat -> bool) -> nat -> list -> option |>
  /\(0 -> 0 -> True,
     0 -> S nat -> False,
     S nat -> 0 -> False) ->
  0 ->
  /\(
        [] -> None,
        [(0, 3)] -> Some 3,
        [(1, 3)] -> None,
        [(1, 3); (0, 3)] -> Some 3
    )
= ?
