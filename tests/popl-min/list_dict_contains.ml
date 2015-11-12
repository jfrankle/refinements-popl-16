type nat = 
  | Z
  | S of nat

type list =
  | Nil
  | Cons of (nat * nat) * list

type bool =
  | False
  | True

let rec list_dict_contains : (nat -> nat -> bool) -> nat -> list -> bool |>
  /\(0 -> 0 -> True,
     0 -> S nat -> False,
     S nat -> 0 -> False) ->
  0 ->
  /\(
        [] -> False,
        [(0, nat)] -> True,
        [(S nat, nat)] -> False,
        [(nat, nat); (0, nat)] -> True
    )
= ?
