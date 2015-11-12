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

(* 3 is required because the number has to be different than any of the
   keys for the synthesizer not to return the key instead *)
let rec list_dict_find : (nat -> nat -> bool) -> nat -> list -> option |>
  /\(0 -> 0 -> True,
     0 -> 1 -> False,
     1 -> 0 -> False) ->
  0 ->
  /\(
        [] -> None,
        [(0, 3)] -> Some 3,
        [(1, 3)] -> None,
        [(1, 3); (0, 3)] -> Some 3
    )
= ?
