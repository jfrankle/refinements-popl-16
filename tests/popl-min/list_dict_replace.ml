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

(* These numbers were picked incredibly carefully so the synthesizer wouldn't
   just increment keys or values.  Doing so was a painful process *)
let rec list_dict_replace : (nat -> nat -> bool) -> nat -> nat -> list -> list |>
  /\(0 -> 0 -> True,
     0 -> 3 -> False,
     3 -> 0 -> False) ->
  0 -> 5->
  /\(
        [] -> [],
        [(0, nat)] -> [(0, 5)],
        [(3, 4)] -> [(3, 4)],
        [(3, 4); (0, nat)] -> [(3, 4); (0, 5)],
        [(0, nat); (3, 4)] -> [(0, 5); (3, 4)]
    )
= ?
