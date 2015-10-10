(* This is really a boollist example, not a natlist example *)

type nat =
  | Z
  | S of nat

type bool =
  | True
  | False

type list =
  | Nil
  | Cons of bool * list

let rec list_even_parity : list -> bool |>
/\([]      -> True,
   [False] -> True,
   [True]  -> False,
   [False; False] -> True,
   [False; True ] -> False,
   [True;  False] -> False,
   [True;  True ] -> True) = ?
