type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type listp =
  | PNil
  | PCons of (nat * nat) * listp

let rec list_fst : listp -> list |>
  /\(PNil    -> [],
     PCons((1, 0), PNil) -> [1],
     PCons((0, 1), PCons((1, 0), PNil)) -> [0; 1]) = ?
