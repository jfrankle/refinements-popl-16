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
     PCons((S nat, nat), PNil) -> [S nat],
     PCons((0, nat), PCons((S nat, nat), PNil)) -> [0; S nat]) = ?
