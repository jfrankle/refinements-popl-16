type nat =
  | Z
  | S of nat

type listp =
  | PNil
  | PCons of (nat * nat) * listp

type list =
  | Nil
  | Cons of nat * list

let rec list_unzip : listp -> (list * list) |>
  /\(PNil    -> (Nil, Nil),
     PCons((S nat, 0), PNil) -> ([S nat], [0]),
     PCons((0, S nat), PCons((S nat, 0), PNil)) -> ([0;S nat], [S nat;0])) = ?
