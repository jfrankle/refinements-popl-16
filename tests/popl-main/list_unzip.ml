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
     PCons((1, 0), PNil) -> ([1], [0]),
     PCons((0, 1), PCons((1, 0), PNil)) -> ([0;1], [1;0])) = ?
