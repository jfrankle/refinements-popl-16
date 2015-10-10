type nat =
  | Z
  | S of nat

type listp =
  | PNil
  | PCons of (nat * nat) * listp

let rec list_tuple_swap : listp -> listp |>
  /\(PNil    -> PNil,
     PCons((S nat, 0), PNil) -> PCons((0, S nat), PNil),
     PCons((0, S nat), PCons((S nat, 0), PNil)) -> PCons((S nat, 0), PCons((0, S nat), PNil))) = ?
