type nat =
  | Z
  | S of nat

type listp =
  | PNil
  | PCons of (nat * nat) * listp

let rec list_tuple_swap : listp -> listp |>
  /\(PNil    -> PNil,
     PCons((1, 0), PNil) -> PCons((0, 1), PNil),
     PCons((0, 1), PCons((1, 0), PNil)) -> PCons((1, 0), PCons((0, 1), PNil))) = ?
