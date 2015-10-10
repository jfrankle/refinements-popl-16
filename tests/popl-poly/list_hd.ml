type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

let list_hd : 'a list -> 'a option |>
/\([]  -> None,
   [a1] -> Some a1) = ?
