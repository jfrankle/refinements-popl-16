type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

let rec list_last : 'a list -> 'a option |>
 /\([]        -> None,
    \/([a1], [a2; a1])-> Some(a1)) = ?


