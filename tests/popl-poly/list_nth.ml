type nat =
  | Z
  | S of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

let rec list_nth : 'a list -> nat -> 'a option |>
   /\([] -> nat -> None,
      ['a] -> S nat -> None,
	  [a1] -> 0 -> Some a1,
	  [a2; a1] -> /\(0 -> Some a2, 1 -> Some a1)) = ?
