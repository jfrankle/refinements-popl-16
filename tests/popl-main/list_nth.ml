type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type opt =
  | None
  | Some of nat

let rec list_nth : list -> nat -> opt |>
/\([]        -> /\(0 -> None,   1 -> None),
   [2]       -> /\(0 -> Some 2, 1 -> None),
   [1; 2]    -> /\(0 -> Some 1, 1 -> Some 2),
   [1]       -> /\(0 -> Some 1, 1 -> None),
   [2; 1]    -> /\(0 -> Some 2, 1 -> Some 1)) = ?
