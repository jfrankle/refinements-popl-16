type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type natopt =
  | None
  | Some of nat

let rec list_last : list -> natopt |>
 /\([]        -> None,
    [1]       -> Some(1),
    [2]       -> Some(2),
    [2; 1]    -> Some(1)) = ?


