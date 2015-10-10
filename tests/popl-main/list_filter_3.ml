type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type bool =
  | True
  | False

let rec list_filter_3 :list -> (nat -> bool) -> list |>
/\([] -> /\(0 -> True, 1 -> False, 2 -> True) -> [],
   [0] -> /\(0 -> True, 1 -> False, 2 -> True) -> [0],
   [1] -> /\(0 -> True, 1 -> False, 2 -> True) -> [],
   [2] -> /\(0 -> True, 1 -> False, 2 -> True) -> [2],
   [0; 0] -> /\(0 -> True, 1 -> False, 2 -> True) -> [0; 0],
   [0; 1] -> /\(0 -> True, 1 -> False, 2 -> True) -> [0],
   [] -> /\(0 -> False, 1 -> True, 2 -> True) -> [],
   [1] -> /\(0 -> False, 1 -> True, 2 -> True) -> [1],
   [0; 1] -> /\(0 -> False, 1 -> True, 2 -> True) -> [1],
   [0; 0; 1] -> /\(0 -> False, 1 -> True, 2 -> True) -> [1]) = ?
