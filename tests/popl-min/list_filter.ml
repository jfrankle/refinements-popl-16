type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type bool =
  | True
  | False

let rec list_filter :(nat -> bool) -> list -> list |>
/\(/\(1 -> False, not(1) -> True) ->
   /\(\/([], [1]) -> [], \/([0; 1], [0]) -> [0], [2] -> [2],
        [0; 0] -> [0; 0]),
   /\(0 -> False, S nat -> True) ->
     /\([] -> [], \/([1], [0; 1], [0; 0; 1]) -> [1])
  ) = ?
