type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type tree =
  | L
  | N of tree * nat * tree

let rec tree_map : (nat -> nat) -> tree -> tree |>
/\(/\(0 -> 0, 1 -> 0, 2 -> 1) ->
       /\( L -> L
         , N (L, 0, L) -> N (L, 0, L)
         , N (L, 2, L) -> N (L, 1, L)
         , N (N (L, 2, L), 2, L) -> N (N (L, 1, L), 1, L)
         , N (L, 1, N (L, 2, L)) -> N (L, 0, N (L, 1, L))
         )
  ,/\(0 -> 1) ->
       /\( L -> L
         , N (L, 0, L) -> N (L, 1, L)
         )
   ) = ?
