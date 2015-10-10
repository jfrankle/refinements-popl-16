type cmp =
  | CEq
  | CGt
  | CLt

type nat =
  | Z
  | S of nat

type tree =
  | L
  | N of tree * nat * tree

let rec comp_nat (n1:nat) (n2:nat) : cmp =
  match n1 with
  | Z u -> (match n2 with
          | Z u -> CEq
          | S n2 -> CLt)
  | S n1 -> (match n2 with
              | Z u -> CGt
              | S n2 -> comp_nat n1 n2)
;;

let rec tree_binsert : tree -> nat -> tree |>
/\(L -> /\( 0 -> N (L, 0, L)
          , 1 -> N (L, 1, L)
          , 2 -> N (L, 2, L))
  , N (L, 1, L) -> /\( 0 -> N (N (L, 0, L), 1, L)
                     , 1 -> N (L, 1, L)
                     , 2 -> N (L, 1, N (L, 2, L)))
  , N (L, 0, L) -> /\( 0 -> N (L, 0, L)
                     , 1 -> N (L, 0, N (L, 1, L))
                     , 2 -> N (L, 0, N (L, 2, L)))
  , N (L, 2, L) -> /\( 0 -> N (N (L, 0, L), 2, L)
                     , 1 -> N (N (L, 1, L), 2, L)
                     , 2 -> N (L, 2, L))
  , N (N (L, 0, L), 1, L) ->
    /\( 0 -> N (N (L, 0, L), 1, L)
      , 1 -> N (N (L, 0, L), 1, L)
      , 2 -> N (N (L, 0, L), 1, N(L, 2, L)))
  , N (L, 0, N (L, 1, L)) ->
      /\( 2 -> N (L, 0, N (L, 1, N(L, 2, L))))
  , N (N (L, 1, L), 2, L) ->
      /\( 0 -> N (N (N(L, 0, L), 1, L), 2, L))
  , N (L, 1, N (L, 2, L)) ->
      /\( 0 -> N (N (L, 0, L), 1, N (L, 2, L))
        , 1 -> N (L, 1, N (L, 2, L)))
  , N (N (L, 1, L), 2, L) ->
      /\( 0 -> N (N (N(L, 0, L), 1, L), 2, L))
  ) = ?
