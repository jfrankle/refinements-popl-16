type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type cmp =
  | LT
  | EQ
  | GT

let rec compare (n1 : nat) (n2 :nat) : cmp =
  match n1 with
  | Z u -> ( match n2 with
           | Z u -> EQ
           | S m -> LT
         )
  | S m1 ->
      ( match n2 with
      | Z u -> GT
      | S m2 -> (compare m1 m2) )
;;

let rec insert (l : list) (n :nat) : list =
  match l with
  | Nil u -> Cons(n, Nil)
  | Cons t ->
    (match compare n (#1 t) with
     | LT u -> Cons (n, Cons(#1 t, #2 t))
     | EQ u -> l
     | GT u -> Cons (#1 t, insert (#2 t) n)
    )
;;

let rec list_sort_sorted_insert : list -> list |>
/\([]      -> [],
   [0]     -> [0],
   [1]     -> [1],
   [0;0]   -> [0],
   [1;0]   -> [0;1],
   [1;1]   -> [1],
   [0;1;1] -> [0;1]) = ?


