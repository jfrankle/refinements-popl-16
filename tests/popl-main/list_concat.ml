type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type llist =
  | LNil
  | LCons of list * llist

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil u -> l2
  | Cons t -> Cons (#1 t, append (#2 t) l2)
;;

let rec list_concat : llist -> list |>
  /\(LNil -> [],
     LCons ([], LNil) -> [],
     LCons ([0], LNil) -> [0],
     LCons ([0], LCons([0], LNil)) -> [0;0],
     LCons ([1], LNil) -> [1],
     LCons ([1], LCons([1], LNil)) -> [1;1]
     ) = ?
