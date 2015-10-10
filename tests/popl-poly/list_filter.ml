type bool =
  | True
  | False

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_filter : ('a -> bool) -> 'a list -> 'a list |>
  /\(/\(a1 -> False, a2 -> True) ->
      /\([  ] -> [  ],
	     \/([a2], Cons(a1, \/([a2], [a1; a2]))) -> [a2],
		 [a2; a1; a2] -> [a2; a2])) = ?

