(** rank of node = shortest distance to a leaf (empty node)
  * hence, rank of node = 1 + min(rnak of left, rank of right)
  * leftist tree has 2 properties:
  *  For ever node
  *  * leftist: rnak(left) >= rank(right)
  *  * min-heap: value(parent) <= value(node)
  *)

type 'a t = L | N of int * 'a * 'a t * 'a t;;

exception Empty;;

let rank = function
  | L -> 0
  | N (rk, _, _, _) -> rk;;

let empty = L;;

let is_empty t = t = L;;

let rec merge t1 t2 =
  match t1, t2 with
  | t, L | L, t -> t
  | N (_, x1, _, _), N (_, x2, _, _) when x2 < x1 ->
    merge t2 t1
  | N (_, x, l, r), t ->
    let t' = merge r t in
    if rank t' > rank l then N (1 + rank l, x, t', l)
    else N (1 + rank t', x, l, t');;

let insert x t =
  merge t (N (1, x, L, L));;

let of_list l = List.fold_left (Fun.flip insert) L l;;

let get_min = function
  | L -> raise Empty
  | N (_, x, _, _) -> x;;

let delete_min = function
  | L -> L
  | N (_, _, l, r) -> merge l r;;

let rec to_list t =
  if is_empty t then []
  else get_min t :: to_list (delete_min t);;
