type 'a t = Leaf | Node of 'a * 'a t * 'a t;;

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) ->
    1 + size l + size r;;

let rec height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) ->
    1 + max (height l) (height r);;

let empty = Leaf;;

let is_empty t = t = Leaf;;

let rec insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' ->
    Node (x', insert x l, r)
  | Node (x', l, r) when x > x' ->
    Node (x', l, insert x r)
  | _ -> t;;

let of_list l =
  List.fold_left (Fun.flip insert) Leaf l;;

let rec mem x t =
  match t with
  | Leaf -> false
  | Node (x', l, _) when x < x' ->
    mem x l
  | Node (x', _, r) when x > x' ->
    mem x r
  | _ -> true;;

let rec largest t =
  match t with
  | Leaf -> failwith "largest: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> largest r;;

let rec smallest t =
  match t with
  | Leaf -> failwith "smallest: empty tree"
  | Node (x, Leaf, _) -> x
  | Node (_, l, _) -> smallest l;;

let rec delete x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when x < x' ->
    Node (x', delete x l, r)
  | Node (x', l, r) when x > x' ->
    Node (x', l, delete x r)
  | Node (_, l, Leaf) -> l
  | Node (_, Leaf, r) -> r
  | Node (_, l, r) ->
    let succ = largest l in
    Node (succ, delete succ l, r);;
