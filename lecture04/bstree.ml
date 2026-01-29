type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree;;

let rec bstree_size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 
    1 + bstree_size l + bstree_size r;;

let rec bstree_height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 
    1 + max (bstree_height l) (bstree_height r);;

let bstree_empty = Leaf;;

let bstree_is_empty t = t = Leaf;;

let rec bstree_insert ~cmp x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when cmp x x' < 0 ->
    Node (x', bstree_insert ~cmp x l, r)
  | Node (x', l, r) when cmp x x' > 0 ->
    Node (x', l, bstree_insert ~cmp x r)
  | _ -> t;;

let bstree_of_list ~cmp l =
  List.fold_left (Fun.flip (bstree_insert ~cmp)) Leaf l;;

let rec bstree_mem ~cmp x t =
  match t with
  | Leaf -> false
  | Node (x', l, _) when cmp x x' < 0 ->
    bstree_mem ~cmp x l
  | Node (x', _, r) when cmp x x' > 0 ->
    bstree_mem ~cmp x r
  | _ -> true;;

let rec bstree_largest t =
  match t with
  | Leaf -> failwith "bstree_largest: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> bstree_largest r;;

let rec bstree_smallest t =
  match t with
  | Leaf -> failwith "bstree_smallest: empty tree"
  | Node (x, Leaf, _) -> x
  | Node (_, l, _) -> bstree_smallest l;;

let rec bstree_delete ~cmp x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when cmp x x' < 0 ->
    Node (x', bstree_delete ~cmp x l, r)
  | Node (x', l, r) when cmp x x' > 0 ->
    Node (x', l, bstree_delete ~cmp x r)
  | Node (_, Leaf, Leaf) -> Leaf (* this does not need to be here, its a special case but ill leave it for clarity *)
  | Node (_, l, Leaf) -> l
  | Node (_, Leaf, r) -> r
  | Node (_, l, r) ->
    let succ = bstree_largest l in
    Node (succ, bstree_delete ~cmp succ l, r);;

