type ('k, 'v) kvtree = Leaf | Node of 'k * 'v * ('k, 'v) kvtree * ('k, 'v) kvtree;;

let rec kvtree_size t =
  match t with
  | Leaf -> 0
  | Node (_, _, l, r) ->
    1 + kvtree_size l + kvtree_size r;;

let rec kvtree_height t =
  match t with
  | Leaf -> 0
  | Node (_, _, l, r) ->
    1 + max (kvtree_height l) (kvtree_height r);;

let kvtree_empty = Leaf;;

let kvtree_is_empty t = t = Leaf;;

let rec kvtree_insert ~cmp k v t =
  match t with
  | Leaf -> Node (k, v, Leaf, Leaf)
  | Node (k', v', l, r) when cmp k k' < 0 ->
    Node (k', v', kvtree_insert ~cmp k v l, r)
  | Node (k', v', l, r) when cmp k k' > 0 ->
    Node (k', v', l, kvtree_insert ~cmp k v r)
  | _ -> t;;

let kvtree_of_list ~cmp l =
  List.fold_left (fun acc (k, v) -> kvtree_insert ~cmp k v acc) Leaf l;;

let rec kvtree_mem ~cmp k t =
  match t with
  | Leaf -> false
  | Node (k', _, l, _) when cmp k k' < 0 ->
    kvtree_mem ~cmp k l
  | Node (k', _, _, r) when cmp k k' > 0 ->
    kvtree_mem ~cmp k r
  | _ -> true;;

let rec kvtree_largest t =
  match t with
  | Leaf -> failwith "kvtree_largest: empty tree"
  | Node (k, v, _, Leaf) -> (k, v)
  | Node (_, _, _, r) -> kvtree_largest r;;

let rec kvtree_smallest t =
  match t with
  | Leaf -> failwith "kvtree_smallest: empty tree"
  | Node (k, v, Leaf, _) -> (k, v)
  | Node (_, _, l, _) -> kvtree_smallest l;;

let rec kvtree_delete ~cmp k t =
  match t with
  | Leaf -> Leaf
  | Node (k', v, l, r) when cmp k k' < 0 ->
    Node (k', v, kvtree_delete ~cmp k l, r)
  | Node (k', v, l, r) when cmp k k' > 0 ->
    Node (k', v, l, kvtree_delete ~cmp k r)
  | Node (_, _, l, Leaf) -> l
  | Node (_, _, Leaf, r) -> r
  | Node (_, _, l, r) ->
    let (ks, vs) = kvtree_largest l in
    Node (ks, vs, kvtree_delete ~cmp ks l, r);;

let rec kvtree_find_opt ~cmp k t =
  match t with
  | Leaf -> None
  | Node (k', _, l, _) when cmp k k' < 0 ->
    kvtree_find_opt ~cmp k l
  | Node (k', _, _, r) when cmp k k' > 0 ->
    kvtree_find_opt ~cmp k r
  | Node (_, v, _, _) -> Some v;;
