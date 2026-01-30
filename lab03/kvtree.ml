(* default comparator for testing *)
let scmp = String.compare;;

(** [kvtree] is a key value tree where each node has a key, value, left and right *)
type ('k, 'v) kvtree = Leaf | Node of 'k * 'v * ('k, 'v) kvtree * ('k, 'v) kvtree;;

(* example of an empty kvtree *)
let kvtree_empty = Leaf;;

(** [kvtree_insert ~cmp k v t] takes a comparator [~cmp], key [k], value [v],
    and a kvtree [t] to insert the key and value [k, v] into the tree [t] in
    the order specefied by the comparator [~cmp] to compare keys *)
let rec kvtree_insert ~cmp k v t =
  match t with
  | Leaf -> Node (k, v, Leaf, Leaf)
  | Node (k', v', l, r) when cmp k k' < 0 ->
    Node (k', v', kvtree_insert ~cmp k v l, r)
  | Node (k', v', l, r) when cmp k k' > 0 ->
    Node (k', v', l, kvtree_insert ~cmp k v r)
  | _ -> t;;

(**/**)2
let test_kvtree_insert () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_insert ~cmp:scmp "Bread" 20 Leaf = a);
  assert (kvtree_insert ~cmp:scmp "Mitch" 22 a = b);
  assert (kvtree_insert ~cmp:scmp "Luka" 19 b = c)
(**/**)

(** [kvtree_of_list ~cmp l] takes a list of pairs [l] and creates a
    kvtree in the order specefied by the comparator [~cmp] to compare
    keys *)
let kvtree_of_list ~cmp l =
  List.fold_left (fun acc (k, v) -> kvtree_insert ~cmp k v acc) Leaf l;;

(**/**)
let test_kvtree_of_list () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  assert (kvtree_of_list ~cmp:scmp [] = Leaf);
  assert (kvtree_of_list ~cmp:scmp [("Bread", 20)] = a);
  assert (kvtree_of_list ~cmp:scmp [("Bread", 20); ("Mitch", 22)] = b)
(**/**)

(** [kvtree_is_empty t] takes a kvtree [t] and checks if it is empty or not *)
let kvtree_is_empty t = t = Leaf;;

(**/**)
let test_kvtree_is_empty () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  assert (kvtree_is_empty Leaf = true);
  assert (kvtree_is_empty a = false);
  assert (kvtree_is_empty b = false)
(**/**)

(** [kvtree_size t] takes a kvtree [t] and returns the size of the tree
    i.e. number of nodes *)
let rec kvtree_size t =
  match t with
  | Leaf -> 0
  | Node (_, _, l, r) ->
    1 + kvtree_size l + kvtree_size r;;

(**/**)
let test_kvtree_size () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_size Leaf = 0);
  assert (kvtree_size a = 1);
  assert (kvtree_size b = 2);
  assert (kvtree_size c = 3)
(**/**)

(** [kvtree_height t] takes a kvtree [t] and returns the height of
    the tree (how deep the nodes go down in the tree) *)
let rec kvtree_height t =
  match t with
  | Leaf -> 0
  | Node (_, _, l, r) ->
    1 + max (kvtree_height l) (kvtree_height r);;

(**/**)
let test_kvtree_height () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_height Leaf = 0);
  assert (kvtree_height a = 1);
  assert (kvtree_height b = 2);
  assert (kvtree_height c = 3)
(**/**)

(** [kvtree_mem ~cmp k t] checks if a given key [k] is in a given kvtree
    [t] in the order specified by the comparator [~cmp] *)
let rec kvtree_mem ~cmp k t =
  match t with
  | Leaf -> false
  | Node (k', _, l, _) when cmp k k' < 0 ->
    kvtree_mem ~cmp k l
  | Node (k', _, _, r) when cmp k k' > 0 ->
    kvtree_mem ~cmp k r
  | _ -> true;;

(**/**)
let test_kvtree_mem () =
  let a = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_mem ~cmp:scmp "Bread" Leaf = false);
  assert (kvtree_mem ~cmp:scmp "Bread" a = true);
  assert (kvtree_mem ~cmp:scmp "Mitch" a = true);
  assert (kvtree_mem ~cmp:scmp "Ryan" a  = false)
(**/**)

(** [kvtree_largest t] finds the largest key in a given kvtree [t]
    and retursn the key value pair [k, v] *)
let rec kvtree_largest t =
  match t with
  | Leaf -> failwith "kvtree_largest: empty tree"
  | Node (k, v, _, Leaf) -> (k, v)
  | Node (_, _, _, r) -> kvtree_largest r;;

(**/**)
(* largest by key not by value *)
let test_kvtree_largest () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_largest a = ("Bread", 20));
  assert (kvtree_largest b = ("Mitch", 22));
  assert (kvtree_largest c = ("Mitch", 22))
(**/**)

(** [kvtree_smallest t] finds the smallest key in a given kvtree [t]
    and retursn the key value pair [k, v] *)
let rec kvtree_smallest t =
  match t with
  | Leaf -> failwith "kvtree_smallest: empty tree"
  | Node (k, v, Leaf, _) -> (k, v)
  | Node (_, _, l, _) -> kvtree_smallest l;;

(**/**)
(* smallest by key not by value *)
let test_kvtree_smallest () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_smallest a = ("Bread", 20));
  assert (kvtree_smallest b = ("Bread", 20));
  assert (kvtree_smallest c = ("Bread", 20))
(**/**)

(** [kvtree_delete ~cmp k t] takes a key [k] and deletes it from a given
    kvtree [t] in the order specefied by the comparator [~cmp] *)
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

(**/**)
let test_kvtree_delete () =
  let a = Node ("Bread", 20, Leaf, Leaf) in
  let b = Node ("Bread", 20, Leaf, Node("Mitch", 22, Leaf, Leaf)) in
  let c = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  let d = Node ("Bread", 20, Leaf, Node("Luka", 19, Leaf, Leaf)) in
  assert (kvtree_delete ~cmp:scmp "Bread" a = Leaf);
  assert (kvtree_delete ~cmp:scmp "Mitch" c = d);
  assert (kvtree_delete ~cmp:scmp "Luka" c = b)
(**/**)

(** [kvtree_find_opt ~cmp k t] takes a key [k] and a kvtree [t] and returns
    and Optional value found from the given key [k]. Searching through the
    tree in the order specefied by the comparator [~cmp] *)
let rec kvtree_find_opt ~cmp k t =
  match t with
  | Leaf -> None
  | Node (k', _, l, _) when cmp k k' < 0 ->
    kvtree_find_opt ~cmp k l
  | Node (k', _, _, r) when cmp k k' > 0 ->
    kvtree_find_opt ~cmp k r
  | Node (_, v, _, _) -> Some v;;

(**/**)
let test_kvtree_find_opt () =
  let a = Node ("Bread", 20, Leaf, Node("Mitch", 22, Node("Luka", 19, Leaf, Leaf), Leaf)) in
  assert (kvtree_find_opt ~cmp:scmp "Luka" Leaf = None);
  assert (kvtree_find_opt ~cmp:scmp "Ryan" a = None);
  assert (kvtree_find_opt ~cmp:scmp "Bread" a = Some 20);
  assert (kvtree_find_opt ~cmp:scmp "Mitch" a = Some 22)
(**/**)

(**/**)
let run_all_tests () =
  test_kvtree_insert();
  test_kvtree_of_list();
  test_kvtree_is_empty();
  test_kvtree_size();
  test_kvtree_height();
  test_kvtree_mem();
  test_kvtree_largest();
  test_kvtree_smallest();
  test_kvtree_delete();
  test_kvtree_find_opt()
