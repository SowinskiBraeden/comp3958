module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type k
  type ('k, 'v) t
  exception Not_found

  val empty : (k, 'v) t
  val is_empty : (k, 'v) t -> bool
  val insert : k -> 'v -> (k, 'v) t -> (k, 'v) t
  val find_opt : k -> (k, 'v) t -> 'v option
  val delete : k -> (k, 'v) t -> (k, 'v) t
  val of_list : (k * 'v) list -> (k, 'v) t
  val size : (k, 'v) t -> int
  val find : k -> (k, 'v) t -> 'v
  val to_list : (k, 'v) t -> (k * 'v) list
  val to_string : ((k * 'v) -> string) -> (k, 'v) t -> string
end

module Make(Ord: OrderedType) = struct
  (** [k] is the key of a key-value pair *)
  type k = Ord.t

  (** [t] is a key-value tree where each N has a key, value, left and right *)
  type ('k, 'v) t = L | N of k * 'v * (k, 'v) t * (k, 'v) t;;

  (** [Not_found] exception if a given key cannot be found in the tree *)
  exception Not_found;;

  (** empty tree *)
  let empty = L;;

  (** [is_empty t] takes a kvtree [t] and checks if it is empty or not *)
  let is_empty t = t = L;;

  (** [insert k v t] takes a comparator [], key [k], value [v],
    * and a kvtree [t] to insert the key and value [k, v] into the tree [t]. *)
  let rec insert k v t =
    match t with
    | L -> N (k, v, L, L)
    | N (k', v', l, r) when Ord.compare k k' < 0 ->
      N (k', v', insert k v l, r)
    | N (k', v', l, r) when Ord.compare k k' > 0 ->
      N (k', v', l, insert k v r)
    | N (k', _, l, r) when Ord.compare k k' = 0 ->
      N (k, v, l, r)
    | _ -> t;;

  (** [find_opt k t] takes a key [k] and a kvtree [t] and returns
    * an Optional value found from the given key [k]. *)
  let rec find_opt k t =
    match t with
    | L -> None
    | N (k', _, l, _) when Ord.compare k k' < 0 ->
      find_opt k l
    | N (k', _, _, r) when Ord.compare k k' > 0 ->
      find_opt k r
    | N (_, v, _, _) -> Some v;;

  (** [largest t] finds the largest key in a given kvtree [t]
    * and retursn the key value pair [k, v] *)
  let rec largest t =
    match t with
    | L -> failwith "largest: empty tree"
    | N (k, v, _, L) -> (k, v)
    | N (_, _, _, r) -> largest r;;

  (** [smallest t] finds the smallest key in a given kvtree [t]
    * and retursn the key value pair [k, v] *)
  let rec smallest t =
    match t with
    | L -> failwith "smallest: empty tree"
    | N (k, v, L, _) -> (k, v)
    | N (_, _, l, _) -> smallest l;;

  (** [delete k t] takes a key [k] and deletes it from a given
    * kvtree [t]. *)
  let rec delete k t =
    match t with
    | L -> L
    | N (k', v, l, r) when Ord.compare k k' < 0 ->
      N (k', v, delete k l, r)
    | N (k', v, l, r) when Ord.compare k k' > 0 ->
      N (k', v, l, delete k r)
    | N (_, _, l, L) -> l
    | N (_, _, L, r) -> r
    | N (_, _, l, r) ->
      let (ks, vs) = largest l in
      N (ks, vs, delete ks l, r);;

  (** [of_list l] takes a list of pairs [l] and creates a
    * kvtree in the order specefied by the comparator [] to compare
    * keys *)
  let of_list  l =
    List.fold_left (fun acc (k, v) -> insert k v acc) L l;;

  (** [size t] takes a kvtree [t] and returns the size of the tree
    * i.e. number of Ns *)
  let rec size t =
    match t with
    | L -> 0
    | N (_, _, l, r) ->
      1 + size l + size r;;

  (** [find k t] takes a key [k] and a kvtree [t] and returns
    * a value found from the given key [k]. *)
  let rec find k t =
    match t with
    | L -> raise Not_found
    | N (k', _, l, _) when Ord.compare k k' < 0 ->
      find k l
    | N (k', _, _, r) when Ord.compare k k' > 0 ->
      find k r
    | N (_, v, _, _) -> v;;

  (** [to_list t] takes a key-value tree [t] and returns
    * a list representation of the key-value pairs *)
  let to_list t =
    let rec aux acc t =
      match t with
      | L -> acc
      | N (k, v, l, r) ->
        aux ((k, v) :: (aux acc r)) l
    in
    aux [] t;;

  (** [to_string f t] takes a function [f] to "convert" a
    * key-value pair to a string, and applies that to all
    * key-value pairs in a given tree [t]. *)
  let rec to_string f t =
    match t with
    | L -> "#"
    | N (k, v, l, r) ->
      Printf.sprintf "^(%s, %s, %s)" (f (k, v)) (to_string f l) (to_string f r);;

end
