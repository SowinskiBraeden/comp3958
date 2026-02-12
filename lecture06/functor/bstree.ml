module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val size : t -> int
  val height : t -> int
  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val of_list : elt list -> t
  val mem : elt -> t -> bool
  val delete : elt -> t -> t
end

module Make(Ord : OrderedType) = struct
  type elt = Ord.t;;
  type t = L | N of elt * t * t;;

  let rec size t =
    match t with
    | L -> 0
    | N (_, l, r) ->
      1 + size l + size r;;

  let rec height t =
    match t with
    | L -> 0
    | N (_, l, r) ->
      1 + max (height l) (height r);;

  let empty = L;;

  let is_empty t = t = L;;

  let rec insert x t =
    match t with
    | L -> N (x, L, L)
    | N (x', l, r) when Ord.compare x x' < 0 ->
      N (x', insert  x l, r)
    | N (x', l, r) when Ord.compare x x' > 0 ->
      N (x', l, insert  x r)
    | _ -> t;;

  let of_list l =
    List.fold_left (Fun.flip insert) L l;;

  let rec mem x t =
    match t with
    | L -> false
    | N (x', l, _) when Ord.compare x x' < 0 ->
      mem  x l
    | N (x', _, r) when Ord.compare x x' > 0 ->
      mem  x r
    | _ -> true;;

  let rec largest t =
    match t with
    | L -> failwith "largest: empty tree"
    | N (x, _, L) -> x
    | N (_, _, r) -> largest r;;

  let rec smallest t =
    match t with
    | L -> failwith "smallest: empty tree"
    | N (x, L, _) -> x
    | N (_, l, _) -> smallest l;;

  let rec delete x t =
    match t with
    | L -> L
    | N (x', l, r) when Ord.compare x x' < 0 ->
      N (x', delete  x l, r)
    | N (x', l, r) when Ord.compare x x' > 0 ->
      N (x', l, delete  x r)
    | N (_, L, L) -> L (* this does not need to be here, its a special case but ill leave it for clarity *)
    | N (_, l, L) -> l
    | N (_, L, r) -> r
    | N (_, l, r) ->
      let succ = largest l in
      N (succ, delete  succ l, r);;
end
