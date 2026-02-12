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

module Make(Ord: OrderedType) : S with type k = Ord.t
