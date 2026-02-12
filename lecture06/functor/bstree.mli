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

module Make(Ord: OrderedType) : S with type elt = Ord.t
