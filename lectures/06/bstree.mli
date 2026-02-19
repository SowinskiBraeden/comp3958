(* type 'a t = Leaf | Node of 'a * 'a t * 'a t *)

type 'a t (* for abstract data type *)

val size : 'a t -> int
val height : 'a t -> int
val empty : 'a t
val is_empty : 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val mem : 'a -> 'a t -> bool
val largest : 'a t -> 'a
val smallest : 'a t -> 'a
val delete : 'a -> 'a t -> 'a t
