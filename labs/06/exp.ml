(** A lazy infinite stream containing values of type ['a]. *)
type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

(** [from n] creates an infinite lazy stream starting at [n]
  * and increasing by [1.] for each subsequent element.
  *)
let rec from n = Cons (n, lazy (from (n +. 1.)))

(** [take n s] returns a list containing the first [n] elements
  * of lazy stream [s].
  * If [n <= 0], it returns the empty list.
  *)
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (Lazy.force t)

(** [map f s] returns a new lazy stream where function [f]
  * is applied to every element of stream [s].
  *)
let rec map f (Cons (h, t)) =
  Cons (f h, lazy (map f (Lazy.force t)))

(** [fact n] computes the factorial of [n].
  * This function assumes [n] is a non-negative floating-point integer value.
  *)
let fact n =
  let rec fact' acc i =
    if i = 0. then acc
    else fact' (i *. acc) (i -. 1.)
  in
  fact' 1. n;;

(** [fold_left f acc l] applies function [f] to each element of list [l]
  * from left to right, carrying an accumulator [acc].
  * The function [f] takes the current accumulator and element
  * and produces a new accumulator.
  *)
let rec fold_left f acc l =
  match l with
  | [] -> acc
  | a :: l' -> fold_left f (f acc a) l';;

(** [exp_terms x] returns an infinite lazy stream of terms in the
  * Taylor series expansion of [e^x], where each term is
  * [(x ** n) /. fact n] for [n = 0., 1., 2., ...].
  *)
let exp_terms x = map (fun n -> (x**n) /. (fact n)) @@ from 0.;;

(** [exp n x] approximates [e^x] by summing the first [n]
  * terms of the Taylor series expansion for [e^x].
  *)
let exp n x = fold_left (+.) 0. (take n @@ exp_terms x);;

exp 20 1.1;;
