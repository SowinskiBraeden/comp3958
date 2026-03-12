(** An infinite stream containing values of type ['a],
  * where the tail is produced by a thunk.
  *)
type 'a infstream = Cons of 'a * (unit -> 'a infstream)

(** [take n s] returns a list containing the first [n] elements
  * of infinite stream [s].
  * If [n <= 0], it returns the empty list.
  *)
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (t ())

(** [from n] creates an infinite stream of integers starting at [n]
  * and increasing by [1] for each subsequent element.
  *)
let rec from n = Cons (n, fun () -> from (n + 1))

(** [filter f s] returns a new infinite stream containing only
  * the elements of stream [s] that satisfy predicate [f].
  *)
let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ()))
  else filter f (t ())

(** [primes] is an infinite stream of prime numbers generated
  * using the sieve of Eratosthenes.
  *)
let primes =
  let rec sieve (Cons (h, t)) =
    Cons (h, fun () ->
      sieve (filter (fun x -> x mod h <> 0) (t ()))
    )
  in
  sieve @@ from 2;;

take 100 primes;;
