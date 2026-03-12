type 'a infstream = Cons of 'a * (unit -> 'a infstream)

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (t ())

let rec from n = Cons (n, fun () -> from (n + 1))

let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ()))
  else filter f (t ())

let primes =
  let rec sieve (Cons (h, t)) =
    Cons (h, fun () ->
      sieve (filter (fun x -> x mod h <> 0) (t ()))
    )
  in
  sieve @@ from 2;;

take 100 primes;;
