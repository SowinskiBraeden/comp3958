type 'a infstream = Cons of 'a * (unit -> 'a infstream)

let hd (Cons (h, _)) = h
let tl (Cons (_, t)) = t ()

let rec from n = Cons (n, fun () -> from (n + 1))
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (t ())
