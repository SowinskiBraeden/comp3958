(*** INFINITE STREAMS ***)

(* will blow the stack - infinite *)
let rec from n = n :: from (n + 1)

(* 1 + 1 is not calculated till we call f *)
let f () = 1 + 1

(* apply same logic to from to defer n + 1 *)
(* let rec from n = n :: (fun () -> from (n + 1)) *)

(* but this results in a type error, so we need to wrap it in a type variance *)
(* this will map the unit type of an anonymouse function to our infinite stream type *)
type 'a infstream = Cons of 'a * (unit -> 'a infstream)

let rec from n = Cons (n, (fun () -> from (n + 1)))

(* head and tail of infinite stream *)
let hd (Cons (h, t)) = h
let tl (Cons (h, t)) = t ()

(* natural number (starting from 1) *)
let nats = from 1;;
nats |> tl |> hd

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (t ())

let rec drop n (Cons (h, t) as s) =
  if n <= 0 then s
  else drop (n - 1) (t ())

let rec map f (Cons (h, t)) =
  Cons (f h, fun () -> map f (t ()))

let squares = map (fun x -> x * x) nats;;
take 10 squares

let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, fun () -> map2 f (t1 ()) (t2 ()))

let s = map2 (+) nats squares;;
take 10 s

let rec fibs =
  Cons (0, fun () -> Cons (1, fun () -> map2 (+) fibs (tl fibs)));;

take 20 fibs

let rec unfold f x =
  let (v, x') = f x in Cons (v, fun () -> unfold f x')

let fibs = unfold (fun (a, b) -> (a, (b, a + b))) (0, 1);;


(*** Lazy ***)
lazy (1 + 1)
let x = lazy (1 + 1);;
x;;
Lazy.force x;;
x;;

type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t;;

let hd (Cons (h, _)) = h;;
let tl (Cons (_, t)) = Lazy.force t;;
let rec from n = Cons (n, lazy (from (n + 1)));;

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (Lazy.force t);;

let nats = from 1;;
nats;;
take 10 nats;;

let rec drop n (Cons (h, t) as s) =
  if n <= 0 then s
  else drop (n - 1) (Lazy.force t);;

drop 5 nats;;
