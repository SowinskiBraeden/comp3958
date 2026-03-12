type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

let rec from n = Cons (n, lazy (from (n +. 1.)))

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (Lazy.force t)

let rec map f (Cons (h, t)) =
  Cons (f h, lazy (map f (Lazy.force t)))

let fact n =
  let rec fact' acc i =
    if i = 0. then acc
    else fact' (i *. acc) (i -. 1.)
  in
  fact' 1. n;;

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | a :: l' -> fold_left f (f acc a) l';;

let exp_terms x = map (fun n -> (x**n) /. (fact n)) @@ from 0.;;

let exp n x = fold_left (+.) 0. (take n @@ exp_terms x);;

exp 20 1.1;;
