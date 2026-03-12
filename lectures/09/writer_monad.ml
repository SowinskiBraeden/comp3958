let return x = (x, "")

let ( >>= ) (x, s) f =
  let (x', s') = f x in
  (x', s  ^ s')

let ( >> ) mx my =
  mx >>= fun _ -> my

let square x =
  let y = x * x in
  (y, Printf.sprintf "square %d = %d" x y)

let inc x =
  let y = x + 1 in
  (y, Printf.sprintf "inc %d = %d" x y)

let dec x =
  let y = x - 1 in
  (y, Printf.sprintf "dec %d = %d" x y);;

return 2 >>= square >>= square >>= dec >>= square >>= inc;;

let tell s = ((), s);;

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;

gcd 32 24;;

let rec gcd_logged a b =
  if b = 0 then tell (Printf.sprintf "gcd = %d" a) >> return a
  else tell (Printf.sprintf "gcd %d %d: " a b) >> gcd_logged b (a mod b);;

gcd_logged 24 32;;
