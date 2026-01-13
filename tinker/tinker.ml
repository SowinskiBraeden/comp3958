let pow b e =
  let rec pow' acc i =
    if i = 0 then acc
    else pow' (acc * b) (i - 1)
  in
  pow' 1 e;;

let rec root n k g =
  let next = (1. /. k) *. ((k -. 1.) *. g +. n /. float_of_int (pow g (k - 1))) in
  if abs(next - g) < 0.00000000001 then next
  else root n k next;;

let float_pow b e =
  let rec float_pow' acc i =
    if i <= 0. then acc
    else float_pow' (acc *. b) (i -. 1.)
  in
  float_pow' 1. e;;
