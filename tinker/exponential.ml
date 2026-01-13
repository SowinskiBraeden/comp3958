let fact n =
  let rec fact' acc i =
    if i = 0. then acc
    else fact' (i *. acc) (i -. 1.)
  in
  fact' 1. n;;

let pow a b =
  let rec pow' acc i =
    if i = 0. then acc
    else pow' (acc *. a) (i -. 1.)
  in
  pow' 1. b;;

let expo n x =
  let rec expo' acc i =
    if i = 0. then acc
    else expo' (acc +. pow x i /. fact i) (i -. 1.)
  in
  expo' 1. n;;
