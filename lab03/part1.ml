let digits n =
  let rec digits' n acc =
    if n = 0 then acc
    else digits' (n / 10) (n mod 10 :: acc)
  in
  digits' n [];;

let int_of_digits d =
  (List.fold_left (fun x acc -> (x + acc) * 10) 0 d) / 10;;
