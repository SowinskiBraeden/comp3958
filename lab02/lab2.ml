(** [fold_right f l acc] applies function [f] to each element of list [l]
  * from right to left, adding to the accumulator [acc].
  * The function [f] takes the current element and accumulator
  * and produces a new accumulator.
  *)
let rec fold_right f l acc =
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc);;

(**/**)
let test_fold_right () =
  assert (fold_right (+) [] 0 = 0);
  assert (fold_right (+) [2] 0 = 2);
  assert (fold_right (+) [1; 2; 3; 4] 0 = 10)
(**/**)

(** [map f l] for each element in list [l] apply func [f]
  * return list of [l] with func [f] applied to elems *)
let map f l = fold_right (fun x acc -> f x :: acc) l [];;

(**/**)
let test_map () =
  assert (map (fun x -> x * x) [] = []);
  assert (map (fun x -> x * x) [5] = [25]);
  assert (map (fun x -> x * x) [1; 2; 3] = [1; 4; 9]);
  assert (map (fun x -> x + x) [1; 2; 3] = [2; 4; 6]);
  assert (map (fun x -> 2. ** x) [0.; 1.; 2.; 3.; 4.; 5.] = [1.; 2.; 4.; 8.; 16.; 32.])
(**/**)

(** [dedup l] takes in a list [l] and collapses consecutive duplicated
  * elements into a single element *)
let dedup l = fold_right (fun x acc ->
  match acc with
  | y :: ys when y = x -> acc
  | _ -> x :: acc
) l [];;

(**/**)
let test_dedup () =
  assert (dedup [] = []);
  assert (dedup [1] = [1]);
  assert (dedup [1; 2] = [1; 2]);
  assert (dedup [1; 1; 2; 2; 2; 1; 3; 3; 2] = [1; 2; 1; 3; 2]);
  assert (dedup [1; 1; 2; 2; 2; 1; 3; 3; 2; 4] = [1; 2; 1; 3; 2; 4])
(**/**)

(** [reverse_tr l] returns the reverse order of list [l]; tail recursive *)
let reverse l =
  let rec reverse' acc l =
    match l with
    | [] -> acc
    | x :: xs -> reverse' (x :: acc) xs
  in
  reverse' [] l;;

(**/**)
let test_reverse () =
  assert (reverse [] = []);
  assert (reverse [1] = [1]);
  assert (reverse [1; 2] = [2; 1]);
  assert (reverse [1; 2; 3] = [3; 2; 1])
(**/**)

(** [filteri f l] for each element in list [l] keep
  * element if it passes predicate func [f] where
  * the predicate [f] takes an index i and elem x *)
let filteri f l =
  let rec filteri' i l acc =
    match l with
    | [] -> reverse acc
    | x :: xs when f i x -> filteri' (i + 1) xs (x :: acc)
    | _ :: xs -> filteri' (i + 1) xs acc
  in
  filteri' 0 l [];;

(**/**)
let test_filteri () =
  assert (filteri (fun i x -> i > 3 && x mod 2 = 0) [1; 2; 3; 4; 5; 6] = [6]);
  assert (filteri (fun i x -> i < 3 && x mod 2 != 0) [1; 2; 3; 4; 5; 6] = [1; 3]);
  assert (filteri (fun i x -> i != 1 && x >= 0) [-1; 0; 1; 2] = [1; 2])
(**/**)

(** [filteri f l] for each element in list [l] keep
  * element if it passes predicate func [f] where
  * the predicate [f] takes an element x *)
let filter f l = filteri (fun _ x -> f x) l;;

(**/**)
let test_filter () =
  assert (filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6] = [2; 4; 6]);
  assert (filter (fun x -> x mod 2 != 0) [1; 2; 3; 4; 5; 6] = [1; 3; 5]);
  assert (filter (fun x -> x > 0) [-1; 0; 1; 2] = [1; 2])
(**/**)

(** [every n l] returns a list of elements containing
    every [n]th elemnt from list [l]
    Required: [n] > 0 *)
let every n l = filteri (fun i _ -> (i + 1) mod n = 0) l;;

(**/**)
let test_every () =
  assert (every 1 [] = []);
  assert (every 2 [1] = []);
  assert (every 3 [1;2;3;4;5;6;7;8;9;10] = [3;6;9]);
  assert (every 2 [1;2;3;4;5;6;7;8] = [2;4;6;8])
(**/**)

(** [fold_while f acc l] folds over [l] from left to right using [f]
    and accumulator [acc], stopping early if [f] returns [None]. *)
let rec fold_while f acc l =
  match l with
  | [] -> acc
  | x :: xs ->
    match f acc x with
    | None -> acc
    | Some acc' -> fold_while f acc' xs;;

(**/**)
let test_fold_while () =
  let p = (fun acc x ->
    if acc + x > 10 then None
    else Some (acc + x)
  ) in

  assert (fold_while p 0 [] = 0);
  assert (fold_while p 0 [1;2;3;4] = 10);
  assert (fold_while p 0 [5;5;5] = 10);
  assert (fold_while p 0 [2;2;2;2;2;2] = 10);
  assert (fold_while p 0 [20;1;2;3] = 0)
(**/**)

(** [fold_left f acc l] applies function [f] to each element of list [l]
  * from left to right, adding to the accumulator [acc].
  * The function [f] takes the current element and accumulator
  * and produces a new accumulator.
  *)
let fold_left f acc l = fold_while (fun acc x -> Some (f acc x)) acc l;;

(**/**)
let test_fold_left () =
  assert (fold_left (+) 0 [] = 0);
  assert (fold_left (+) 0 [1;2;3;4;5] = 15);
  assert (fold_left ( * ) 1 [1;2;3;4] = 24);
  assert (fold_left (-) 0 [1;2;3] = -6);
  assert (fold_left (fun acc x -> acc ^ x) "" ["a"; "b"; "c"] = "abc")
(**/**)

(** [sum_while_less_than n l] takes a list of integers [l] and
    and maximum value [n]. Where it sums elements of the list [l]
    until the sum reaches a maximum [n] and returns a pair of the
    count and the sum.
  *)
let sum_while_less_than n l =
  let sum (c, acc) x =
    if acc + x >= n then None
    else Some (c + 1, acc + x)
  in
  fold_while sum (0, 0) l;;

(**/**)
let test_sum_while_less_than () =
  assert (sum_while_less_than 0 [6; 5; 5; 3; 4] = (0, 0));
  assert (sum_while_less_than 20 [6; 5; 5; 3; 4] = (4, 19));
  assert (sum_while_less_than 6 [6; 5; 5; 3; 4] = (0, 0));
  assert (sum_while_less_than 6 [] = (0, 0))
(**/**)

(**/**)
let run_all_tests() =
  test_fold_left();
  test_fold_right();
  test_map();
  test_dedup();
  test_reverse();
  test_filteri();
  test_filter();
  test_every();
  test_fold_while();
  test_sum_while_less_than()
(**/**)
