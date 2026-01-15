(*
  if everything is fine, test functions
  will return the unit value
*)

(** [length l] returns the number of elements in the list [l]; non tail recursive *)
let rec length l =
  match l with
  | [] -> 0
  | x :: xs -> 1 + length xs;;

(**/**)
let test_length () =
  assert (length [] = 0);
  assert (length [2] = 1);
  assert (length [5; 7; 8;] = 3)
(**/**)

(** [length_tr l] returns the number of elements in the list [l]; tail recursive *)
let length_tr l =
  let rec lenth_tr' acc l =
    match l with
    | [] -> acc
    | _ :: xs -> lenth_tr' (acc + 1) xs
  in
  lenth_tr' 0 l;;

(**/**)
let test_length_tr () =
  assert (length_tr [] = 0);
  assert (length_tr [2] = 1);
  assert (length_tr [5; 7; 8;] = 3)
(**/**)

(** [reverse l] returns the reverse order of list [l]; non tail recursive *)
let rec reverse l =
  match l with
  | [] -> []
  | x :: xs -> reverse xs @ [x];;

(**/**)
let test_reverse () =
  assert (reverse [] = []);
  assert (reverse [1] = [1]);
  assert (reverse [1; 2] = [2; 1])
(**/**)

(** [reverse_tr l] returns the reverse order of list [l]; tail recursive *)
let reverse_tr l =
  let rec reverse_tr' acc l =
    match l with
    | [] -> acc
    | x :: xs -> reverse_tr' (x :: acc) xs
  in
  reverse_tr' [] l;;

(**/**)
let test_reverse_tr () =
  assert (reverse_tr [] = []);
  assert (reverse_tr [1] = [1]);
  assert (reverse_tr [1; 2] = [2; 1])
(**/**)

(* list.rev is a built in reverse function *)

(** [take n l] returns a list containing the first [n] elements of [l];
  * return [] if (n <= 0); return [l] if [l has fewer elements than [n]];
  * non tail recursive *)
let rec take n l =
  if n <= 0 then []
  else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(**/**)
let test_take () =
  assert (take 0 [1; 2; 3;] = []);
  assert (take 3 [4; 5; 6; 7; 8; 9] = [4; 5; 6]);
  assert (take 5 [1; 2; 3] = [1; 2; 3])
(**/**)

(** [take_tr n l] returns a list containing the first [n] elements of [l];
  * return [] if (n <= 0); return [l] if [l has fewer elements than [n]];
  * tail recursive *)
let take_tr n l =
  let rec take_tr' acc n l =
    if n <= 0 then reverse_tr(acc)
    else
      match l with
      | [] -> acc
      | x :: xs -> x :: take_tr' (x :: acc) (n - 1) xs
  in
  take_tr' [] n l;;

(**/**)
let test_take_tr () =
  assert (take_tr 0 [1; 2; 3;] = []);
  assert (take_tr 3 [4; 5; 6; 7; 8; 9] = [4; 5; 6]);
  assert (take_tr 5 [1; 2; 3] = [1; 2; 3])
(**/**)

(** [every_other l] returns a list consisting of every other element of [l]
  * starting from the first element; non tail recursive *)
let rec every_other l =
  match l with
  | x :: _ :: xs -> every_other xs
  | _ -> l;;

(**/**)
let test_every_other () =
  assert (every_other [] = []);
  assert (every_other [1] = []);
  assert (every_other [1; 2] = [2]);
  assert (every_other [1; 2; 3] = [2]);
  assert (every_other [1; 2; 3; 4] = [2; 4])
(**/**)

(** [every_other_tr l] returns a list consisting of every other element of [l]
  * starting from the first element; tail recursive *)
let every_other_tr l =
  let rec every_other_tr' acc l =
    match l with
    | x :: _ :: xs -> every_other_tr' (x :: acc) xs
    | _ -> reverse_tr acc
  in
  every_other_tr' [] l;;

(**/**)
let test_every_other_tr () =
  assert (every_other_tr [] = []);
  assert (every_other_tr [1] = []);
  assert (every_other_tr [1; 2] = [2]);
  assert (every_other_tr [1; 2; 3] = [2]);
  assert (every_other_tr [1; 2; 3; 4] = [2; 4])
(**/**)

(** [sum l1 l2] returns a list consisting of the sum of corresponding integers
  * in [l1] and [l2]; non tail recursive *)
let rec sum l1 l2 =
  match l1, l2 with (* this is a tuple of (l1, l2) *)
  | [], _ | _, [] -> [] (* if l1 is empty or l2 is empty, return empty *)
  | x1 :: xs1, x2 :: xs2 ->
    (x1 + x2) :: sum xs1 xs2;;

(**/**)
let test_sum () =
  assert (sum [] [] = []);
  assert (sum [1] [] = []);
  assert (sum [] [1] = []);
  assert (sum [7] [8] = [15]);
  assert (sum [7; 3] [8; 8] = [15; 11]);
  assert (sum [7] [8; 8] = [15])
(**/**)

(** [sum_tr l1 l2] returns a list consisting of the sum of corresponding integers
  * in [l1] and [l2]; tail recursive *)
let sum_tr l1 l2 =
  let rec sum_tr' acc l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> reverse_tr acc
    | x1 :: xs1, x2 :: xs2 ->
      sum_tr' ((x1 + x2) :: acc) xs1 xs2
  in
  sum_tr' [] l1 l2;;

(**/**)
let test_sum_tr () =
  assert (sum_tr [] [] = []);
  assert (sum_tr [1] [] = []);
  assert (sum_tr [] [1] = []);
  assert (sum_tr [7] [8] = [15]);
  assert (sum_tr [7; 3] [8; 8] = [15; 11]);
  assert (sum_tr [7] [8; 8] = [15])
(**/**)

(** [count_change amt denoms] returns the number of ways of breaking up [amt]
  * into currencies with denominations specified by [denoms];
  * Require: elements of [denoms] must be positive *)
let rec count_change amt denoms =
  if amt < 0 then 0
  else if amt = 0 then 1
  else
    match denoms with
    | [] -> 0
    | d :: ds ->
      count_change (amt - d) denoms + count_change amt ds;; (* use/not-use d *)
