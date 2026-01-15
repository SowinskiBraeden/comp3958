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

(** [zip l1 l2] combines elements from [l1] and [l2] into a
  * new list of tuples; non tail recursive
  *)
let rec zip l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> [] (* if l1 or l2 is empty, return empty *)
  | x1 :: xs1, x2 :: xs2 ->
    (x1, x2) :: zip xs1 xs2;;

(**/**)
let test_zip () =
  assert (zip [] [] = []);
  assert (zip [1] [] = []);
  assert (zip [] ['a'] = []);
  assert (zip [1; 2; 3] ['a'; 'b'] = [(1, 'a'); (2, 'b')]);
  assert (zip [1; 2; 3] ['a'; 'b'; 'c'] = [(1, 'a'); (2, 'b'); (3, 'c')])
(**/**)

(** [zip_tr l1 l2] combines elements from [l1] and [l2] into a
  * new list of tuples; tail recursive
  *)
let zip_tr l1 l2 =
  let rec zip_tr' acc l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> reverse_tr acc (* if l1 or l2 is empty, return empty *)
    | x1 :: xs1, x2 :: xs2 ->
      zip_tr' ((x1, x2) :: acc) xs1 xs2
  in
  zip_tr' [] l1 l2;;

(**/**)
let test_zip_tr () =
  assert (zip_tr [] [] = []);
  assert (zip_tr [1] [] = []);
  assert (zip_tr [] ['a'] = []);
  assert (zip_tr [1; 2; 3] ['a'; 'b'] = [(1, 'a'); (2, 'b')]);
  assert (zip_tr [1; 2; 3] ['a'; 'b'; 'c'] = [(1, 'a'); (2, 'b'); (3, 'c')])
(**/**)

(** [unzip l] takes in a list of tuples [l] where each tuple is
  * a pair, we seperate the pairs (x, y) into sepeate lists, ([x], [y])
  * and return a tuple of both lists; non tail recursive *)
let rec unzip l =
  match l with
  | [] -> ([], [])
  | (x, y) :: xys ->
    let (l1, l2) = unzip xys in
      x :: l1, y :: l2;;

(**/**)
let test_unzip () =
  assert (unzip [] = ([], []));
  assert (unzip [(1, 'a')] = ([1], ['a']));
  assert (unzip [(1, 'a'); (2, 'b')] = ([1; 2], ['a'; 'b']))
(**/**)

(** [unzip_tr l] takes in a list of tuples [l] where each tuple is
  * a pair, we seperate the pairs (x, y) into sepeate lists, ([x], [y])
  * and return a tuple of both lists; tail recursive *)
let unzip_tr l =
  let rec unzip_tr' (a1, a2) l =
    match l with
    | [] -> (a1, a2)
    | (x, y) :: xys ->
      unzip_tr' (x :: a1, y :: a2) xys
  in
  unzip_tr' ([], []) (reverse_tr l);;

(**/**)
let test_unzip_tr () =
  assert (unzip_tr [] = ([], []));
  assert (unzip_tr [(1, 'a')] = ([1], ['a']));
  assert (unzip_tr [(1, 'a'); (2, 'b')] = ([1; 2], ['a'; 'b']))
(**/**)

(** [dedup l] takes in a list [l] and collapses consecutive duplicated
  * elements into a single element; non tail recursive *)
let rec dedup l =
  match l with
  | [] -> []
  | [x] -> l
  | x :: y :: zs ->
    if x = y then dedup (x :: zs)
    else x :: dedup (y :: zs);;

(**/**)
let test_dedup () =
  assert (dedup [] = []);
  assert (dedup [1] = [1]);
  assert (dedup [1; 2] = [1; 2]);
  assert (dedup [1; 1; 2; 2; 2; 1; 3; 3; 2] = [1; 2; 1; 3; 2])
(**/**)

(** [dedup l] takes in a list [l] and collapses consecutive duplicated
  * elements into a single element; tail recursive *)
let dedup_tr l =
  let rec dedup' acc l =
    match l with
    | [] -> acc
    | [x] -> l
    | x :: y :: zs ->
      if x = y then dedup' (x :: acc) (x :: zs)
      else x :: dedup' (x :: acc) (y :: zs)
  in
  dedup' [] l;;

(**/**)
let test_dedup_tr () =
  assert (dedup_tr [] = []);
  assert (dedup_tr [1] = [1]);
  assert (dedup_tr [1; 2] = [1; 2]);
  assert (dedup_tr [1; 1; 2; 2; 2; 1; 3; 3; 2] = [1; 2; 1; 3; 2])
(**/**)

(**/**)
let run_all_tests () =
  test_reverse();
  test_reverse_tr();
  test_zip();
  test_zip_tr();
  test_unzip();
  test_unzip_tr();
  test_dedup();
  test_dedup_tr();
(**/**)
