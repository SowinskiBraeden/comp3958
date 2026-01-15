(** [fact n] calculates the factorial of [n]; non tail recursive *)
let rec fact n =
  if n = 0. then 1.
  else n *. fact(n -. 1.);;

(**/**)
let test_fact () =
  assert (fact 0. = 1.);
  assert (fact 3. = 6.);
  assert (fact 5. = 120.)
(**/**)

(** [fact_tr n] calculates the factorial of [n]; tail recursive *)
let fact_tr n =
  let rec fact_tr' acc i =
    if i = 0. then acc
    else fact_tr' (i *. acc) (i -. 1.)
  in
  fact_tr' 1. n;;

(**/**)
let test_fact_tr () =
  assert (fact_tr 0. = 1.);
  assert (fact_tr 3. = 6.);
  assert (fact_tr 5. = 120.)
(**/**)

(** [pow_tr a b] calculates [a] to the power of [b]; non tail recursive *)
let rec pow a b =
  if b = 0. then 1.
  else a *. pow a (b -. 1.);;

(**/**)
let test_pow () =
  assert (pow 0. 1. = 0.);
  assert (pow 0. 5. = 0.);
  assert (pow 1. 0. = 1.);
  assert (pow 5. 0. = 1.);
  assert (pow 1. 1. = 1.);
  assert (pow 5. 1. = 5.);
  assert (pow 2. 3. = 8.)
(**/**)

(** [pow_tr a b] calculates [a] to the power of [b]; tail recursive *)
let pow_tr a b =
  let rec pow_tr' acc i =
    if i = 0. then acc
    else pow_tr' (acc *. a) (i -. 1.)
  in
  pow_tr' 1. b;;

(**/**)
let test_pow_tr () =
  assert (pow_tr 0. 1. = 0.);
  assert (pow_tr 0. 5. = 0.);
  assert (pow_tr 1. 0. = 1.);
  assert (pow_tr 5. 0. = 1.);
  assert (pow_tr 1. 1. = 1.);
  assert (pow_tr 5. 1. = 5.);
  assert (pow_tr 2. 3. = 8.)
(**/**)

(** [expo_tr n x] calculates the approximation of e to the power of [x];
  * with a max iteration detail of [n]; non tail recursive
  * Require: [n] >= 1
  *)
let rec expo n x =
  if n = 0 then 1.
  else pow x (float_of_int n) /. fact (float_of_int n) +.
    expo (n - 1) x;;

(**/**)
let test_expo () =
  let tolerance = 1e-6 in
  let diff = abs_float (expo 20 1. -. exp 1.) in
  assert (diff < tolerance);
  let diff = abs_float (expo 20 2. -. exp 2.) in
  assert (diff < tolerance);
  let diff = abs_float (expo 20 3. -. exp 3.) in
  assert (diff < tolerance)
(**/**)

(** [expo_tr n x] calculates the approximation of e to the power of [x];
  * with a max iteration detail of [n]; tail recursive
  * Require: [n] >= 1
  *)
let expo_tr n x =
  let rec expo_tr' acc i =
    if i = 0 then acc
    else expo_tr' (acc +. pow_tr x (float_of_int i) /.
      fact_tr (float_of_int i)) (i - 1)
  in
  expo_tr' 1. n;;

(**/**)
let test_expo_tr () =
  let tolerance = 1e-6 in
  let diff = abs_float (expo_tr 20 1. -. exp 1.) in
  assert (diff < tolerance);
  let diff = abs_float (expo_tr 20 2. -. exp 2.) in
  assert (diff < tolerance);
  let diff = abs_float (expo_tr 20 3. -. exp 3.) in
  assert (diff < tolerance)
(**/**)

(**/**)
let run_all_tests () =
  test_fact();
  test_fact_tr();
  test_pow();
  test_pow_tr();
  test_expo();
  test_expo_tr()
(**/**)
