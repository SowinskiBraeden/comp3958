(** [digits n] takes a positive integer [n] and returns
    a list of integers of each digit in the number [n] *)
let digits n =
  let rec digits' n acc =
    if n = 0 then acc
    else digits' (n / 10) (n mod 10 :: acc)
  in
  digits' n [];;

(**/**)
let test_digits () =
  assert (digits 0 = []);
  assert (digits 123 = [1;2;3]);
  assert (digits 123040 = [1;2;3;0;4;0])
(**/**)

(** [int_of_digits d] takes a list of positive integers [d]
    and returns a single number where each digit is from the
    list [d]
  *)
let int_of_digits d =
  List.fold_left (fun x acc -> x * 10 + acc) 0 d;;

(**/**)
let test_int_of_digits () =
  assert (int_of_digits [] = 0);
  assert (int_of_digits [0] = 0);
  assert (int_of_digits [0;0;1;2;3] = 123);
  assert (int_of_digits [2;0;0;5] = 2005)
(**/**)

(**/**)
let run_all_tests () =
  test_digits();
  test_int_of_digits()
(**/**)
