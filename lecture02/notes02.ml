(*
  ***** TUPLES *****
  has a fixed number of elements
*)
let x = (1, 2.1, "hello");;

(*
  this is also a tuple, but this tuple
  is a different type to the tuple above
*)
let y = (1, 2.1, "hello", "goodbye");;

(*
  function with tuple argument

  accepts a tuple with 3 elements of int
*)
let add_tuple (x, y, z) = x + y + z;;

(*
  ***** PATTERN MATCHING *****
  where x gets deconstructed into a, b, and c
*)
let (a, b, c) = x;;

a;;
b;;
c;;

(*
  ***** LISTS *****

  lists are semi-colon seperated values of the same type
  lists are a recursive data type
*)
let nums = [1; 2; 3];; (* this is a valid list *)

(*
  warning -
  this is valid syntax but this is a list with a single
  element that is a tuple of 3 ints
*)
let nums = [1, 2, 3];;
(* results in - list : [(1, 2, 3)] *)

(*
  since lists are recursive data structures
  we can add elements to the front of the list
  as seen below

  this operation is called cons
*)
2 :: (1 :: []);;
2 :: 1 :: [];;

(*
  [1; 2; 3] === 1 :: 2 :: 3 :: [];;
*)

(* list of lists *)
[[1; 2]; [3; 4]];;

(* list concatonation *)
[1; 2] @ [3; 4];;

[];;
(*
  results in - : 'a list = []

  where 'a means its a type variable
  since lists are generic
*)

let l = [[1; 2]; [3]; []];;

(*
  pattern matching lists to extract 3
  pattern matching must be exhaustive
*)
let [_; [x]; _] = l;;

let data = [(1, 2, 'a'); (5, 3, 'b'); (4, 1, 'c'); (5, 0, 'd')];;
let _ :: (_, _, x) :: _ = data;;
x;;

(*
  recursive function
  with pattern matching to find
  length of list
*)

(**
  * {length l} returns the number of elements
  * in the list {l} - non tail recursive
  *)
let rec length l =
  match l with
  | [] -> 0
  | x :: xs -> 1 + length xs;;

(**
  * {length_tr l} returns the number of elements
  * in the list {l} - tail recursive
  *)
let length_tr l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
  in
  aux 0 l;;

(*
  GENERATE WEB DOCUMENTATION

  mkdir html
  ocamldoc -html -d html main.ml

*)

(*
  there is no void in ocaml,
  everything has to return a value
  all expressions have a value;

  if your function has no value to
  return, you still need to return something
  so return the unit value

  example:
  printf is like a utility to print to the
  console but does not need to return any value
  so it returns the unit value
*)
Printf.printf "Hello, world\n";;
