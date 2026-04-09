(* easy example *)


let numbers = [42; 1337; 2112; 90125; 5040; 7; 1984];;

let rec cata op base l = 
  match l with
  | [] -> base
  | x :: xs -> op x (cata op base xs);;

cata (+) 0 numbers;;

(* or simply *)

List.fold_left (+) 0 numbers;;

(* medium example *)


List.fold_left (fun x y -> (x + y) mod 360) 0 numbers;;


(*
  also, you see this with boolean values,
  in other languages you may use the ternary operator
  while in ocaml you may use if ... then ... 

  this is also catamorphic because it reduces the 
  true/false case down to a single value 
*)


let cata_bool case true_case false_case =
    if case then true_case else false_case;;

let is_raining = true;;

Printf.printf "%s\n" @@ cata_bool is_raining "Use Umbrella" "Wear nice shoes";; 

(* tree example - hardish *)

type int_tree =
  | Leaf
  | Node of int * int_tree * int_tree;;

let rec tree_cata leaf_case op tree =
  match tree with
  | Leaf -> leaf_case
  | Node (x, left, right) ->
    op x
      (tree_cata leaf_case node_case left)
      (tree_cata leaf_case node_case right);;

let sum_tree tree =
  tree_cata 0 (fun x l r -> x + l + r) tree;;

let size_tree tree =
  tree_cata 0 (fun _ l r -> 1 + l + r) tree;;

let height_tree tree =
  tree_cata 0 (fun _ l r -> 1 + max l r) tree;;

