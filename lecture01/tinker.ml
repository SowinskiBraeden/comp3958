(*
  ocamlc -o tinker tinker.ml

  ^^^ compile command
*)

(* Comments are weird *)

(*
  concat takes to paramters a and b and concats them
   together while seperated by a space
*)
let concat a b = a ^ " " ^ b;;

(*
  describe maps the concat function to each element
  in the list to describe the element with "eat"
*)
let describe = List.map (fun f -> concat "eat" f);;


let full_name = concat "Braeden" "Sowinski";;
print_endline full_name;;

let gruit = ["apple"; "banana"; "grape"];;

(*
  takes in a list and iterates over elements
  to print_endline the element
*)
let print_list_string list =
  List.iter print_endline list;;

(*
  I think this acts like a main method,
  creates the describe list then iterates
  and passes to the print_list_string function
*)
let () =
  let eating = describe gruit in
  print_list_string eating;;
