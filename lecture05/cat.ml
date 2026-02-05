(* display content of file read via stdin *)
let rec cat () =
  try
    let line = read_line () in
    print_endline line;
    cat()
  with
  | _ -> ();; (* EOF return unit *)

(* similar to main function *)
let () =
  let rec aux () =
    try
      print_endline @@ read_line ();
      aux ()
      with
      | _ -> ()
  in
  aux()
  (* cat() *)

(**** build and test ****)
(*
  ocamlbuild cat.native
  ./cat.native < filename
*)
