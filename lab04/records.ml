type record = {firstname: string; lastname: string; score: int};;

let new_record f l s = {firstname = f; lastname = l; score = s};;

let rec print_records r =
  match r with
  | [] -> ()
  | x :: xs -> Printf.printf "%3d %s %s\n" x.score x.lastname x.firstname; print_records xs;;

(** [records_insert x l] inserts a record element [x] into
  * list of records [l]
  * Requires: [l] is in ascending order. *)
let rec records_insert x l =
  match l with
  | y :: ys when x.score < y.score -> y :: records_insert x ys
  | _ -> x :: l

(** [records_insertion_sort l] sorts list of records [l] in ascending order *)
let rec records_insertion_sort l =
  match l with
  | [] -> []
  | x :: xs -> records_insert x (records_insertion_sort xs)

let rec parse l =
  try
    Scanf.sscanf l " %s %s %s" (fun f l s ->
      match int_of_string_opt s with
      | Some v when v >= 0 && v <= 100 -> Some {firstname = f; lastname = l; score = v}
      | _ -> None
    )
  with
  | Scanf.Scan_failure _ -> None

let rec read_file acc ic =
  try
    match parse @@ input_line ic with
    | None -> read_file acc ic
    | Some v -> read_file (v :: acc) ic
  with
  | End_of_file -> close_in ic; acc

let () =
  if Array.length Sys.argv = 1 then
    Printf.printf "%s: expects 1 file input.\nUsage: \"%s <filename>\"\n" Sys.argv.(0) Sys.argv.(0)
  else
    let ic = open_in Sys.argv.(1) in
    print_records @@ records_insertion_sort @@ read_file [] ic;;
