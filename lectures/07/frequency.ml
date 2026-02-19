module F = Map.Make(String)

let rec count map =
  let word = Scanf.scanf " %s" (fun w -> w) in
  if word = "" then map
  else count (F.update word (function
    | None -> Some 1
    | Some n -> Some (n + 1))
  map);;

let () =
  List.iter (fun (s, n) -> Printf.printf "%s: %d\n" s n) @@ count F.empty;;
