let () =
  let rec aux sum =
    try
      aux @@ Scanf.scanf " %d" (fun x -> sum + x)
    with
    | Scanf.Scan_failure _ ->
      Scanf.scanf " %s" (fun _ -> ());
      aux sum
    | End_of_file -> sum
  in
  Printf.printf "%d\n" @@ aux 0;;

