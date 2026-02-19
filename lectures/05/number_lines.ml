let () =
  let rec aux acc =
    try
      Printf.printf "%5d| %s\n" acc @@ read_line ();
      aux (acc + 1)
    with
    | _ -> ()
  in
  aux 1;;
