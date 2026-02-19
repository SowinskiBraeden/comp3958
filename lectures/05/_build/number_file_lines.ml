let rec number acc ic =
  try
    Printf.printf "%5d| %s\n" acc @@ input_line ic;
    number (acc + 1) ic
  with
  | _ -> if ic <> stdin then close_in ic else ()

let () =
  if Array.length Sys.argv = 1 then
    number 1 stdin
  else
    try
      number 1 @@ open_in Sys.argv.(1)
    with
    | Sys_error s -> Printf.eprintf "%s\n" s;;

