let () =
  let len = Array.length Sys.argv - 1 in
  for i = 1 to len do
    Printf.printf (if i <> len then "%s " else "%s\n") Sys.argv.(i)
  done;;

