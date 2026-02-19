let counter =
  let c = ref 0 in
  (fun () -> incr c; !c);;

let make_counter init =
  let c = ref init in
  (fun () -> let x = !c in incr c; x);;
