let return x = [x]

let ( >>= ) l f = List.concat_map f l

let gaurd cond l =
  if cond then l else []

let multiply_to n =
  List.init n ((+) 1) >>= fun x ->
    List.init n ((+) 1) >>= fun y ->
      gaurd (x * y = n) [(x, y)]

let ( let* ) = ( >>= )

let multiply_to' n =
  let* x = List.init n ((+) 1) in
  let* y = List.init n ((+) 1) in
  if x * y = n then [(x, y)] else []
