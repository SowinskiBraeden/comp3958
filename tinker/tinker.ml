(*** testing stuff ***)

let stacks = 10;;
let slices = 12;;

let gen_sphere_vertecies st sl r =
  let rec gen_slice j sp cp sv =
    if j = -1 then sv
    else
      let theta = 2. *. 3.1415926 *. float_of_int j /. float_of_int (slices) in
      let sinTheta = sin theta in
      let cosTheta = cos theta in
      gen_slice (j - 1) sp cp ((r *. cosTheta *. sp, r *. cp, r *. sinTheta *. sp) :: sv)
  in

  let rec gen_stack i sv =
    if i = -1 then sv
    else
      let phi = 3.1415926 *. float_of_int i /. float_of_int (stacks - i) in
      let sinPhi = sin phi in
      let cosPhi = cos phi in
      gen_stack (i - 1) (gen_slice (slices - 1) sinPhi cosPhi sv)
  in

  gen_stack (stacks - 1) [];;

let draw_line (x1, y1) (x2, y2) =
  Printf.printf "(%f, %f) -> (%f, %f)\n" x1 y1 x2 y2

let draw_ellipse ev o =
  let f = match ev with
  | [] -> (0., 0.)
  | (x, y, _) :: _ -> (x, y) in

  let rec iter_vertex (lx, ly) v =
    match v with
    | [] -> (lx, ly)
    | [(x, y, _)] -> draw_line (x, y) (lx, ly); (x, y)
    | [(x1, y1, _); (x2, y2, _)] -> draw_line (x1, y1) (x2, y2); (x2, y2)
    | (x1, y1, _) :: (x2, y2, _) :: vs ->
      draw_line (x1, y1) (x2, y2);
      iter_vertex (x2, y2) ((x2, y2, 0.) :: vs)
  in
  let l = iter_vertex (0., 0.) ev in

  if o = 0 then draw_line f l;;

let rec get_nth_vert e n =
  match e with
  | [] -> failwith "cannot find nth element"
  | x :: xs ->
    if n = 0 then x
    else get_nth_vert xs (n - 1);;
