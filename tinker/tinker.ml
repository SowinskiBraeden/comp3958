(*** testing stuff ***)

let stacks = 10;;
let slices = 12;;
let radius = 0.175;;

let gen_sphere_vertecies st sl r =
  let rec gen_slice j sp cp sv =
    if j = -1 then sv
    else
      let theta = 2. *. 3.1415926 *. float_of_int j /. float_of_int sl in
      let sinTheta = sin theta in
      let cosTheta = cos theta in
      gen_slice (j - 1) sp cp ((r *. cosTheta *. sp, r *. cp, r *. sinTheta *. sp) :: sv)
  in

  let rec gen_stack i sv =
    if i = -1 then sv
    else
      let phi = 3.1415926 *. float_of_int i /. float_of_int (st - 1) in
      let sinPhi = sin phi in
      let cosPhi = cos phi in
      gen_stack (i - 1) (gen_slice (sl - 1) sinPhi cosPhi sv)
  in

  gen_stack (st - 1) [];;

let draw_line (x1, y1, z1) (x2, y2, z2) =
  Printf.printf "(%f, %f, %f) -> (%f, %f, %f)\n" x1 y1 z1 x2 y2 z2

let draw_ellipse ev o =
  let f = match ev with
  | [] -> (0., 0., 0.)
  | a :: _ -> a in

  let rec iter_vertex l v =
    match v with
    | [] -> l
    | [a] -> draw_line a l; a
    | [a; b] -> draw_line a b; b
    | a :: b :: vs ->
      draw_line a b;
      iter_vertex b (b :: vs)
  in
  let l = iter_vertex (0., 0., 0.) ev in

  if o then draw_line f l;;

let rec get_nth_vert e n =
  match e with
  | [] -> failwith "cannot find nth vertex"
  | x :: xs ->
    if n = 0 then x
    else get_nth_vert xs (n - 1);;

let draw_sphere v st sl =
  let rec render_slices i =
    let rec iter_slices j e =
      if j = 1 then e
      else iter_slices (j + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_slices 0 [] in
    draw_ellipse e true;
    if i = st - 1 then ()
    else render_slices (i + 1)
  in

  let rec render_stacks j =
    let rec iter_stacks i e =
      if i = 1 then e
      else iter_stacks (i + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_stacks 0 [] in
    draw_ellipse e false;
    if j = sl - 1 then ()
    else render_stacks (j + 1)
  in

  render_slices 0;;
  (* render_stacks 0;; *)

let s_vertecies = gen_sphere_vertecies stacks slices radius;;
draw_sphere s_vertecies stacks slices;;
