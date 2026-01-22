open Graphics;;

let window_w = 800.;;
let window_h = 800.;;

let screen_space (x, y) =
  (
    int_of_float ((x +. 1.) /. 2. *. window_w),
    int_of_float ((1. -. (y +. 1.) /. 2.) *. window_h)
  );;

let project (x, y, z) =
  (x /. z, y /. z);;

let rotate_y (x, y, z) angle =
  let ox = x in
  let oz = z in
  (
    ox *. cos(angle) -. oz *. sin(angle),
    y,
    ox *. sin(angle) +. oz *. cos(angle)
  );;

let rotate_x (x, y, z) angle =
  let oy = y in
  let oz = z in
  (
    x,
    oy *. cos(angle) -. oz *. sin(angle),
    oy *. sin(angle) +. oz *. cos(angle)
  );;

let translate_z (x, y, z) dz =
  (x, y +. 0.175, z +. dz);;

(* CUBE *)
let vertecies = [
  (0.3, 0.3, 0.3);
  (-0.3, 0.3, 0.3);
  (-0.3, -0.3, 0.3);
  ( 0.3, -0.3, 0.3);
  (0.3, 0.3, -0.3);
  (-0.3, 0.3, -0.3);
  (-0.3, -0.3, -0.3);
  (0.3, -0.3, -0.3)
];;

let faces = [
  (0, 1, 2, 3); (* front *)
  (4, 5, 6, 7); (* back *)
  (0, 1, 5, 4); (* top *)
  (2, 3, 7, 6)  (* bottom *)
];;

let draw_line va vb =
  let va = project va in
  let vb = project vb in

  let (x1, y1) = screen_space va in
  let (x2, y2) = screen_space vb in

  moveto x1 y1;
  lineto x2 y2;
  ();;

let rec get_vertex v i =
  match v with
  | [] -> (0., 0., 0.)
  | x :: xs ->
    if i = 0 then x
    else get_vertex xs (i - 1);;

let draw_face (a, b, c, d) angle =
  let va = (get_vertex vertecies a) in
  let vb = (get_vertex vertecies b) in
  let vc = (get_vertex vertecies c) in
  let vd = (get_vertex vertecies d) in

  let va = rotate_y va angle in
  let vb = rotate_y vb angle in
  let vc = rotate_y vc angle in
  let vd = rotate_y vd angle in

  let va = rotate_x va angle in
  let vb = rotate_x vb angle in
  let vc = rotate_x vc angle in
  let vd = rotate_x vd angle in

  let va = translate_z va 1.25 in
  let vb = translate_z vb 1.25 in
  let vc = translate_z vc 1.25 in
  let vd = translate_z vd 1.25 in

  draw_line va vb;
  draw_line vb vc;
  draw_line vc vd;
  draw_line vd va;;

let rec draw_cube faces angle =
  match faces with
  | [] -> ()
  | (a, b, c, d) :: fs ->
    draw_face (a, b, c, d) angle;
    draw_cube fs angle;;
(* END CUBE *)

(* SPHERE *)
let stacks = 10;;
let slices = 12;;
let radius = 0.175;;

let gen_sphere_vertecies st sl r =
  let rec gen_slice j sp cp sv =
    if j = -1 then sv
    else
      let theta = 2. *. 3.1415926 *. float_of_int j /. float_of_int (sl) in
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

let s_vertecies = gen_sphere_vertecies stacks slices radius;;

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
      if j = (sl - 1) then e
      else
        iter_slices (j + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_slices 0 [] in
    draw_ellipse e true;
    if i = st - 1 then ()
    else render_slices (i + 1)
  in

  let rec render_stacks j =
    let rec iter_stacks i e =
      if i = (st - 1) then e
      else iter_stacks (i + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_stacks 0 [] in
    draw_ellipse e false;
    if j = sl - 1 then ()
    else render_stacks (j + 1)
  in

  render_slices 0;
  render_stacks 0;;

let list_translate_z l =
  let rec translate r n =
    match r with
    | [] -> n
    | x :: xs -> translate (xs) (translate_z x 1. :: n)
  in

  translate l [];;

let list_rotate_y l angle =
  let rec translate r n =
    match r with
    | [] -> n
    | x :: xs -> translate (xs) (rotate_y x angle :: n)
  in

  translate l [];;

(* END SPHERE *)

let rec animate angle sv =
  clear_graph ();

  set_color (rgb 0 0 0);
  fill_rect 0 0 800 800;

  set_color cyan;

  (* draw_cube faces angle; *)
  draw_sphere sv stacks slices;

  Graphics.synchronize ();

  animate (angle +. 0.0005) (list_translate_z (list_rotate_y (gen_sphere_vertecies 10 12 0.175) angle));;

let () =
  open_graph " 800x800+560+140";

  auto_synchronize false;

  animate 0. s_vertecies;

  close_graph ()
