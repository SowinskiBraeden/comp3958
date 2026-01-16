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
  (x, y, z +. dz);;

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

let draw_line (x1, y1) (x2, y2) =
  moveto x1 y1;
  lineto x2 y2;;

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

  let va = project va in
  let vb = project vb in
  let vc = project vc in
  let vd = project vd in

  let va = screen_space va in
  let vb = screen_space vb in
  let vc = screen_space vc in
  let vd = screen_space vd in

  draw_line va vb;
  draw_line vb vc;
  draw_line vc vd;
  draw_line vd va;;

let rec iter_faces faces angle =
  match faces with
  | [] -> ()
  | (a, b, c, d) :: fs ->
    draw_face (a, b, c, d) angle;
    iter_faces fs angle;;

let rec spin_cube angle =
  clear_graph ();

  set_color (rgb 0 0 0);
  fill_rect 0 0 800 800;

  set_color cyan;

  iter_faces faces angle;

  Graphics.synchronize ();

  spin_cube (angle +. 0.000075);;

let () =
  open_graph " 800x800+560+140";

  auto_synchronize false;

  spin_cube 0.;

  close_graph ()
