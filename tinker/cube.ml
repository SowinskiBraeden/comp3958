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
  (
    x *. cos(angle) -. z *. sin(angle),
    y,
    x *. sin(angle) +. z *. cos(angle)
  );;

let rotate_x (x, y, z) angle =
  (
    x,
    y *. cos(angle) -. z *. sin(angle),
    y *. sin(angle) +. z *. cos(angle)
  );;

let rotate_z (x, y, z) angle =
  (
    x *. cos(angle) -. y *. sin(angle),
    x *. sin(angle) +. y *. cos(angle),
    z
  );;

let translate_x (x, y, z) dx =
  (x +. dx, y, z);;

let translate_y (x, y, z) dy =
  (x, y +. dy, z);;

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

let draw_face (a, b, c, d) v angle =
  let va = get_vertex v a in
  let vb = get_vertex v b in
  let vc = get_vertex v c in
  let vd = get_vertex v d in

  draw_line va vb;
  draw_line vb vc;
  draw_line vc vd;
  draw_line vd va;;

let rec draw_cube faces v angle =
  match faces with
  | [] -> ()
  | (a, b, c, d) :: fs ->
    draw_face (a, b, c, d) v angle;
    draw_cube fs v angle;;
(* END CUBE *)

(* SPHERE *)
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
      if j = sl then e
      else iter_slices (j + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_slices 0 [] in
    draw_ellipse e true;
    if i = st - 1 then ()
    else render_slices (i + 1)
  in

  let rec render_stacks j =
    let rec iter_stacks i e =
      if i = st then e
      else iter_stacks (i + 1) (get_nth_vert v (i * sl + j) :: e)
    in
    let e = iter_stacks 0 [] in
    draw_ellipse e false;
    if j = st + 1 then ()
    else render_stacks (j + 1)
  in

  render_slices 0;
  render_stacks 0;;

let list_transform t l d =
  let rec translate r n =
    match r with
    | [] -> n
    | x :: xs -> translate (xs) (t x d :: n)
  in

  translate l [];;

(* END SPHERE *)

let rec animate angle sv sqv =
  clear_graph ();

  set_color (rgb 0 0 0);
  fill_rect 0 0 (int_of_float window_w) (int_of_float window_h);

  set_color cyan;

  draw_cube faces sqv angle;
  draw_sphere sv stacks slices;

  Graphics.synchronize ();

  let nsv = gen_sphere_vertecies 10 12 0.175 in
  let nsv = list_transform rotate_y nsv (-1. *. angle) in
  let nsv = list_transform rotate_x nsv (-1. *. angle) in
  let nsv = list_transform translate_x nsv (sin angle /. -2.) in
  let nsv = list_transform translate_y nsv (cos angle /. -2.) in
  let nsv = list_transform translate_z nsv (1. +. sin(angle) /. 2.) in

  let nsqv = vertecies in
  let nsqv = list_transform rotate_y nsqv angle in
  let nsqv = list_transform rotate_x nsqv angle in
  let nsqv = list_transform translate_x nsqv (sin angle) in
  let nsqv = list_transform translate_y nsqv (cos angle) in
  let nsqv = list_transform translate_z nsqv (2. +. (sin (-1. *. angle)) /. 2.) in

  animate (angle +. 0.0005) nsv nsqv;;

let () =
  let size = " " ^ (string_of_float window_w) ^ "x" ^ (string_of_float window_h) ^ "+560+140" in
  Printf.printf "Size: %s\n" size;
  (* open_graph size; *)
  open_graph " 800x800+560+140";

  auto_synchronize false;

  animate 0. s_vertecies vertecies;

  close_graph ()
