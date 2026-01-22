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

let reverse_tr l =
  let rec reverse_tr' acc l =
    match l with
    | [] -> acc
    | x :: xs -> reverse_tr' (x :: acc) xs
  in
  reverse_tr' [] l;;

let write_string_list_to_file filename (lines : string list) : unit =
  Out_channel.with_open_text filename (fun oc ->
    List.iter (Printf.fprintf oc "%s\n") lines
  );;

let convert_to_string l =
  let rec convert r n =
    match r with
    | [] -> reverse_tr n
    | (x, y, z) :: xs ->
      let s = Printf.sprintf "(%f, %f, %f)" x y z in
      convert xs (s :: n)
  in
  convert l [];;

let () =
  let vertecies = gen_sphere_vertecies 10 12 0.175 in
  let file_path = "output.txt" in
  write_string_list_to_file file_path (convert_to_string vertecies);
  Printf.printf "Successfully wrote list to %s\n" file_path
