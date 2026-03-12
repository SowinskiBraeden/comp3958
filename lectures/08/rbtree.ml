type color = R | B
type 'a t = E | N of (color * 'a t * 'a * 'a t)

let balance = function
  | B, N (R, N (R, a, x, b), y, c), z, d
  | B, N (R, a, x, N (R, b, y, c)), z, d
  | B, a, x, N (R, N (R, b, y, c), z, d)
  | B, a, x, N (R, b, y, N (R, c, z, d)) ->
      N (R, N (B, a, x, b), y, N (B, c, z, d))
  | c, l, x, r ->
      N (c, l, x, r)

let rec insert x t =
  let rec ins = function
    | E -> N (R, E, x, E)
    | N (c, l, y, r) when x < y ->
        balance (c, ins l, y, r)
    | N (c, l, y, r) when x > y ->
        balance (c, l, y, ins r)
    | t -> t
  in
  match ins t with
  | E -> failwith "insert: impossible"
  | N (_, l, y, r) -> N (B, l, y, r)

let of_list l =
  List.fold_left (Fun.flip insert) E l

let print pr t =
  let rec aux level t =
    Printf.printf "%*s" (3 * level) "";
    match t with
    | E -> Printf.printf "-\n"
    | N (c, l, x, r) ->
        if c = R then (
          Printf.printf "\u{1b}[1;31m";
          pr x;
          Printf.printf "\u{1b}[1;0m\n"
        )
        else (
          pr x;
          Printf.printf "\n"
        );
        aux (level + 1) l;
        aux (level + 1) r
  in
  aux 0 t
