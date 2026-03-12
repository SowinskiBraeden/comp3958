type bstree = L | N of int * bstree * bstree

let rec insert x t =
  match t with
  | L -> N (x, L, L)
  | N (x', l, r) ->
    if x < x' then N (x', insert x l, r)
    else if x > x' then N (x', l, insert x r)
    else t

let of_list l = List.fold_left (Fun.flip insert) L l

(* val right : bstree -> bstree option *)
let right t =
  match t with
  | L -> None
  | N (_, _, r) -> Some r

(* val left : bstree -> bstree option *)
let left t =
  match t with
  | L -> None
  | N (_, l, _) -> Some l

let right_left t =
  match right t with
  | None -> None
  | Some r -> left r

(* maybe monad *)
let bind mt f =
  match mt with
  | None -> None
  | Some t -> f t

let ( >>= ) = bind

let t = of_list [3;2;7;6;8];;
t |> right >>= left;;
t |> right >>= left >>= right >>= left;;

let return t = Some t;;

return t >>= right >>= left;;
