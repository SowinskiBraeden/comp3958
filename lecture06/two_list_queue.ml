(* queue is empty iff the first list is empty *)
type 'a t = 'a list * 'a list;;

exception Empty;;

let empty = ([], []);;

let is_empty (l, _) = l = [];;

let enqueue x (l1, l2) =
  match l1 with
  | [] -> [x], l2
  | _ -> (l1, x :: l2);;

let dequeue (l1, l2) =
  match l1 with
  | [] -> raise Empty
  | [_] -> (List.rev l2, [])
  | _ :: xs -> (xs, l2);;

let dequeue_opt q =
  try
    Some (dequeue q)
  with
  | Empty -> None;;

let front (l, _) =
  match l with
  | [] -> raise Empty
  | x :: _ -> x;;

let front_opt (l, _) =
  match l with
  | [] -> None
  | x :: _ -> Some x;;

let of_list l = 
  List.fold_left (Fun.flip enqueue) empty l;;

let to_list (l1, l2) =
  l1 @ List.rev l2;;

