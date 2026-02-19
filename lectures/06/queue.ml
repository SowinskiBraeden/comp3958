type 'a t = 'a list;;

exception Empty

let empty = [];;

let is_empty q = q = [];;

let enqueue x q = q @ [x];;

let dequeue q =
  match q with
  | [] -> raise Empty
  | _ :: xs -> xs;;

let dequeue_opt q =
  match q with
  | [] -> None
  | _ :: xs -> Some xs;;

let front q =
  match q with
  | [] -> raise Empty
  | x :: _ -> x;;

let front_opt q =
  match q with
  | [] -> None
  | x :: _ -> Some x;;

let length q =
  List.length q;;

let of_list l = l;;

let to_list q = q;;

