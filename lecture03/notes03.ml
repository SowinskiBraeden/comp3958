(* PARTIAL FUNCTIONS - because it does not return a value in
   some cases *)

let hd l =
  match l with
  | [] -> failwith "hd: empty list"
  | x:: _ -> x;;

let tl l =
  match l with
  | [] -> failwith "tl: empty list"
  | _ :: xs -> xs;;

(* associative list is a list of pairs *)
let al = [("a123", 55); ("b456", 67)];;

(** [find k l] returns a value v from a key [k] in a list of pairs [l]
  * fails if not found
  *)
let rec find k l =
  match l with
  | [] -> failwith "find: key not found"
  | (k', v') :: _ when k = k' -> v'
  | _ :: xs -> find k xs;;

(*
  best case is to have a TOTAL FUNCTION, always return a value
  this can be accomplished with an option type, that has two
  variants, either None or Some e.g. None;; Some 2;;
*)

(** [find_opt k l] returns an optional value v from a key [k] in a list of pairs [l] *)
let rec find_opt k l =
  match l with
  | [] -> None
  | (k', v') :: _ when k = k' -> Some v'
  | _ :: xs -> find_opt k xs;;

(*** HIGHER ORDER FUNCTIONS ***)

(** [insert x l] inserts element [x] into list [l]
  * Requires: [l] is in ascending order. *)
let rec insert x l =
  match l with
  | [] -> [x]
  | y :: ys ->
    if x <= y then x :: l
    else y :: insert x ys;;

(* Note: the following insert is the same logically, just shorter *)
(** [insert x l] inserts element [x] into list [l]
  * Requires: [l] is in ascending order. *)
let rec insert x l =
  match l with
  | y :: ys when x > y -> y :: insert x ys
  | _ -> x :: l

(** [insertion_sort l] sorts list [l] in ascending order *)
let rec insertion_sort l =
  match l with
  | [] -> []
  | x :: xs -> insert x (insertion_sort xs)

(*** ANONYMOUS FUNCTIONS ***)
(* fun x -> x * x;; <-- syntax*)
let f' x = x * x;;

(* these functions are equivalent *)
let a = fun x y -> x + y;; (* func that takes two params *)
let a' x = fun y -> x + y;; (* func taht takes 1 param and returns a func that takes 1 param *)

let s x = x * x;;
(* f 2 + 3;;    (* result: 7 *)
f (2 + 3);;  (* result: 25 *)
f @@ 2 + 3;; (* result: 25 *) *)

let rec insertion_sort l =
  match l with
  | [] -> []
  | x :: xs -> insert x @@ insertion_sort xs;;

(* function is short form of "l = match l with ..." *)
let rec insertion_sort = function
  | [] -> []
  | x :: xs -> insert x @@ insertion_sort xs;;

let inc x = x + 1;;
let square x = x * x;;
let add x y = x + y;;

square (inc 1);;   (* r = 4 *)
square @@ inc 1;;  (* r = 4 *)
1 |> inc |> square;; (* pipe operator - r = 4 or <|*)
1 |> inc |> square |> add 2;; (* r = 6 *)

let flip f x y = f y x;;
let sub x y = x - y;;
flip sub 1 2;; (* flips two arguments, becomes sub 2 1 *)

(** [map f l] for each element in list [l] apply func [f]
  * return list of [l] with func [f] applied to elems *)
let rec map f l =
  match l with
  | [] -> []
  | x :: xs -> f x :: map f xs;;

map (fun x -> x * x) [1; 2; 3; 4; 5; 6];;
map int_of_string_opt ["1"; "2"; "3"; "z"];;

(** [filter f l] for each element in list [l] keep
  * element if it passes predicate func [f] *)
let rec filter f l =
  match l with
  | [] -> []
  | x :: xs when f x -> x :: filter f xs
  | _ :: xs -> filter f xs;;

filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9];;
["a"; "1"; "2"] |> map int_of_string_opt |> filter (function | None -> false | Some _ -> true) (* filter out None *)
(* list gets piped into the snd arg of map and result of map is piped to snd arg of filter *)
(* pipes can go left also? <| *)

(** [take_while f l] returns the longest prefix of [l] each of its elements
  * satisfying [f] *)
let take_while f l =
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
    if f x then aux xs (x :: acc)
    else List.rev acc
  in
  aux l [];;

take_while (fun x -> x mod 2 = 0) [3; 2; 6; 6; 8];;
take_while (fun x -> x mod 2 = 0) [2; 6; 7; 6; 8];;
take_while (fun x -> x mod 2 = 0) [2; 6; 6; 8];;

(** [take_while f l] returns the longest prefix of [l] each of its elements
  * satisfying [f] *)
let take_while f l =
  let rec aux l acc =
    match l with
    | x :: xs when f x -> aux xs (x :: acc)
    | _ -> List.rev acc
  in
  aux l [];;

take_while (fun x -> x mod 2 = 0) [3; 2; 6; 6; 8];;
take_while (fun x -> x mod 2 = 0) [2; 6; 7; 6; 8];;
take_while (fun x -> x mod 2 = 0) [2; 6; 6; 8];;



(*
  cmp x y < 0 x before y
          = 0 doesnt matter, x = y
          > 0 x after y
*)

let rec insert' cmp x l =
  match l with
  | [] -> [x]
  | y :: ys ->
    if cmp x y <= 0 then x :: l
    else y :: insert' cmp x ys;;

let rec sort' cmp l =
  match l with
  | [] -> []
  | x :: xs ->
    insert' cmp x @@ sort' cmp xs;;

sort' Int.compare [3;2;7;6;8];;
sort' (Fun.flip Int.compare) [3;2;7;6;8];;

(* with labeled arguments *)
let rec insert'' ~cmp x l =
  match l with
  | [] -> [x]
  | y :: ys ->
    if cmp x y <= 0 then x :: l
    else y :: insert'' ~cmp x ys;;

let rec sort'' ~cmp l =
  match l with
  | [] -> []
  | x :: xs ->
    insert'' ~cmp x @@ sort'' ~cmp xs;;

(* labeled arguments can go anywhere *)
sort'' [3;2;7;6;8] ~cmp:Int.compare;;
sort'' ~cmp:(Fun.flip Int.compare) [3;2;7;6;8];;

(* process list from left to right *)
(* fold_left -> (((acc $ x1) $ x2) $ x3) *)
let rec fold_left f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs;;

fold_left (fun acc x -> acc + x) 0 [3; 2; 7; 6; 8];;
fold_left (+) 0 [3; 2; 7; 6; 8];;
(+) 1 3;; (* makes + act like a function *)

(* process list from right to left *)
(* fold_right -> (x1 $ (x2 $ (x3 $ acc))) *)
let rec fold_right f l acc =
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc);;

fold_left min max_int [3; 2; 7; 6; 8];;
fold_right min [3; 2; 7; 6; 8] max_int;;
