(* Some, None - Are called data/value constructors *)
Some 1;; (* - : int option = Some 1 *)

None;; (* - : 'a option = None *)

(* OPTIONS are called type constructors, it takes a type *)
(* 'a is a type variable, where 'a is some type such as int *)

type 'a option = None | Some of 'a;;

(*
  You can define any type you want, this type constructor starts
  with a lower case letter "direction" and the value constructor starts
  with uppercase "North", "East", ... 

  type constructors are not functions
*)
type direction = North | East | South | West;;

(* There are RESULT types *)
Ok 1;;
Error "hell";; (* albert's example is hell not my example *)

(*
  A result is a two type variable, this is the syntax for
  something that takes to type variables
*)
type ('a, 'b) result = Ok of 'a | Error of 'b;;

(1, 2);; (* type is - : int * int = (1, 2) *)
(* (int, int) : (type * type) *)

(* we define an expresion that has a type of Int, and can be of type Add, Sub or Mul *)
type expr = 
    Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr;;

(* We then define an eval function, that takes in an eval,
   and recursively evaluates e1, e2 till they are just of
   type Int to then evaluate the addition, subtraction, etc. *)
let rec eval = function
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2;;

let e = Mul(Add (Int 1, Int 2), Int 7);;
eval e;;

(* lets apply the type variables and constructors to create a card type *)
type suit = Club | Diamond | Heart | Spade;;
type rank = Num of int | Jack | Queen | King | Ace;;
type card = rank * suit;;

let compare_rank r1 r2 = 
  match r1, r2 with
  | Num x, Num y -> Int.compare x y
  | Num _, _ -> -1
  | _, Num _ -> 1
  | _, _ -> Stdlib.compare r1 r2;;

let compare_suit s1 s2 = Stdlib.compare s1 s2;;

let compare_card (r1, s1) (r2, s2) =
  let c = compare_rank r1 r2 in
  if c = 0 then compare_suit s1 s2
  else c;;

let string_of_suit = function
  | Club -> "clubs"
  | Diamond -> "diamonds"
  | Heart -> "hearts"
  | Spade -> "spades";;

let string_of_rank = function
  | Num n -> string_of_int n
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | Ace -> "Ace"

let string_of_card (r, s) =
  string_of_rank r ^ " of " ^ string_of_suit s;;

compare_card (King, Diamond) (Queen, Heart);;
string_of_card (Queen, Heart);;

let all_suits = [Club; Diamond; Heart; Spade];;
let all_ranks =
  (List.init 9 (fun x -> x + 2) |> List.map (fun x -> Num x)) @ 
  [Jack; Queen; King; Ace];;

let all_cards =
  List.fold_right (fun rank acc -> (
    List.fold_right (fun suit acc -> (rank, suit) :: acc) all_suits []
  ) @ acc) all_ranks [];;

type 'a linked_list = Nil | Cons of 'a * 'a linked_list;;

let rec map_linked f = function
  | Nil -> Nil
  | Cons (x, l) -> Cons (f x, map_linked f l);;

Cons (1, Cons (2, Cons (3, Nil))) |> map_linked (fun x -> x * x);;

