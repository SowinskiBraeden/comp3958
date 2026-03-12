(* monad, mx means monad x *)
let (>>=) mx f =
  match mx with
  | None -> None
  | Some x -> f x

let return x = Some x

let (/) a b =
  if b = 0 then None
  else Some (a / b)

let square x = x * x
let square' mx = mx >>= fun x -> return (square x)

let (<$>) f mx =
  match mx with
  | None -> None
  | Some x -> Some (f x)

(* functor *)
let lift f mx = f <$> mx

let (<*>) mf mx =
  match mf with
  | None -> None
  | Some f -> f <$> mx

let add a b = a + b;;

add <$> 4 / 2 <*> Some 1

let lift' f mx my = f <$> mx <*> my

let ( + ) = lift' Stdlib.( + );;
let ( - ) = lift' Stdlib.( - );;
let ( * ) = lift' Stdlib.( * );;

Some 1 + (4 / 2) = Some 3;;
