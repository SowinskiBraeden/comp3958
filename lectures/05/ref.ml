(* we can impliment our own ref type *)
type 'a ref = { mutable contents: 'a };;

(* reference and dereference implimentation *)
let ref x = { contents = x };;
let (!) r = r.contents;;

(* assign to a mutable field *)
let (:=) r x = r.contents <- x;;

let incr r = r.contents <- r.contents + 1;;
let decr r = r.contents <- r.contents - 1;;
