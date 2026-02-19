(* bstree with records *)
type 'a t = L | N of {v: 'a; l: 'a t; r: 'a t};;
