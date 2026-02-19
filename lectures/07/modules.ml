(*
  - a module can contain other modules as well as module type definitions
  - a module type/sig (signature) can not contain modules but can contain other sigs
  - for a .ml/.mli pair,
    * anything not specified in .mli file is hidden
    * everything specified in the .mli file must be defined in the .ml file
*)

module M = struct
  let f x = 2 * x
end;;

module type S = module type of M;;

module M = struct
  let f x = 2 * x;;
  let g x = 3 * x;;
  module N = struct
    type t = int
  end
end;;

(* S signature is the same as M if loaded in utop *)
module type S = module type of M;;

(***** EXTENDED MODULES *****)

(* lets extend List module *)
module ListExt = struct
  include List;; (* include module to extend *)
  let to_array l = Array.of_list l;;
end;;

ListExt.to_array [3;2;7;6;8];;

module F = Map.Make(String);;

(* returns abstract type *)
let m = F.empty |> F.add "hello" 1 |> F.add "hello" 2;;
F.to_list m;;

let incr key m =
  F.update key (function | None -> Some 1 | Some x -> Some (x + 1))
  m;;

let m = incr "hello" m;;
let m = incr "world" m;;
