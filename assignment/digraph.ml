module VertexMap = Map.Make(String)

type t = ((string * int) list) VertexMap.t
type edge = string * string * int

exception Invalid of string

(** [empty] is the empty digraph. *)
let empty = VertexMap.empty

(** [validate_edge (v1, v2, ed_len)] checks whether an edge is valid.
  * It raises [Invalid] if either vertex is empty, if the vertices
  * are the same, or if the edge length is not positive.
  *)
let validate_edge ((v1, v2, ed_len) : edge) : unit =
  if v1 = "" then
    raise (Invalid "source vertex cannot be empty")
  else if v2 = "" then
    raise (Invalid "destination vertex cannot be empty")
  else if v1 = v2 then
    raise (Invalid "source and destination must be distinct")
  else if ed_len <= 0 then
    raise (Invalid "edge length must be positive")
  else
    ()

(** [add_edge (v1, v2, ed_len) graph] returns a new graph with the given
  * directed edge added.
  * It raises [Invalid] if the edge is invalid or if an edge with the
  * same source and destination already exists.
  *)
let add_edge ((v1, v2, ed_len) : edge) (graph : t) : t =
  let _ = validate_edge (v1, v2, ed_len) in
  let existing_edges =
    match VertexMap.find_opt v1 graph with
    | Some edges -> edges
    | None -> []
  in
  if List.exists (fun (dest, _) -> dest = v2) existing_edges then
    raise (Invalid "duplicate edge")
  else
    VertexMap.add v1 ((v2, ed_len) :: existing_edges) graph

(** [of_edges ls] builds a digraph from the list of edges [ls].
  * It raises [Invalid] if any edge in [ls] is invalid or duplicated.
  *)
let of_edges (ls : edge list) : t =
  List.fold_left (fun acc edge -> add_edge edge acc) empty ls

(** [edges graph] returns a sorted list of all distinct edges in [graph].
  * The edges are sorted first by source vertex, then by destination vertex.
  *)
let edges (graph : t) : edge list =
  let compare_edges (v1, v2, _) (va, vb, _) =
    let cmp1 = String.compare v1 va in
    if cmp1 <> 0 then
      cmp1
    else
      String.compare v2 vb
  in
  let all_edges =
    VertexMap.fold
      (fun src neighbors acc ->
        List.fold_left
          (fun inner_acc (dest, len) -> (src, dest, len) :: inner_acc)
          acc
          neighbors)
      graph
      []
  in
  List.sort compare_edges all_edges

(** [vertices graph] returns a sorted list of all distinct vertices
  * in [graph].
  * This includes vertices that appear only as destinations.
  *)
let vertices (graph : t) : string list =
  let vertex_list =
    VertexMap.fold
      (fun src neighbors acc ->
        let acc_with_src = src :: acc in
        List.fold_left
          (fun inner_acc (dest, _) -> dest :: inner_acc)
          acc_with_src
          neighbors)
      graph
      []
  in
  List.sort_uniq String.compare vertex_list

(** [neighbors vtx graph] returns the outgoing neighbors of [vtx]
  * as a list of [(destination, length)] pairs.
  * If [vtx] has no outgoing edges, it returns the empty list.
  *)
let neighbors vtx (graph : t) : (string * int) list =
  match VertexMap.find_opt vtx graph with
  | Some verts -> verts
  | None -> []
