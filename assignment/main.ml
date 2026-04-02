open Digraph

(** [read_file src] returns all lines from the file named [src]
  * in the same order they appear in the file.
  *)
let read_file src =
  let ic = open_in src in
  let rec read_file_line acc =
    try
      let line = input_line ic in
      read_file_line (line :: acc)
    with
    | End_of_file ->
        close_in ic;
        List.rev acc
  in
  read_file_line []

(** [parse_line line] tries to parse [line] as an edge.
  * It uses only the first 3 words and ignores any extra words.
  * It returns [None] if [line] does not contain 2 vertices followed
  * by an integer edge length.
  *)
let parse_line line =
  let parts = String.split_on_char ' ' line in
  let filtered_parts = List.filter (fun x -> x <> "") parts in
  match filtered_parts with
  | src :: dst :: len_str :: _ ->
      (
        try
          let len = int_of_string len_str in
          Some (src, dst, len)
        with
        | Failure _ -> None
      )
  | _ -> None

(** [read_data src] reads graph data from the file named [src]
  * and returns the resulting digraph.
  * Lines that do not match the expected format are skipped.
  * It may raise [Invalid] if a parsed edge is invalid or duplicated.
  *)
let read_data src =
  let lines = read_file src in
  let edge_list =
    List.fold_right
      (fun line acc ->
        match parse_line line with
        | Some edge -> edge :: acc
        | None -> acc)
      lines
      []
  in
  Digraph.of_edges edge_list

(** [update_distance vertex new_dist dist_list] returns a new distance
  * list where [vertex] has distance [new_dist].
  * If [vertex] is not already present, it is added.
  *)
let rec update_distance vertex new_dist dist_list =
  match dist_list with
  | [] -> [(vertex, new_dist)]
  | (v, d) :: rest ->
      if v = vertex then
        (v, new_dist) :: rest
      else
        (v, d) :: update_distance vertex new_dist rest

(** [find_min_vertex dist_list unvisited] returns [Some (v, d)] where [v]
  * is the unvisited vertex with the smallest distance in [dist_list].
  * It returns [None] if no unvisited vertex remains.
  *)
let find_min_vertex dist_list unvisited =
  List.fold_left
    (fun acc (v, d) ->
      if not (List.mem v unvisited) then
        acc
      else
        match acc with
        | None -> Some (v, d)
        | Some (_, d_min) ->
            if d < d_min then
              Some (v, d)
            else
              acc)
    None
    dist_list

(** [update_predecessor vertex pred pred_list] returns a new predecessor
  * list where [vertex] has predecessor [pred].
  * If [vertex] is not already present, it is added.
  *)
let rec update_predecessor vertex pred pred_list =
  match pred_list with
  | [] -> [(vertex, Some pred)]
  | (v, p) :: rest ->
      if v = vertex then
        (v, Some pred) :: rest
      else
        (v, p) :: update_predecessor vertex pred rest

(** [construct_path prev_list v acc] builds the path ending at [v]
  * by following predecessors in [prev_list].
  * The list [acc] stores the path built so far.
  *)
let rec construct_path prev_list v acc =
  match List.assoc v prev_list with
  | None -> acc
  | Some u -> construct_path prev_list u (u :: acc)

(** [shortest_path src dst graph] returns a pair containing the total
  * length of a shortest path from [src] to [dst] in [graph], and the
  * path itself as a list of vertices from [src] to [dst].
  * It raises [Failure] if no path exists or if either vertex is missing.
  *)
let shortest_path src dst graph =
  let verts = Digraph.vertices graph in
  if not (List.mem src verts) || not (List.mem dst verts) then
    failwith "No path found"
  else
    let dist_list =
      List.map (fun v -> (v, if v = src then 0 else max_int)) verts
    in
    let prev_list =
      List.map (fun v -> (v, None)) verts
    in
    let rec dijkstra_helper dist_list prev_list unvisited =
      match find_min_vertex dist_list unvisited with
      | None -> failwith "No path found"
      | Some (u, d_u) ->
          if d_u = max_int then
            failwith "No path found"
          else if u = dst then
            let path = construct_path prev_list u [u] in
            (d_u, path)
          else
            let neighbors_list = Digraph.neighbors u graph in
            let (dist_list', prev_list') =
              List.fold_left
                (fun (d_acc, p_acc) (v, weight) ->
                  let d_v = List.assoc v d_acc in
                  let alt = d_u + weight in
                  if alt < d_v then
                    let d_acc' = update_distance v alt d_acc in
                    let p_acc' = update_predecessor v u p_acc in
                    (d_acc', p_acc')
                  else
                    (d_acc, p_acc))
                (dist_list, prev_list)
                neighbors_list
            in
            let unvisited' = List.filter (fun x -> x <> u) unvisited in
            dijkstra_helper dist_list' prev_list' unvisited'
    in
    dijkstra_helper dist_list prev_list verts
