type t                              (* the abstract digraph type *)
type edge = string * string * int   (* type of an edge *)

(** raised by [add_edge] and [of_edges] when adding an edge to a digraph;
    this could happen when the edge has non-positive length, or the 2 vertices
    in the edge are not distinct, or the edge to add is a "duplicate" in the
    sense that there is already an edge in the graph with the same source
    and destination vertices, and which can be of the same length or of
    different lengths.
*)
exception Invalid of string   (* the string argument specifies the reason *)

(** [empty] is the empty digraph *)
val empty : t

(** [add_edge edge graph] adds [edge] to [graph].
    Raises: [Inv_edge] if [edge] is invalid; raises [Inv_graph] if [edge] is
            a duplicate with a different length *)
val add_edge : edge -> t -> t

(** [of_edges edges] is the digraph formed from the list [edges].
    May raise [Inv_edge] or [Inv_graph] (see above) *)
val of_edges : edge list -> t

(** [edges graph] is the sorted list of all distinct edges in [graph] *)
val edges : t -> edge list

(** [vertices graph] is the list of all distinct vertices (in alphabetical
    order) of [graph] *)
val vertices : t -> string list

(** [neighbors vertex graph] is the list of neighbors of [vertex] in
    [graph]; each neighbor is a pair of the form ([vertex2], [length]) which
    indicates there is an edge from [vertex] to [vetex2] of length [length]) *)
val neighbors : string -> t -> (string * int) list
