(** Def-use graph *)
module type S = 
sig
  module Pre : Pre.S
  type t
  type vertex = BasicDom.Node.t
  type loc
  val make              : ?skip_nodes : BasicDom.Node.t BatSet.t -> Global.t * Pre.t * loc BatSet.t -> t 
  val nodesof           : t -> vertex BatSet.t
  val succ              : vertex -> t -> vertex list
  val pred              : vertex -> t -> vertex list
  val nb_vertex         : t -> int
  val add_edge          : vertex -> vertex -> t -> t
  val remove_node       : vertex -> t -> t
  val get_abslocs       : vertex -> vertex -> t -> loc BatSet.t
  val mem_duset         : loc -> loc BatSet.t -> bool
  val add_absloc        : vertex -> loc -> vertex -> t -> t
  val add_abslocs       : vertex -> loc BatSet.t -> vertex -> t -> t
  val fold_node         : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges        : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a 
  val iter_edges        : (vertex -> vertex -> unit) -> t -> unit
  val sizeof            : t -> int
  val to_dot            : t -> string
  val to_json           : t -> Yojson.Safe.json
  val to_json_intra     : t -> Pre.t -> Yojson.Safe.json
  val to_json_inter     : t -> Pre.t -> Yojson.Safe.json
end

module Make (Pre: Pre.S) : S with type loc = Pre.Access.Loc.t and type Pre.t = Pre.t
