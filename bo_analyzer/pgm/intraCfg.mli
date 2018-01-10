(** Intra-procedural CFG *)
open Cil

module Node : sig
  type t 
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val fromCilStmt : Cil.stmt -> t
  val make : unit -> t
  val getid : t -> int
  val to_string : t -> string

  val entry : t
  val exit : t
end

module Cmd : sig
  type t = 
  | Cinstr of instr list 
  | Cif of exp * block * block * location 
  | CLoop of location
  (* final graph has the following cmds only *)
  | Cset of lval * exp * location
  | Cexternal of lval * location 
  | Calloc of lval * exp * static * location
  | Csalloc of lval * string * location
  | Cfalloc of lval * fundec * location
  | Cassume of exp * location
  | Cseq of t list 
  | Ccall of lval option * exp * exp list * location 
  | Creturn of exp option * location
  | Casm of attributes * string list * 
            (string option * string * lval) list *
            (string option * string * exp) list *
            string list * location
  | Cskip
  and static = bool

  val fromCilStmt : Cil.stmtkind -> t
  val to_string : t -> string
end

type t
and dom_fronts = (Node.t, Node.t BatSet.t) BatMap.t
and dom_tree
and node = Node.t
and cmd = Cmd.t

val generate_global_proc : Cil.global list -> Cil.fundec -> t 
val fromFunDec : Cil.fundec -> Cil.location -> t
val compute_scc : t -> t
val compute_dom : t -> t

val get_pid : t -> string
val find_cmd : node -> t ->  cmd
val add_cmd : node -> cmd -> t -> t
val nodesof : t -> node list
val is_entry : node -> bool
val is_exit : node -> bool
val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val returnof : node -> t -> node 
val is_inside_loop : node -> t -> bool
val callof : node -> t -> node
val get_formals : t -> string list
val unreachable_node : t -> node BatSet.t
val remove_node : node -> t -> t
val print_dot : out_channel -> t -> unit
val to_json : t -> Yojson.Safe.json
val entryof : t -> Node.t
val exitof : t -> Node.t
val pred : node -> t -> node list
val succ : node -> t -> node list
val add_new_node : node -> cmd -> node -> t -> t
val add_node_with_cmd : node -> cmd -> t -> t
val add_edge : node -> node -> t -> t
val get_dom_fronts : t ->  dom_fronts
val get_dom_tree : t ->  dom_tree
val children_of_dom_tree : node -> dom_tree -> node BatSet.t
val parent_of_dom_tree : node -> dom_tree -> node option
val opt_basic_block : t -> t
