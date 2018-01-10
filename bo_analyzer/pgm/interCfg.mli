(** Inter-procedural CFG *)

module Proc : AbsDom.HASHABLE_SET with type t = string

module Node : sig
  include AbsDom.HASHABLE_SET
  val get_pid   : t -> Proc.t
  val get_cfgnode : t -> IntraCfg.Node.t
  val make      : Proc.t -> IntraCfg.Node.t -> t 
end

type pid = Proc.t
and node = Node.t
type t
val init : Cil.file -> t
val add_call_edge : Node.t -> Proc.t -> t -> t
val get_callees : Node.t -> t -> Proc.t BatSet.t
val is_undef : pid -> t -> bool
val pidsof : t -> pid list
val remove_function : pid -> t -> t
val remove_node : node -> t -> t
val to_json : t -> Yojson.Safe.json
val argsof : t -> pid -> string list
val cmdof : t -> Node.t -> IntraCfg.cmd
val nodesof : t -> Node.t list
val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val global_proc : Proc.t
val unreachable_node : t -> node BatSet.t

val callof : node -> t -> node
val fold_cfgs : (Proc.t -> IntraCfg.t -> 'a -> 'a) -> t -> 'a -> 'a
val callnodesof : t -> node list
val returnof : node -> t -> node 
val entryof : t -> pid -> node
val exitof : t -> pid -> node
val start_node : node
val cfgof : t -> pid -> IntraCfg.t 
val nodes_of_pid : t -> pid -> Node.t list
