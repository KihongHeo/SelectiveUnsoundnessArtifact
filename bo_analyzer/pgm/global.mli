(** Global information *)
open BasicDom

type t = {
  file : Cil.file;
  icfg : InterCfg.t;
  callgraph : CallGraph.t;
  dump : Dump.t;
  mem : ItvDom.Mem.t;
}

val init : Cil.file -> t

val update_icfg : InterCfg.t -> t -> t
val update_callgraph : CallGraph.t -> t -> t
val update_mem : ItvDom.Mem.t -> t -> t

val is_rec : InterCfg.pid -> t -> bool
val is_undef : InterCfg.pid -> t -> bool

val get_icfg : t -> InterCfg.t
val get_callgraph : t -> CallGraph.t
val get_callees : InterCfg.node -> t -> InterCfg.pid list

val remove_function : InterCfg.Proc.t -> t -> t
val remove_node : InterCfg.node -> t -> t
val remove_unreachable_functions : t -> t 
