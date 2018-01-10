open DPow
open Datatypes
open InterCfg
open InterNode
open IntraCfg
open IntraNode
open List0
open Syn
open TStr
open UserInputType
open VocabA

val fail_cmd_mem : pid_t -> t -> bool

val fail_access : pid_t -> t -> bool

val fail_dump : pid_t -> t -> bool

val fail_intra_edge : pid_t -> t -> t -> bool

val fail_inter_edge : InterNode.t -> InterNode.t -> bool

val fail_inter_edge_invalid : InterNode.t -> InterNode.t -> bool

val fail_inter_edge_no_callof : InterNode.t -> InterNode.t -> bool

module Make : 
 functor (I:INPUT) ->
 sig 
  val get_access : pid_t -> I.access_map -> I.Acc.t
  
  val get_def_access : pid_t -> I.access_map -> I.PowLoc.t
  
  val get_use_access : pid_t -> I.access_map -> I.PowLoc.t
  
  val get_all_access : pid_t -> I.access_map -> I.PowLoc.t
  
  val mem_restrict : I.PowLoc.t -> I.Mem.t -> I.Mem.t
  
  val mem_removes : I.PowLoc.t -> I.Mem.t -> I.Mem.t
  
  val icfg : I.G.t -> InterCfg.t
  
  val dump : I.G.t -> I.Dump.t
  
  val valid_cmd :
    I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t ->
    t -> cmd -> bool
  
  val valid_cmds :
    I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t ->
    IntraCfg.t -> bool
  
  val valid_intra_edge :
    I.Table.t -> I.Table.t -> pid_t -> IntraCfg.t -> t -> t -> bool
  
  val valid_intra_edges :
    I.Table.t -> I.Table.t -> pid_t -> IntraCfg.t -> bool
  
  val valid_cfg :
    I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t ->
    IntraCfg.t -> bool
  
  val valid_cfgs :
    I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> bool
  
  val valid_inter_edge :
    I.G.t -> I.access_map -> I.Table.t -> I.Table.t -> InterNode.t ->
    InterNode.t -> bool
  
  val valid_inter_edges :
    I.G.t -> I.access_map -> I.Table.t -> I.Table.t -> bool
  
  val valid :
    I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> bool
  
  val check_query_access :
    pid_t -> I.Mem.t -> I.query -> I.status list I.coq_AccPair
  
  val check_query_only : pid_t -> I.Mem.t -> I.query -> I.status list
  
  val check_queries :
    pid_t -> I.Mem.t -> (I.query * DPos.DPos.t) ->
    (I.query * DPos.DPos.t) * I.status list
  
  val query_flatten' :
    I.query -> DPos.DPos.t -> I.status list ->
    ((I.query * DPos.DPos.t) * I.status) list
  
  val query_flatten :
    ((I.query * DPos.DPos.t) * I.status list) list ->
    ((I.query * DPos.DPos.t) * I.status) list
  
  val collect_alarm_result_node :
    I.Table.t -> string_t -> t' -> cmd ->
    ((I.query * DPos.DPos.t) * I.status) list ->
    ((I.query * DPos.DPos.t) * I.status) list
  
  val collect_alarm_result_intra :
    I.Table.t -> string_t -> IntraCfg.t ->
    ((I.query * DPos.DPos.t) * I.status) list ->
    ((I.query * DPos.DPos.t) * I.status) list
  
  val collect_alarm_result :
    I.G.t -> I.Table.t -> ((I.query * DPos.DPos.t) * I.status) list
 end

