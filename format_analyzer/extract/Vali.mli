open GenFunc
open InterCfg
open InterNode
open IntraCfg
open IntraNode
open Syn
open TStr
open UserInput

module Vali : 
 sig 
  val get_access : pid_t -> Input.access_map -> Input.Acc.t
  
  val get_def_access : pid_t -> Input.access_map -> Input.PowLoc.t
  
  val get_use_access : pid_t -> Input.access_map -> Input.PowLoc.t
  
  val get_all_access : pid_t -> Input.access_map -> Input.PowLoc.t
  
  val mem_restrict : Input.PowLoc.t -> Input.Mem.t -> Input.Mem.t
  
  val mem_removes : Input.PowLoc.t -> Input.Mem.t -> Input.Mem.t
  
  val icfg : Input.G.t -> InterCfg.t
  
  val dump : Input.G.t -> Input.Dump.t
  
  val valid_cmd :
    Input.G.t -> Input.PowLoc.t -> Input.access_map -> Input.Table.t ->
    Input.Table.t -> pid_t -> t -> cmd -> bool
  
  val valid_cmds :
    Input.G.t -> Input.PowLoc.t -> Input.access_map -> Input.Table.t ->
    Input.Table.t -> pid_t -> IntraCfg.t -> bool
  
  val valid_intra_edge :
    Input.Table.t -> Input.Table.t -> pid_t -> IntraCfg.t -> t -> t -> bool
  
  val valid_intra_edges :
    Input.Table.t -> Input.Table.t -> pid_t -> IntraCfg.t -> bool
  
  val valid_cfg :
    Input.G.t -> Input.PowLoc.t -> Input.access_map -> Input.Table.t ->
    Input.Table.t -> pid_t -> IntraCfg.t -> bool
  
  val valid_cfgs :
    Input.G.t -> Input.PowLoc.t -> Input.access_map -> Input.Table.t ->
    Input.Table.t -> bool
  
  val valid_inter_edge :
    Input.G.t -> Input.access_map -> Input.Table.t -> Input.Table.t ->
    InterNode.t -> InterNode.t -> bool
  
  val valid_inter_edges :
    Input.G.t -> Input.access_map -> Input.Table.t -> Input.Table.t -> bool
  
  val valid :
    Input.G.t -> Input.PowLoc.t -> Input.access_map -> Input.Table.t ->
    Input.Table.t -> bool
  
  val check_query_access :
    pid_t -> Input.Mem.t -> Input.query -> Input.status list
    Input.coq_AccPair
  
  val check_query_only :
    pid_t -> Input.Mem.t -> Input.query -> Input.status list
  
  val check_queries :
    pid_t -> Input.Mem.t -> (Input.query * DPos.DPos.t) ->
    (Input.query * DPos.DPos.t) * Input.status list
  
  val query_flatten' :
    Input.query -> DPos.DPos.t -> Input.status list ->
    ((Input.query * DPos.DPos.t) * Input.status) list
  
  val query_flatten :
    ((Input.query * DPos.DPos.t) * Input.status list) list ->
    ((Input.query * DPos.DPos.t) * Input.status) list
  
  val collect_alarm_result_node :
    Input.Table.t -> string_t -> t' -> cmd ->
    ((Input.query * DPos.DPos.t) * Input.status) list ->
    ((Input.query * DPos.DPos.t) * Input.status) list
  
  val collect_alarm_result_intra :
    Input.Table.t -> string_t -> IntraCfg.t ->
    ((Input.query * DPos.DPos.t) * Input.status) list ->
    ((Input.query * DPos.DPos.t) * Input.status) list
  
  val collect_alarm_result :
    Input.G.t -> Input.Table.t ->
    ((Input.query * DPos.DPos.t) * Input.status) list
 end

