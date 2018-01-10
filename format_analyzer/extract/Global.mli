open InterCfg
open InterNode
open IntraNode
open Syn
open VocabA

module Callgraph : 
 sig 
  type t = { node_calls : PidSet.t NodeMap.t; calls : PidSet.t PidMap.t;
             trans_calls : PidSet.t PidMap.t }
  
  val t_rect :
    (PidSet.t NodeMap.t -> PidSet.t PidMap.t -> PidSet.t PidMap.t -> 'a1) ->
    t -> 'a1
  
  val t_rec :
    (PidSet.t NodeMap.t -> PidSet.t PidMap.t -> PidSet.t PidMap.t -> 'a1) ->
    t -> 'a1
  
  val node_calls : t -> PidSet.t NodeMap.t
  
  val calls : t -> PidSet.t PidMap.t
  
  val trans_calls : t -> PidSet.t PidMap.t
  
  val empty : t
  
  val is_rec : pid_t -> t -> bool
  
  val node_calls_remove_function :
    pid_t -> PidSet.t NodeMap.t -> PidSet.t NodeMap.t
  
  val calls_remove_function : pid_t -> PidSet.t PidMap.t -> PidSet.t PidMap.t
  
  val remove_function : pid_t -> t -> t
  
  val remove_node : InterNode.t -> t -> t
 end

module type DUMP = 
 sig 
  type t 
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val remove_function : pid_t -> t -> t
  
  val remove_node : InterNode.t -> t -> t
 end

module Global : 
 functor (Dump:DUMP) ->
 sig 
  type t = { icfg : InterCfg.t; callgraph : Callgraph.t; dump : Dump.t }
  
  val t_rect : (InterCfg.t -> Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1
  
  val t_rec : (InterCfg.t -> Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1
  
  val icfg : t -> InterCfg.t
  
  val callgraph : t -> Callgraph.t
  
  val dump : t -> Dump.t
  
  val is_undef : pid_t -> t -> bool
  
  val is_call_node : t -> InterNode.t -> bool
  
  val is_exit_node : InterNode.t -> bool
  
  val is_rec : pid_t -> t -> bool
  
  val get_callees : t -> InterNode.t -> PidSet.t
  
  val remove_function : pid_t -> t -> t
  
  val remove_node : InterNode.t -> t -> t
 end

