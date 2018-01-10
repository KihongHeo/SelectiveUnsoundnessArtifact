open InterCfg
open InterNode
open IntraNode
open Syn
open VocabA

module Callgraph = 
 struct 
  type t = { node_calls : PidSet.t NodeMap.t; calls : PidSet.t PidMap.t;
             trans_calls : PidSet.t PidMap.t }
  
  (** val t_rect :
      (PidSet.t NodeMap.t -> PidSet.t PidMap.t -> PidSet.t PidMap.t -> 'a1)
      -> t -> 'a1 **)
  
  let t_rect f t0 =
    let { node_calls = x; calls = x0; trans_calls = x1 } = t0 in f x x0 x1
  
  (** val t_rec :
      (PidSet.t NodeMap.t -> PidSet.t PidMap.t -> PidSet.t PidMap.t -> 'a1)
      -> t -> 'a1 **)
  
  let t_rec f t0 =
    let { node_calls = x; calls = x0; trans_calls = x1 } = t0 in f x x0 x1
  
  (** val node_calls : t -> PidSet.t NodeMap.t **)
  
  let node_calls x = x.node_calls
  
  (** val calls : t -> PidSet.t PidMap.t **)
  
  let calls x = x.calls
  
  (** val trans_calls : t -> PidSet.t PidMap.t **)
  
  let trans_calls x = x.trans_calls
  
  (** val empty : t **)
  
  let empty =
    { node_calls = NodeMap.empty; calls = PidMap.empty; trans_calls =
      PidMap.empty }
  
  (** val is_rec : pid_t -> t -> bool **)
  
  let is_rec p cg =
    match PidMap.find p cg.trans_calls with
    | Some fs -> PidSet.mem p fs
    | None -> false
  
  (** val node_calls_remove_function :
      pid_t -> PidSet.t NodeMap.t -> PidSet.t NodeMap.t **)
  
  let node_calls_remove_function f node_calls0 =
    let is_not_f = fun node callees ->
      if InterCfg.Pid.eq_dec f (get_pid node) then false else true
    in
    NodeMap.filter is_not_f node_calls0
  
  (** val calls_remove_function :
      pid_t -> PidSet.t PidMap.t -> PidSet.t PidMap.t **)
  
  let calls_remove_function f calls0 =
    let is_not_f = fun g callees ->
      if InterCfg.Pid.eq_dec f g then false else true
    in
    PidMap.filter is_not_f calls0
  
  (** val remove_function : pid_t -> t -> t **)
  
  let remove_function f cg =
    { node_calls = (node_calls_remove_function f cg.node_calls); calls =
      (calls_remove_function f cg.calls); trans_calls =
      (calls_remove_function f cg.trans_calls) }
  
  (** val remove_node : InterNode.t -> t -> t **)
  
  let remove_node node cg =
    { node_calls = (NodeMap.remove node cg.node_calls); calls = cg.calls;
      trans_calls = cg.trans_calls }
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

module Global = 
 functor (Dump:DUMP) ->
 struct 
  type t = { icfg : InterCfg.t; callgraph : Callgraph.t; dump : Dump.t }
  
  (** val t_rect :
      (InterCfg.t -> Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1 **)
  
  let t_rect f t0 =
    let { icfg = x; callgraph = x0; dump = x1 } = t0 in f x x0 x1
  
  (** val t_rec :
      (InterCfg.t -> Callgraph.t -> Dump.t -> 'a1) -> t -> 'a1 **)
  
  let t_rec f t0 =
    let { icfg = x; callgraph = x0; dump = x1 } = t0 in f x x0 x1
  
  (** val icfg : t -> InterCfg.t **)
  
  let icfg t0 =
    t0.icfg
  
  (** val callgraph : t -> Callgraph.t **)
  
  let callgraph t0 =
    t0.callgraph
  
  (** val dump : t -> Dump.t **)
  
  let dump t0 =
    t0.dump
  
  (** val is_undef : pid_t -> t -> bool **)
  
  let is_undef f g =
    is_undef f (icfg g)
  
  (** val is_call_node : t -> InterNode.t -> bool **)
  
  let is_call_node p node =
    is_call_node (icfg p) node
  
  (** val is_exit_node : InterNode.t -> bool **)
  
  let is_exit_node node =
    is_exit_node (get_node node)
  
  (** val is_rec : pid_t -> t -> bool **)
  
  let is_rec f g =
    Callgraph.is_rec f (callgraph g)
  
  (** val get_callees : t -> InterNode.t -> PidSet.t **)
  
  let get_callees g node =
    let opt_callees = NodeMap.find node (callgraph g).Callgraph.node_calls in
    default PidSet.empty opt_callees
  
  (** val remove_function : pid_t -> t -> t **)
  
  let remove_function f g =
    { icfg = (remove_function f (icfg g)); callgraph =
      (Callgraph.remove_function f (callgraph g)); dump =
      (Dump.remove_function f (dump g)) }
  
  (** val remove_node : InterNode.t -> t -> t **)
  
  let remove_node node g =
    { icfg = (remove_node node (icfg g)); callgraph =
      (Callgraph.remove_node node (callgraph g)); dump =
      (Dump.remove_node node (dump g)) }
 end

