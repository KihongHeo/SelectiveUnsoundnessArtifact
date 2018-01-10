open AUGER_Monad
open DFMapAVL
open DFSetAVL
open DNat
open Datatypes
open InterNode
open IntraCfg
open IntraNode
open Syn
open TStr

type __ = Obj.t

module Pid = String_as_OT

module PidMap = FMapAVL'.Make(Pid)

module PidSet = FSetAVL'.Make(Pid)

module NodeMap = FMapAVL'.Make(InterNode)

module NodeSet = FSetAVL'.Make(InterNode)

type t = { cfgs : IntraCfg.t PidMap.t; succ : NodeSet.t NodeMap.t;
           pred : NodeSet.t NodeMap.t; nodes : NodeSet.t }

(** val t_rect :
    (IntraCfg.t PidMap.t -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t ->
    NodeSet.t -> 'a1) -> t -> 'a1 **)

let t_rect f t0 =
  let { cfgs = x; succ = x0; pred = x1; nodes = x2 } = t0 in f x x0 x1 x2

(** val t_rec :
    (IntraCfg.t PidMap.t -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t ->
    NodeSet.t -> 'a1) -> t -> 'a1 **)

let t_rec f t0 =
  let { cfgs = x; succ = x0; pred = x1; nodes = x2 } = t0 in f x x0 x1 x2

(** val cfgs : t -> IntraCfg.t PidMap.t **)

let cfgs x = x.cfgs

(** val succ : t -> NodeSet.t NodeMap.t **)

let succ x = x.succ

(** val pred : t -> NodeSet.t NodeMap.t **)

let pred x = x.pred

(** val nodes : t -> NodeSet.t **)

let nodes x = x.nodes

(** val get_cmd : t -> InterNode.t -> cmd option **)

let get_cmd g node =
  let pid = get_pid node in
  let cfg_node = get_node node in
  bind (Obj.magic coq_Maybe) (PidMap.find pid (Obj.magic g.cfgs)) (fun cfg ->
    get_cmd cfg cfg_node)

(** val has_cmd : t -> InterNode.t -> bool **)

let has_cmd g n =
  match get_cmd g n with
  | Some c -> true
  | None -> false

(** val get_args : t -> pid_t -> vid_t list option **)

let get_args g pid =
  bind (Obj.magic coq_Maybe) (PidMap.find pid (Obj.magic g.cfgs)) (fun cfg ->
    Some cfg.args)

(** val is_undef : pid_t -> t -> bool **)

let is_undef pid g =
  negb (PidMap.mem pid g.cfgs)

(** val is_call_node : t -> InterNode.t -> bool **)

let is_call_node g node =
  let pid = get_pid node in
  (match PidMap.find pid g.cfgs with
   | Some cfg -> is_call_node cfg (get_node node)
   | None -> false)

(** val is_return_node : t -> InterNode.t -> bool **)

let is_return_node g node =
  let pid = get_pid node in
  (match PidMap.find pid g.cfgs with
   | Some cfg -> is_return_node cfg (get_node node)
   | None -> false)

(** val is_unreachable_return : t -> InterNode.t -> bool **)

let is_unreachable_return g n =
  (&&) (is_return_node g n)
    (match NodeMap.find n g.pred with
     | Some preds ->
       if Nat.eq_dec (NodeSet.cardinal preds) 0 then true else false
     | None -> true)

(** val pidsof : t -> pid_t list **)

let pidsof g =
  PidMap.fold (fun pid x x0 -> pid :: x0) g.cfgs []

(** val returnof : t -> InterNode.t -> InterNode.t option **)

let returnof g node =
  let f = get_pid node in
  (match PidMap.find f g.cfgs with
   | Some cfg ->
     (match returnof cfg (get_node node) with
      | Some return_node -> Some (f, return_node)
      | None -> None)
   | None -> None)

(** val callof : t -> InterNode.t -> InterNode.t option **)

let callof g node =
  let f = get_pid node in
  (match PidMap.find f g.cfgs with
   | Some cfg ->
     (match callof cfg (get_node node) with
      | Some call_node -> Some (f, call_node)
      | None -> None)
   | None -> None)

(** val callnodesof : t -> NodeSet.t **)

let callnodesof g =
  NodeSet.filter (is_call_node g) g.nodes

(** val is_f : pid_t -> pid_t -> bool **)

let is_f f g =
  if Pid.eq_dec f g then true else false

(** val is_node_of_f : pid_t -> InterNode.t -> bool **)

let is_node_of_f f n =
  is_f f (get_pid n)

(** val is_same_node : InterNode.t -> InterNode.t -> bool **)

let is_same_node n1 n2 =
  if InterNode.eq_dec n1 n2 then true else false

(** val nodeset_remove : (InterNode.t -> bool) -> NodeSet.t -> NodeSet.t **)

let nodeset_remove cond s =
  NodeSet.filter (fun n -> negb (cond n)) s

(** val nodemap_remove :
    (InterNode.t -> bool) -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t **)

let nodemap_remove cond s =
  let remove1 = fun k v acc ->
    if cond k then acc else NodeMap.add k (nodeset_remove cond v) acc
  in
  NodeMap.fold remove1 s NodeMap.empty

(** val remove_function : pid_t -> t -> t **)

let remove_function f g =
  { cfgs = (PidMap.remove f g.cfgs); succ =
    (nodemap_remove (is_node_of_f f) g.succ); pred =
    (nodemap_remove (is_node_of_f f) g.pred); nodes =
    (nodeset_remove (is_node_of_f f) g.nodes) }

(** val cfgs_remove_node :
    InterNode.t -> IntraCfg.t PidMap.t -> IntraCfg.t PidMap.t **)

let cfgs_remove_node node cfgs0 =
  let pid = get_pid node in
  (match PidMap.find pid cfgs0 with
   | Some cfg -> PidMap.add pid (remove_node (get_node node) cfg) cfgs0
   | None -> cfgs0)

(** val remove_node : InterNode.t -> t -> t **)

let remove_node node g =
  { cfgs = (cfgs_remove_node node g.cfgs); succ =
    (nodemap_remove (is_same_node node) g.succ); pred =
    (nodemap_remove (is_same_node node) g.pred); nodes =
    (NodeSet.remove node g.nodes) }

