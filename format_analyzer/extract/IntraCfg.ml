open DFMapAVL
open DFSetAVL
open Datatypes
open IntraNode
open Syn

type __ = Obj.t

module NodeMap = FMapAVL'.Make(IntraNode)

module NodeSet = FSetAVL'.Make(IntraNode)

type t = { args : vid_t list; cmds : cmd NodeMap.t;
           succ : NodeSet.t NodeMap.t; pred : NodeSet.t NodeMap.t;
           nodes : NodeSet.t }

(** val t_rect :
    (vid_t list -> cmd NodeMap.t -> NodeSet.t NodeMap.t -> NodeSet.t
    NodeMap.t -> NodeSet.t -> 'a1) -> t -> 'a1 **)

let t_rect f t0 =
  let { args = x; cmds = x0; succ = x1; pred = x2; nodes = x3 } = t0 in
  f x x0 x1 x2 x3

(** val t_rec :
    (vid_t list -> cmd NodeMap.t -> NodeSet.t NodeMap.t -> NodeSet.t
    NodeMap.t -> NodeSet.t -> 'a1) -> t -> 'a1 **)

let t_rec f t0 =
  let { args = x; cmds = x0; succ = x1; pred = x2; nodes = x3 } = t0 in
  f x x0 x1 x2 x3

(** val args : t -> vid_t list **)

let args x = x.args

(** val cmds : t -> cmd NodeMap.t **)

let cmds x = x.cmds

(** val succ : t -> NodeSet.t NodeMap.t **)

let succ x = x.succ

(** val pred : t -> NodeSet.t NodeMap.t **)

let pred x = x.pred

(** val nodes : t -> NodeSet.t **)

let nodes x = x.nodes

(** val get_cmd : t -> IntraNode.t -> cmd option **)

let get_cmd cfg node =
  NodeMap.find node cfg.cmds

(** val is_call_node : t -> IntraNode.t -> bool **)

let is_call_node cfg node =
  match get_cmd cfg node with
  | Some c ->
    (match c with
     | Ccall (ret_opt, f, args0, pos) -> true
     | _ -> false)
  | None -> false

(** val returnof : t -> IntraNode.t -> IntraNode.t option **)

let returnof cfg call_node =
  match NodeMap.find call_node cfg.succ with
  | Some succs -> NodeSet.choose succs
  | None -> None

(** val callof : t -> IntraNode.t -> IntraNode.t option **)

let callof cfg ret_node =
  match NodeMap.find ret_node cfg.pred with
  | Some preds ->
    (match NodeSet.elements preds with
     | [] -> None
     | pred_node :: l ->
       (match l with
        | [] -> if is_call_node cfg pred_node then Some pred_node else None
        | e :: l0 -> None))
  | None -> None

(** val is_return_node : t -> IntraNode.t -> bool **)

let is_return_node cfg node =
  match callof cfg node with
  | Some t0 -> true
  | None -> false

(** val remove_node_pred :
    NodeMap.key -> NodeSet.t NodeMap.t -> NodeSet.t NodeMap.t **)

let remove_node_pred node m =
  let m0 = NodeMap.remove node m in NodeMap.map (NodeSet.remove node) m0

(** val remove_node : IntraNode.t -> t -> t **)

let remove_node node g =
  { args = g.args; cmds = (NodeMap.remove node g.cmds); succ =
    (NodeMap.remove node g.succ); pred = (remove_node_pred node g.pred);
    nodes = (NodeSet.remove node g.nodes) }

