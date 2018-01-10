(** Interprocedural CFG. *)

open VocabB
open Cil

type pid = string
module Node = struct
  type t = pid * FIntraNode.t
  let name = "Node"
  let make pid node = (pid,node)
  let get_pid (pid,node) = pid
  let get_cfgnode (pid,node) = node
  let compare = compare
  let hash = Hashtbl.hash
  let equal (p1, n1) (p2, n2) = p1 = p2 && FIntraNode.equal n1 n2
end

type node = Node.t

type t = {
  cfgs : (pid, FIntraCfg.t) BatMap.t;
  globals : Cil.global list
}

let dummy = {
  cfgs = BatMap.empty;
  globals = []
}

let global_proc = "_G_"
let start_node = Node.make global_proc FIntraNode.ENTRY

let gen_cfgs file = 
  BatMap.add global_proc (FIntraCfg.generate_global_proc file.Cil.globals (Cil.emptyFunction global_proc))
    (list_fold (fun g m ->
      match g with
      | Cil.GFun (f,_) ->
        BatMap.add f.svar.vname (FIntraCfg.fromFunDec f f.svar.vname) m
      | _ -> m
    ) file.Cil.globals BatMap.empty)

let compute_dom_and_scc icfg = 
  { icfg with cfgs = 
      BatMap.map (fun cfg ->
        FIntraCfg.compute_scc (FIntraCfg.compute_dom cfg)
      ) icfg.cfgs }

let init : Cil.file -> t
=fun file -> { cfgs = gen_cfgs file; globals = file.Cil.globals }

let remove_function : pid -> t -> t
=fun pid icfg -> 
  { icfg with cfgs = BatMap.remove pid icfg.cfgs }

let cfgof : t -> pid -> FIntraCfg.t 
=fun g pid -> BatMap.find pid g.cfgs

let cmdof : t -> Node.t -> FIntraCfg.cmd
=fun g (pid,node) -> FIntraCfg.find_cmd node (cfgof g pid)

let add_cmd : t -> Node.t -> FIntraCfg.cmd -> t
=fun g (pid,node) cmd -> 
  {g with cfgs = BatMap.add pid (FIntraCfg.add_cmd node cmd (cfgof g pid)) g.cfgs}

let nodes_of_pid : t -> pid -> Node.t list
=fun g pid -> List.map (Node.make pid) (FIntraCfg.nodesof (cfgof g pid))

let fold_cfgs f g a = BatMap.foldi f g.cfgs a

let nodesof : t -> Node.t list
=fun g ->
  BatMap.foldi (fun pid cfg ->
    List.append 
        (List.map (fun n -> Node.make pid n) (FIntraCfg.nodesof cfg))
  ) g.cfgs []

let pidsof : t -> pid list
=fun g ->
  BatMap.foldi (fun pid _ acc -> pid :: acc) g.cfgs []

let is_undef : pid -> t -> bool
=fun pid g ->
  not (BatMap.mem pid g.cfgs)

let is_callnode : node -> t -> bool
=fun (pid,node) g -> FIntraCfg.is_callnode node (cfgof g pid)

let is_returnnode : node -> t -> bool
=fun (pid,node) g -> FIntraCfg.is_returnnode node (cfgof g pid)

let returnof : node -> t -> node 
=fun (pid,node) g -> (pid, FIntraCfg.returnof node (cfgof g pid))

let is_inside_loop : node -> t -> bool
=fun (pid,node) g -> FIntraCfg.is_inside_loop node (cfgof g pid)

let callof : node -> t -> node
=fun (pid,node) g -> (pid, FIntraCfg.callof node (cfgof g pid))

let argsof : t -> pid -> string list
=fun g pid -> FIntraCfg.get_formals (cfgof g pid)

let callnodesof : t -> node list
=fun g -> List.filter (fun node -> is_callnode node g) (nodesof g)

let entryof : t -> pid -> node
=fun g pid -> Node.make pid FIntraNode.ENTRY

let exitof : t -> pid -> node
=fun g pid -> Node.make pid FIntraNode.EXIT

let unreachable_node_pid : pid -> FIntraCfg.t -> node BatSet.t
=fun pid icfg ->
  BatSet.map (fun node -> (pid, node)) (FIntraCfg.unreachable_node icfg)

let unreachable_node : t -> node BatSet.t
=fun g ->
  let add_unreachable_node pid icfg =
    BatSet.union (unreachable_node_pid pid icfg) in
  fold_cfgs add_unreachable_node g BatSet.empty

let remove_node : node -> t -> t
=fun (pid, intra_node) g ->
  let intra_cfg = cfgof g pid in
  let intra_cfg = FIntraCfg.remove_node intra_node intra_cfg in
  { g with cfgs = BatMap.add pid intra_cfg g.cfgs }
