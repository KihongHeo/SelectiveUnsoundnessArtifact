(** Access Pre-Analysis Framework *)
open VocabA
open VocabB
open Syn
open UserInputType
open UserInput.Input

let idMem = Obj.magic coq_IdMem
let mId = Obj.magic coq_MId

module ProcG = Graph.Persistent.Digraph.Concrete (Key2Ordered (Proc))
module SCC = Graph.Components.Make (ProcG)

type t =
  { mem : Mem.t
  ; total_abslocs : PowLoc.t
  ; access : Acc.t InterCfg.NodeMap.t
  ; access_proc : Acc.t InterCfg.PidMap.t
  ; access_reach : Acc.t InterCfg.PidMap.t
  ; access_local_proc : PowLoc.t InterCfg.PidMap.t
  ; access_local_program : PowLoc.t
  ; defs_of : InterCfg.NodeSet.t LocMap.t
  ; uses_of : InterCfg.NodeSet.t LocMap.t
  }

let empty =
  { mem = Mem.bot
  ; total_abslocs = PowLoc.empty
  ; access = InterCfg.NodeMap.empty
  ; access_proc = InterCfg.PidMap.empty
  ; access_reach = InterCfg.PidMap.empty
  ; access_local_proc = InterCfg.PidMap.empty
  ; access_local_program = PowLoc.empty
  ; defs_of = LocMap.empty
  ; uses_of = LocMap.empty
  }

let get_mem i = i.mem
let get_total_abslocs i = i.total_abslocs
let get_access i n =
  default Acc.empty (InterCfg.NodeMap.find n i.access)
let get_access_proc i pid =
  default Acc.empty (InterCfg.PidMap.find pid i.access_proc)

(* Returns abstract locations exclusively accessed in the given function *)
let get_access_local i pid =
  default PowLoc.empty (InterCfg.PidMap.find pid i.access_local_proc)
let get_access_local_program i = i.access_local_program
let get_access_reach i pid =
  default Acc.empty (InterCfg.PidMap.find pid i.access_reach)
let get_defs_of t = t.defs_of
let get_uses_of t = t.uses_of

let restrict abslocs m = InterCfg.NodeMap.map (Acc.restrict abslocs) m
let init_access (m, g) =
  let nodes = InterCfg.nodes (G.icfg g) in
  let add_access node =
    let access = Run.accessof ~mode:Strong PrePhase node (m, g) in
    let qaccess = Run.qaccessof node (m, g) in
    InterCfg.NodeMap.add node (Acc.join access qaccess) in
  let node2access =
    InterCfg.NodeSet.fold add_access nodes InterCfg.NodeMap.empty in
  let union_access _ access acc =
    let acc = PowLoc.union_small_big (Acc.useof access) acc in
    PowLoc.union_small_big (Acc.defof access) acc in
  let abslocs = InterCfg.NodeMap.fold union_access node2access PowLoc.empty in
  (abslocs, restrict abslocs node2access)

let filter_out_access access locs =
  let filter_out1 node access =
    InterCfg.NodeMap.add node (Acc.filter_out locs access) in
  InterCfg.NodeMap.fold filter_out1 access InterCfg.NodeMap.empty
let restrict_access t locs =
  let add_access node access =
    InterCfg.NodeMap.add node (Acc.restrict locs access) in
  { t with
    access =
      InterCfg.NodeMap.fold add_access t.access InterCfg.NodeMap.empty
  }

let init_access_proc access : Acc.t InterCfg.PidMap.t =
  let add_access node new_access proc2access =
    let pid = InterNode.get_pid node in
    let access =
      default Acc.empty (InterCfg.PidMap.find pid proc2access) in
    let access = Acc.union access new_access in
    InterCfg.PidMap.add pid access proc2access in
  InterCfg.NodeMap.fold add_access access InterCfg.PidMap.empty

let make_n2access n2pids access_proc =
  let add_access pid = Acc.union
      (default Acc.empty (InterCfg.PidMap.find pid access_proc)) in
  let access_procs pids = InterCfg.PidSet.fold add_access pids Acc.empty in
  BatMap.map access_procs n2pids

let rec make_n2reach_access n2access n2next_ns cur_n n2reach_access =
  if BatMap.mem cur_n n2reach_access then n2reach_access else
    let next_ns = BatSet.remove cur_n (BatMap.find cur_n n2next_ns) in
    let add_reach_access = make_n2reach_access n2access n2next_ns in
    let n2reach_access = BatSet.fold add_reach_access next_ns n2reach_access in
    let add_access n = Acc.union (BatMap.find n n2reach_access) in
    let trans_access = BatSet.fold add_access next_ns Acc.empty in
    let cur_access = BatMap.find cur_n n2access in
    let reach_access = Acc.union cur_access trans_access in
    BatMap.add cur_n reach_access n2reach_access

(* NOTE: Not_found cases are unreachable functions from _G_.  They
   will be removed later. *)
let make_pid2reach_access n2reach_access pid2n =
  let get_reach_access n =
    try BatMap.find n n2reach_access with Not_found -> Acc.empty in
  InterCfg.PidMap.map get_reach_access pid2n

let init_ocaml_cg calls =
  let add_call caller callee acc = ProcG.add_edge acc caller callee in
  let add_calls caller callees =
    InterCfg.PidSet.fold (add_call caller) callees in
  InterCfg.PidMap.fold add_calls calls ProcG.empty

let make_pid2n pids pid2n_func scc_num =
  let add_pid2n pid (m, n) =
    let (k, n) = try (pid2n_func pid, n) with Not_found -> (n, n + 1) in
    (InterCfg.PidMap.add pid k m, n) in
  list_fold add_pid2n pids (InterCfg.PidMap.empty, scc_num)

let make_n2pids pids group_num pid2n =
  let n2pids_empty =
    let rec n2pids_empty_rec n m =
      if n <= 0 then m else
        let n' = n - 1 in
        n2pids_empty_rec n' (BatMap.add n' InterCfg.PidSet.empty m) in
    n2pids_empty_rec group_num BatMap.empty in
  let add_n2pid pid m =
    let n = get_some (InterCfg.PidMap.find pid pid2n) in
    let pids = InterCfg.PidSet.add pid (BatMap.find n m) in
    BatMap.add n pids m in
  list_fold add_n2pid pids n2pids_empty

let make_n2next_pids calls n2pids =
  let add_pid2next_pids pid acc =
    let callees =
      default InterCfg.PidSet.empty (InterCfg.PidMap.find pid calls) in
    InterCfg.PidSet.union acc callees in
  let pids2next_pids pids =
    InterCfg.PidSet.fold add_pid2next_pids pids InterCfg.PidSet.empty in
  BatMap.map pids2next_pids n2pids

let make_n2next_ns n2next_pids pid2n =
  let add_pid2n pid acc =
    BatSet.add (get_some (InterCfg.PidMap.find pid pid2n)) acc in
  let pids2ns pids = InterCfg.PidSet.fold add_pid2n pids BatSet.empty in
  BatMap.map pids2ns n2next_pids

let init_access_reach pids cg access_proc =
  let calls = Global.Callgraph.calls cg in
  let ocaml_cg = init_ocaml_cg calls in
  let (scc_num, pid2n_func) = SCC.scc ocaml_cg in
  let (pid2n, group_num) = make_pid2n pids pid2n_func scc_num in
  let n2pids = make_n2pids pids group_num pid2n in
  let n2next_pids = make_n2next_pids calls n2pids in
  let n2next_ns = make_n2next_ns n2next_pids pid2n in
  let start_n = get_some (InterCfg.PidMap.find "_G_" pid2n) in
  let n2access = make_n2access n2pids access_proc in
  let n2reach_access =
    make_n2reach_access n2access n2next_ns start_n BatMap.empty in
  let pid2reach_access = make_pid2reach_access n2reach_access pid2n in
  pid2reach_access

let init_access_local_proc access_proc =
  let update_loc2proc_1 pid loc (loc2proc, nonlocals) =
    if LocMap.mem loc loc2proc then
      let loc2proc = LocMap.remove loc loc2proc in
      let nonlocals = PowLoc.add loc nonlocals in
      (loc2proc, nonlocals)
    else
      let loc2proc = LocMap.add loc pid loc2proc in
      (loc2proc, nonlocals) in
  let update_loc2proc pid acc_of_pid (loc2proc, nonlocals) =
    let locs = PowLoc.diff (Acc.accessof acc_of_pid) nonlocals in
    PowLoc.fold (update_loc2proc_1 pid) locs (loc2proc, nonlocals) in
  let ((loc2proc, _) : string LocMap.t * PowLoc.t) =
    InterCfg.PidMap.fold update_loc2proc access_proc
      (LocMap.empty, PowLoc.empty) in
  let proc2locs = InterCfg.PidMap.map (fun _ -> PowLoc.empty) access_proc in
  let add_loc_pid loc pid proc2locs =
    let locs = default PowLoc.empty (InterCfg.PidMap.find pid proc2locs) in
    InterCfg.PidMap.add pid (PowLoc.add loc locs) proc2locs in
  LocMap.fold add_loc_pid loc2proc proc2locs

let init_access_local_program access_local_proc =
  let union_access _ access = PowLoc.union access in
  InterCfg.PidMap.fold union_access access_local_proc PowLoc.empty

let init_defs_of access_map =
  let add_node node loc defs_of =
    let nodes = default InterCfg.NodeSet.empty (LocMap.find loc defs_of) in
    LocMap.add loc (InterCfg.NodeSet.add node nodes) defs_of in
  let init_defs_of1 node access defs_of =
    PowLoc.fold (add_node node) (Acc.defof access) defs_of in
  InterCfg.NodeMap.fold init_defs_of1 access_map LocMap.empty

let init_uses_of access_map =
  let add_node node loc uses_of =
    let nodes = default InterCfg.NodeSet.empty (LocMap.find loc uses_of) in
    LocMap.add loc (InterCfg.NodeSet.add node nodes) uses_of in
  let init_uses_of1 node access uses_of =
    PowLoc.fold (add_node node) (Acc.useof access) uses_of in
  InterCfg.NodeMap.fold init_uses_of1 access_map LocMap.empty

let fixpt nodes (m, g) =
  let rec fixpt' nodes k (m, g) =
    if not !Options.opt_nobar then
      (prerr_string ("\r#iters: " ^ string_of_int k);
       flush stderr);
    let (m', g') = InterCfg.NodeSet.fold (Run.run PrePhase) nodes (m, g) in
    let m' = Mem.widen m m' in
    if Mem.le_dec m' m && Dump.le_dec (G.dump g') (G.dump g) then
      (prerr_newline ();
       prerr_endline ("#mem entries: " ^ string_of_int (Mem.cardinal m'));
       (m', g'))
    else fixpt' nodes (k + 1) (m', g') in
  fixpt' nodes 1 (m, g)

let callees_of icfg node mem =
  let pid = InterNode.get_pid node in
  match InterCfg.get_cmd icfg node with
  | Some c ->
    (match c with
     | Ccall (_, Lval (Coq_lval_intro (VarLhost (vi, _), NoOffset, _), _), _, _)
       when vi = "zoo_print" -> InterCfg.PidSet.empty
     | Ccall (_, Lval (Coq_lval_intro (VarLhost (vi, _), NoOffset, _), _), _, _)
       when vi = "zoo_dump" -> InterCfg.PidSet.empty
     | Ccall (_, e, _, _) ->
       let v = SemEval.eval idMem mId pid e mem in
       PowProc.fold InterCfg.PidSet.add (pow_proc_of_val v)
         InterCfg.PidSet.empty
     | _ -> InterCfg.PidSet.empty )
  | None -> InterCfg.PidSet.empty

let init_node_calls icfg nodes m cg =
  let add_node_calls icfg m node =
    if InterCfg.is_call_node icfg node then
      InterCfg.NodeMap.add node (callees_of icfg node m)
    else id in
  let node_calls =
    InterCfg.NodeSet.fold (add_node_calls icfg m) nodes
      InterCfg.NodeMap.empty in
  { cg with Global.Callgraph.node_calls = node_calls }

let init_calls pids cg =
  let init_map =
    let add_empty_set pid m =
      InterCfg.PidMap.add pid InterCfg.PidSet.empty m in
    list_fold add_empty_set pids InterCfg.PidMap.empty in
  let calls_add node new_callees calls =
    let caller = InterNode.get_pid node in
    let callees =
      default InterCfg.PidSet.empty (InterCfg.PidMap.find caller calls) in
    let callees = InterCfg.PidSet.union callees new_callees in
    InterCfg.PidMap.add caller callees calls in
  let calls = InterCfg.NodeMap.fold calls_add
      (Global.Callgraph.node_calls cg) init_map in
  { cg with Global.Callgraph.calls = calls }

let make_pid2n pids pid2n_func scc_num =
  let add_pid2n pid (m, n) =
    let (k, n) = try (pid2n_func pid, n) with Not_found -> (n, n + 1) in
    (InterCfg.PidMap.add pid k m, n) in
  list_fold add_pid2n pids (InterCfg.PidMap.empty, scc_num)

let make_n2pids pids group_num pid2n =
  let n2pids_empty =
    let rec n2pids_empty_rec n m =
      if n <= 0 then m else
        let n' = n - 1 in
        n2pids_empty_rec n' (BatMap.add n' InterCfg.PidSet.empty m) in
    n2pids_empty_rec group_num BatMap.empty in
  let add_n2pid pid m =
    let n = get_some (InterCfg.PidMap.find pid pid2n) in
    let pids = InterCfg.PidSet.add pid (BatMap.find n m) in
    BatMap.add n pids m in
  list_fold add_n2pid pids n2pids_empty

let make_n2next_pids calls n2pids =
  let add_pid2next_pids pid acc =
    let callees =
      default InterCfg.PidSet.empty (InterCfg.PidMap.find pid calls) in
    InterCfg.PidSet.union acc callees in
  let pids2next_pids pids =
    InterCfg.PidSet.fold add_pid2next_pids pids InterCfg.PidSet.empty in
  BatMap.map pids2next_pids n2pids

let make_n2next_ns n2next_pids pid2n =
  let add_pid2n pid acc =
    let n = get_some (InterCfg.PidMap.find pid pid2n) in
    BatSet.add n acc in
  let pids2ns pids = InterCfg.PidSet.fold add_pid2n pids BatSet.empty in
  BatMap.map pids2ns n2next_pids

let make_n2trans_ns group_num n2next_ns =
  let rec one_update_n2trans_ns n m =
    if BatMap.mem n m then m else
      let next_ns = BatMap.find n n2next_ns in
      let (is_rec, next_ns') =
        if BatSet.mem n next_ns then (true, BatSet.remove n next_ns) else
          (false, next_ns) in
      let m = BatSet.fold one_update_n2trans_ns next_ns' m in
      let add_trans_ns n acc = BatSet.union acc (BatMap.find n m) in
      let trans_ns = next_ns in
      let trans_ns = BatSet.fold add_trans_ns next_ns' trans_ns in
      BatMap.add n trans_ns m in
  let rec all_update_n2trans_ns n m =
    if n <= 0 then m else
      let n' = n - 1 in
      let m = one_update_n2trans_ns n' m in
      all_update_n2trans_ns n' m in
  all_update_n2trans_ns group_num BatMap.empty

let make_n2trans_pids n2trans_ns n2pids =
  let add_n2pids n acc = InterCfg.PidSet.union acc (BatMap.find n n2pids) in
  let ns2pids ns = BatSet.fold add_n2pids ns InterCfg.PidSet.empty in
  BatMap.map ns2pids n2trans_ns

let make_trans_calls n2trans_pids n2pids =
  let add_pid2trans_pids trans_pids pid acc =
    InterCfg.PidMap.add pid trans_pids acc in
  let add_n2trans_pids n trans_pids acc =
    let pids = BatMap.find n n2pids in
    InterCfg.PidSet.fold (add_pid2trans_pids trans_pids) pids acc in
  BatMap.foldi add_n2trans_pids n2trans_pids InterCfg.PidMap.empty

let init_g_calls calls =
  let add_callee caller callee acc = ProcG.add_edge acc caller callee in
  let add_callees caller callees =
    InterCfg.PidSet.fold (add_callee caller) callees in
  InterCfg.PidMap.fold add_callees calls ProcG.empty

let init_trans_calls pids cg =
  let calls = Global.Callgraph.calls cg in
  let g_calls = init_g_calls calls in
  let (scc_num, pid2n_func) = SCC.scc g_calls in
  let (pid2n, group_num) = make_pid2n pids pid2n_func scc_num in
  let n2pids = make_n2pids pids group_num pid2n in
  let n2next_pids = make_n2next_pids calls n2pids in
  let n2next_ns = make_n2next_ns n2next_pids pid2n in
  let n2trans_ns = make_n2trans_ns group_num n2next_ns in
  let n2trans_pids = make_n2trans_pids n2trans_ns n2pids in
  let trans_calls = make_trans_calls n2trans_pids n2pids in
  { cg with Global.Callgraph.trans_calls = trans_calls }

let init_callgraph nodes g m =
  let icfg = G.icfg g in
  let pids = InterCfg.pidsof icfg in
  let cg =
    G.callgraph g
    |> init_node_calls icfg nodes m
    |> init_calls pids
    |> init_trans_calls pids in
  { g with G.callgraph = cg }

let get_reachable pid cg =
  let trans_calls = Global.Callgraph.trans_calls cg in
  let trans_callees = get_some (InterCfg.PidMap.find pid trans_calls) in
  InterCfg.PidSet.add pid trans_callees

let remove_unreachable_functions (info, g) =
  let pids_all =
    list_fold InterCfg.PidSet.add (InterCfg.pidsof (G.icfg g))
      InterCfg.PidSet.empty in
  let reachable = get_reachable "_G_" (G.callgraph g) in
  let unreachable = InterCfg.PidSet.diff pids_all reachable in
  let g = InterCfg.PidSet.fold G.remove_function unreachable g in
  prerr_string "#functions: ";
  prerr_endline (string_of_int (InterCfg.PidSet.cardinal pids_all));
  prerr_string "#unreachable functions: ";
  prerr_endline (string_of_int (InterCfg.PidSet.cardinal unreachable));
  g

let unreachable_node_intra (cfg : IntraCfg.t) : IntraCfg.NodeSet.t =
  let nodes = IntraCfg.nodes cfg in
  let rec remove_reachable_node work acc =
    match IntraCfg.NodeSet.choose work with
    | Some node ->
      let work = IntraCfg.NodeSet.remove node work in
      if IntraCfg.NodeSet.mem node acc then
        let acc = IntraCfg.NodeSet.remove node acc in
        let succ_map = IntraCfg.succ cfg in
        let succs_opt = IntraCfg.NodeMap.find node succ_map in
        let succs = default IntraCfg.NodeSet.empty succs_opt in
        let succs = IntraCfg.NodeSet.remove node succs in
        let work = IntraCfg.NodeSet.union work succs in
        remove_reachable_node work acc
      else remove_reachable_node work acc
    | None -> acc in
  remove_reachable_node (IntraCfg.NodeSet.singleton IntraNode.Entry)
    nodes

let unreachable_node_inter icfg =
  let add_pid_nodes pid nodes =
    let add_pid_node node = InterCfg.NodeSet.add (pid, node) in
    IntraCfg.NodeSet.fold add_pid_node nodes InterCfg.NodeSet.empty in
  let add_unreachable_node pid cfg =
    let intra_nodes = unreachable_node_intra cfg in
    let inter_nodes = add_pid_nodes pid intra_nodes in
    InterCfg.NodeSet.union inter_nodes in
  InterCfg.PidMap.fold add_unreachable_node (InterCfg.cfgs icfg)
    InterCfg.NodeSet.empty

let remove_unreachable_nodes g =
  let icfg = G.icfg g in
  let unreachable = unreachable_node_inter icfg in
  let g = InterCfg.NodeSet.fold G.remove_node unreachable g in
  prerr_string "#nodes: ";
  prerr_endline
    (string_of_int (InterCfg.NodeSet.cardinal (InterCfg.nodes icfg)));
  prerr_string "#unreachable nodes: ";
  prerr_endline (string_of_int (InterCfg.NodeSet.cardinal unreachable));
  g

let get_single_defs defs_of =
  let add_single_def loc nodes =
    if InterCfg.NodeSet.cardinal nodes = 1 then PowLoc.add loc else id in
  LocMap.fold add_single_def defs_of PowLoc.empty

let init_access_info (m, g) =
  let pids = InterCfg.pidsof (G.icfg g) in
  let (total_abslocs, access) = init_access (m, g) in
  prerr_string "#abstract locations: ";
  prerr_endline (string_of_int (PowLoc.cardinal total_abslocs));
  let defs_of = init_defs_of access in
  let uses_of = init_uses_of access in
  let access_proc = init_access_proc access in
  let access_reach = init_access_reach pids (G.callgraph g) access_proc in
  let access_local_proc = init_access_local_proc access_proc in
  let access_local_program = init_access_local_program access_local_proc in
  { mem = m
  ; total_abslocs = total_abslocs
  ; access = access
  ; access_proc = access_proc
  ; access_reach = access_reach
  ; access_local_proc = access_local_proc
  ; access_local_program = access_local_program
  ; defs_of = defs_of
  ; uses_of = uses_of }

let analyze g =
  let g = Step.small "Remove unreachable nodes" remove_unreachable_nodes g in
  let nodes = InterCfg.nodes (G.icfg g) in
  let (m, g) = Step.small "Fixpoint iteration" (fixpt nodes) (Mem.bot, g) in
  let g = Step.small "Initialize callgraph" (init_callgraph nodes g) m in
  let info = Step.small "Initialize access info" init_access_info (m, g) in
  let g = Step.small "Remove unreachable functions"
      remove_unreachable_functions (info, g) in
  let g = Step.small "Draw inter edges" Trans.t_inter_edges g in
  (info, g)
