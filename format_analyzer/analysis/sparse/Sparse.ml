open VocabB
open UserInputType
open UserInput.Input
open Vali.Vali
open Pre
open Dug

let total_iters = ref 0
let g_clock = ref 0.0

module Workorder = Worklist.Workorder
let needwidening idx order = Workorder.is_loopheader idx order

let def_locs_cache = Hashtbl.create 251
let get_def_locs idx dug =
  try Hashtbl.find def_locs_cache idx with Not_found ->
  let def_locs =
    let union_locs succ = PowLoc.union (get_abslocs idx succ dug) in
    let def_locs = list_fold union_locs (succ idx dug) PowLoc.empty in
    PowLoc.fold (fun x acc -> x :: acc) def_locs [] in
  Hashtbl.add def_locs_cache idx def_locs; def_locs

(* fixpoint iterator specialized to the widening phase *)
let analyze_node fs_locs idx (works, g, dug, pre, inputof, outputof, order) =
  total_iters := !total_iters + 1 ;
  if !total_iters = 1 then g_clock := Sys.time() else
  if !total_iters mod 10000 = 0 then
    (Printf.eprintf "#iters: %d took %.1f sec\n" !total_iters
       (Sys.time () -. !g_clock);
     flush stderr;
     g_clock := Sys.time ())
  else ();
  let input = Table.find idx inputof in
  let old_output = Table.find idx outputof in
  let (new_output, g) =
    Run.run ~mode:Strong ~fs_locs MainPhase idx (input, g) in

  let (new_output, b_stable, unstables) =
    let def_locs = get_def_locs idx dug in
    let is_unstb v1 v2 = not (Mem.B.le_dec v2 v1) in
    let u = Mem.unstables old_output new_output is_unstb def_locs in
    let op = if needwidening idx order then Mem.B.widen else Mem.B.join in
    let u = List.map (fun ((k, v1), v2) -> (k, op v1 v2)) u in
    let new_output =
      list_fold (fun (k, v) -> Mem.add fs_locs k v) u old_output in
    let u = List.map (fun (k, _) -> (k, Mem.find k new_output)) u in
    (new_output, u = [], u) in

  if b_stable then (works, g, dug, pre, inputof, outputof, order) else
    let id1 = Worklist.NodeMap.find idx order.Workorder.order in
    let (works, inputof) =
      let update_succ succ (works, inputof) =
        let old_input = Table.find succ inputof in
        let locs_on_edge = get_abslocs idx succ dug in
        let is_on_edge (x, _) = mem_abslocs x locs_on_edge in
        let to_join = List.filter is_on_edge unstables in
        if to_join = [] then (works, inputof) else
          let weak_add (k, v) = Mem.main_weak_add fs_locs k v in
          let new_input = list_fold weak_add to_join old_input in
          let id2 = Worklist.NodeMap.find succ order.Workorder.order in
          let bInnerLoop = Worklist.compare_order (id2, succ) (id1, idx) in
          (Worklist.queue bInnerLoop order.Workorder.headorder succ id2 works,
           Table.add succ new_input inputof) in
      let succs = succ idx dug in
      list_fold update_succ succs (works, inputof) in
    (works, g, dug, pre, inputof, Table.add idx new_output outputof, order)

let rec iterate fs_locs (widen, join, le)
    (works, g, dug, pre, inputof, outputof, order) =
  match Worklist.pick works with
  | None -> (works, g, dug, pre, inputof, outputof, order)
  | Some (idx, rest) ->
    (rest, g, dug, pre, inputof, outputof, order)
    |> analyze_node fs_locs idx
    |> iterate fs_locs (widen, join, le)

let widening fs_locs (g, dug, pre, inputof, outputof, order) =
  total_iters := 0;
  let init_worklist = Worklist.init order.Workorder.order dug in
  let (_, g, dug, pre, inputof, outputof, order) =
    iterate fs_locs (Mem.B.widen, Mem.B.join, Mem.B.le_dec)
      (init_worklist, g, dug, pre, inputof, outputof, order) in
  prerr_endline ("#total iters: " ^ string_of_int !total_iters);
  (g, dug, pre, inputof, outputof, order)

(* computing the gfp upper bounded by ub *)
let narrowing fs_locs (g, dug, pre, inputof, outputof, order) =
  total_iters := 0;
  let initnodes = nodesof dug in
  let init_worklist = Worklist.init_nodes order.Workorder.order initnodes in
  let (_, g, dug, pre, inputof, outputof, order) =
    iterate fs_locs
      (Mem.B.narrow, Mem.B.join, fun x y -> Mem.B.le_dec y x)
      (init_worklist, g, dug, pre, inputof, outputof, order) in
  prerr_endline ("#iteration in narrowing : " ^ string_of_int !total_iters);
  (g, dug, pre, inputof, outputof, order)

let bind_fi_mem (g, fs_locs, memFI, inputof, outputof) =
  let is_not_in ls k _ = not (PowLoc.mem k ls) in
  let cfgs = InterCfg.cfgs (G.icfg g) in
  let m_fi = Mem.Mem.filteri (is_not_in fs_locs) (Mem.base memFI) in
  let update_base m node =
    Table.add node { Mem.base = m; Mem.spec = Mem.Mem.empty } in
  let iter_node m_fi f node (inputof, outputof) =
    (update_base m_fi (f, node) inputof,
     update_base m_fi (f, node) outputof) in
  let iter_f f cfg (inputof, outputof) =
    let nodes_f = IntraCfg.nodes cfg in
    IntraCfg.NodeSet.fold (iter_node m_fi f) nodes_f (inputof, outputof) in
  InterCfg.PidMap.fold iter_f cfgs (inputof, outputof)

let perform (g, dug, pre, fs_locs, order) =
  (* flow-insensitive analysis *)
  let nodes = InterCfg.nodes (G.icfg g) in
  let (memFI, _) =
    Step.small "Flow-insensitive analysis" (Pre.fixpt nodes) (Mem.bot, g) in

  (* binding flow insensitive locations *)
  let (inputof, outputof) =
    Step.small "Bind flow-insensitive entries"
      bind_fi_mem (g, fs_locs, memFI, Table.empty, Table.empty) in

  (* widening *)
  let (g, dug, pre, inputof, outputof, order) =
    Step.small "Fixpoint iteration"
      (widening fs_locs) (g, dug, pre, inputof, outputof, order) in

  (* meet with memFI *)
  let meet_memFI' memFI m =
    { Mem.base = Mem.base m
    ; Mem.spec = Mem.Mem.meet_big_small memFI (Mem.spec m) } in
  let meet_memFI (inputof, outputof) =
    let is_in ls l _ = PowLoc.mem l ls in
    let memFI = Mem.Mem.filteri (is_in fs_locs) (Mem.base memFI) in
    (Table.map (meet_memFI' memFI) inputof,
     Table.map (meet_memFI' memFI) outputof) in
  let (inputof, outputof) =
    Step.small "Meet with flow-insensitive memory"
      meet_memFI (inputof, outputof) in

  (* narrowing *)
  let (g, dug, pre, inputof, outputof, order) =
    Step.small_opt !Options.opt_narrow "Narrowing"
      (narrowing fs_locs) (g, dug, pre, inputof, outputof, order) in

  (inputof, outputof, memFI)

(* Merge m1 and m2 while taking m2(x) if x is in locs *)
let merge_over locs m1 m2 =
  let add_when_in k m1 = Mem.Mem.add k (Mem.Mem.find k m2) m1 in
  PowLoc.fold add_when_in locs m1

let filter_locs locs m =
  Mem.filteri (fun l _ -> Mem.PowA.mem l locs) m

let get_use_locs_by_du idx_here dug =
  let add_locs pred = PowLoc.union (get_abslocs pred idx_here dug) in
  list_fold add_locs (pred idx_here dug) PowLoc.empty

let get_def_locs_by_du idx_here dug =
  let add_locs succ = PowLoc.union (get_abslocs idx_here succ dug) in
  list_fold add_locs (succ idx_here dug) PowLoc.empty

(* Generate the full input table for a given procedure : exploits the
   SSA property that the value of a location not used at a node is
   identical to the value at the immediate dominator of the node *)
let densify_cfg (g, dug, pre, fs_locs) inputof outputof (f, cfg) f_dom_tree =
  let rec propagate here (inputof, outputof) =
    let idx_here = (f, here) in
    let use_locs = get_use_locs_by_du idx_here dug in
    let def_locs = get_def_locs_by_du idx_here dug in
    let input_here = Table.find idx_here inputof in
    let input_here_base = Mem.base input_here in
    let input_here_spec = Mem.spec input_here in
    let output_here = Table.find idx_here outputof in
    let output_here_base = Mem.base output_here in
    let output_here_spec = Mem.spec output_here in
    let d_input_here_spec =
      let basic_mem_spec =
        match Dug.parent_of_dom_tree here f_dom_tree with
        | None -> Mem.Mem.bot
        | Some idom -> Mem.spec (Table.find (f, idom) outputof) in
      merge_over use_locs basic_mem_spec input_here_spec in
    let d_input_here =
      { Mem.base = input_here_base; Mem.spec = d_input_here_spec } in
    let d_output_here_spec =
      let (output_here', _) =
        Run.run ~mode:Strong ~fs_locs MainPhase idx_here (d_input_here, g) in
      merge_over def_locs (Mem.spec output_here') output_here_spec in
    let d_output_here =
      { Mem.base = output_here_base; Mem.spec = d_output_here_spec } in
    let d_inputof = Table.add idx_here d_input_here inputof in
    let d_outputof = Table.add idx_here d_output_here outputof in
    let nodes_dom_ordered = Dug.children_of_dom_tree here f_dom_tree in
    IntraCfg.NodeSet.fold propagate nodes_dom_ordered (d_inputof, d_outputof)
  in
  propagate IntraNode.Entry (inputof, outputof)

let densify (g, dug, pre, fs_locs, inputof, outputof, dom_tree) =
  let icfg = G.icfg g in
  let pids = InterCfg.pidsof icfg in
  let cfgs = InterCfg.cfgs icfg in
  let todo = List.length pids in
  let iter_func f (k, (inputof, outputof)) =
    let f_dom_tree = get_some (InterCfg.PidMap.find f dom_tree) in
    prerr_progressbar k todo;
    match InterCfg.PidMap.find f cfgs with
    | Some cfg ->
      (k + 1,
       densify_cfg (g, dug, pre, fs_locs) inputof outputof (f, cfg) f_dom_tree)
    | None -> (k + 1, (inputof, outputof)) in
  (1, (inputof, outputof))
  |> list_fold iter_func pids
  |> snd
  
let analyze (pre, g) =
	(* if !Options.opt_pfs = 0 then                                                                *)
	(* 	let fs_locs = PowLoc.empty in                                                             *)
	(* 	let dug = Dug.empty in                                                                    *)
	(* 	let memFI = Pre.get_mem pre in                                                            *)
	(* 	let (inputof, outputof) =                                                                 *)
  (*   	Step.small "Bind flow-insensitive entries"                                              *)
  (*     	bind_fi_mem (g, fs_locs, memFI, Table.empty, Table.empty) in                          *)
	(* 	let dom_tree = InterCfg.PidMap.empty in                                                   *)
	(* 	let (d_inputof, d_outputof) =                                                             *)
  (*     if !Options.opt_densify then                                                            *)
  (*       Step.small "Densify"                                                                  *)
  (*         densify (g, dug, pre, fs_locs, inputof, outputof, dom_tree)                         *)
  (*     else (inputof, outputof) in                                                             *)
	(* 	(dug, fs_locs, memFI, inputof, outputof, d_inputof, d_outputof, Worklist.Workorder.empty) *)
  (* else                                                                                        *)
	  let abslocs = Pre.get_total_abslocs pre in
    let fs_locs = Step.small "Rank locations" VarSelect.rank (abslocs, pre, g) in
    let (dug, dom_tree) =
      Step.small "Def-use graph construction" Dug.icfg2dug (g, pre, fs_locs) in
    let order = Step.small "Compute workorder"
      Worklist.Workorder.perform (g, dug) in
    let (inputof, outputof, memFI) = perform (g, dug, pre, fs_locs, order) in
    let (d_inputof, d_outputof) =
      if !Options.opt_densify then
        Step.small "Densify"
          densify (g, dug, pre, fs_locs, inputof, outputof, dom_tree)
      else (inputof, outputof) in
    (dug, fs_locs, memFI, inputof, outputof, d_inputof, d_outputof, order)
