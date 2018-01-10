open Pos
open VocabA
open VocabB
open PPVocab
open UserInput.Input
open CallGraph
open Vali.Vali
module PMap = BatMap.PMap
exception NoSink
module CGDom = Graph.Dominator.Make (CG)
let total_io_time = ref 0.0
let timer_start () = Sys.time ()
let print_time t =
  let t' = Sys.time () in
  prerr_endline ("Get Path Total Time is " ^ (string_of_float (t' -. t)))


let print_scc_decompose call_g src sink =
  let scc_list = SCC.SCC.scc_list call_g in
  let scc_list = List.filter (fun l -> List.length l > 1) scc_list in
  let scc_list_length = List.length scc_list in
  let flat_scc = List.flatten scc_list in
  let flat_scc_length = List.length flat_scc in
  let nb_v = CG.nb_vertex call_g in
  prerr_endline ("Number of SCC :" ^ (string_of_int scc_list_length));
  prerr_endline ("Number of SCC vertex : " ^ (string_of_int flat_scc_length));
  prerr_endline ("Before SCC Decompose : " ^ (string_of_int nb_v));
  prerr_endline
    ("After SCC Decompose : " ^
     (string_of_int (nb_v - flat_scc_length + scc_list_length)));
  let src_scc = List.exists (fun v -> CG.V.compare src v = 0) flat_scc in
  let sink_scc = List.exists (fun v -> CG.V.compare sink v = 0) flat_scc in
  if src_scc && sink_scc then
    prerr_endline "@@@@Src Sink in SCC"
  else if src_scc then prerr_endline "@@@@@Src in SCC"
  else if sink_scc then prerr_endline "@@@@@Sink in SCC"
  else ()



let get_src_sink_nodes glob_g pos_map m =
  let icfg = G.icfg glob_g in
  let find_node pos = PMap.find (Pos_as_OT.string_of_pos pos) pos_map in
  (* avoiding duplications of sources *)
  let is_duplicated_src src_node ext acc =
    (* alarms of the same positions are considered the same *) 
    let cond1 =
      List.exists (fun n -> InterNode.eq_dec n src_node) (List.map fst acc)
    in 
    (* alarms of the sources in the same function are considered the same *) 
    let cond2 =
      List.exists (fun n -> String.compare (InterNode.get_pid n) (InterNode.get_pid src_node) = 0) (List.map fst acc)
    in 
    (* alarms of the same src function are considered the same *)
    let cond3 =
      List.exists (fun e -> Proc.eq_dec (ExtProcPos.fst e) (ExtProcPos.fst ext)) (List.map snd acc)
    in
    cond2 
  in  
  let get_src_sink_node sink_pos v lst =
    match v with 
    | [] -> lst 
    | (q, Clean) :: tl -> lst
    | (q, Tainted exts) :: tl ->
      let extList = PowExtProcPos.elements exts in
      let sink_node = try find_node sink_pos with _ -> raise NoSink in 
      let fold_srclist acc ext =
        let src_node = find_node (snd ext) in
	(* for call graph projection *)
	let src_info = ((InterNode.get_pid src_node, IntraNode.Entry), ext) in
	if is_duplicated_src src_node ext acc then acc else src_info :: acc
      in
      let src_nodes = List.fold_left fold_srclist [] extList in
      let sink_node = (InterNode.get_pid sink_node, IntraNode.Entry) in
      (* alarms of the same enclosing function of sink are considered the same *)
      if List.exists (fun n -> String.compare (InterNode.get_pid n) (InterNode.get_pid sink_node) = 0) (List.map fst lst) then lst 
      else (sink_node, src_nodes) :: lst
  in
  PMap.foldi get_src_sink_node m []

let make_nodes2pos g nodes =
  let node2pos map node =
    let cmd = Run.get_cmd node g in 
    match cmd with 
    | Syn.Ccall (_, _, _, pos) -> 
      PMap.add (Pos_as_OT.string_of_pos pos) node map
    | _ -> map
  in
  List.fold_left node2pos PMap.empty nodes

let make_callgraph glob =
  let icfg = G.icfg glob in
  let fold_callers caller callees g =
    let fold_callees callee g =
      if String.compare callee caller = 0 then
        g
      else
        let entry_n = IntraNode.Entry in
        let v1 = (caller, entry_n) in
        let v2 = (callee, entry_n) in
        (* Add call-edge *)
        CG.add_edge_e g (v1, "C", v2)
    in
    InterCfg.PidSet.fold fold_callees callees g
  in
  let call_map = Global.Callgraph.calls (G.callgraph glob) in
  InterCfg.PidMap.fold fold_callers call_map CG.empty

(** Return (callgraph', calledge list, retedge list *)
let make_callgraph' call_g =
  let fold_edges v1 v2 g =
    CG.add_edge_e g (v2, "R", v1)
  in
  CG.fold_edges fold_edges call_g call_g
  
(* TODO : elaborating def-use informations using this function *)
let get_powloc_expr icfg node m =
  let pid = InterNode.get_pid node in
  let cmd = (default (Syn.Cskip Pos.Pos_as_OT.unknown_pos) (InterCfg.get_cmd icfg node)) in
  let expr =
    match cmd with
    | Syn.Ccall (ret, f, args, _) ->
      (match is_printf f with
       | Some fname ->
         if is_printf1 fname then
           match args with
           | e :: _ -> e
           | _ -> assert(false)
         else
         if is_printf2 fname then
           match args with
           | _ :: e :: _ -> e
           | _ -> assert(false)
         else
         if is_printf3 fname then
           match args with
           | _ :: _ :: e :: _-> e
           | _ -> assert(false)
         else
           assert(false)
       | None ->
         assert(false))
    | _ -> assert(false) in

  let acc_v =
    SemEval.eval
      (Obj.magic DomMem.coq_AccMem) (Obj.magic DomMem.coq_MAcc) pid expr m in
  snd (*Access.useof*) (get_acc acc_v)

let get_def_nodes call_g pre ext src_node sink_node = []
  (* let entry_node = ("_G_", IntraNode.Entry) in                                  *)
  (* let idM = (Obj.magic DomMem.coq_IdMem) in                                     *)
  (* let mId = (Obj.magic DomMem.coq_MId) in                                       *)

  (* let use_def_lists =                                                           *)
  (*   let mem = Pre.get_mem pre in                                                *)
  (*   let fold_use_def_list v acc =                                               *)
  (*     let f = InterNode.get_pid v in                                            *)
  (*     let use_locs = Acc.useof (Pre.get_access_proc pre f) in                   *)
  (*     let filter_tnt loc =                                                      *)
	(* let v = mem_lookup idM mId (PowLoc.singleton loc) mem in                      *)
	(* (* let v' = mem_lookup idM mId (pow_loc_of_val v) mem in *)                   *)
	(* let v' = DomAbs.Val.bot in                                                    *)
	(* let tnt = pow_proc_pos_of_val (DomAbs.Val.join v v') in                       *)
  (*       PowExtProcPos.mem ext tnt                                               *)
  (*     in                                                                        *)
  (*     let tainted_use_locs = PowLoc.filter filter_tnt use_locs in               *)
  (*     if (PowLoc.is_empty tainted_use_locs) then                                *)
  (*       acc                                                                     *)
  (*     else                                                                      *)
  (*       let fold_def_func v acc =                                               *)
  (*         let g = InterNode.get_pid v in                                        *)
  (*         let def_locs = Acc.defof (Pre.get_access_proc pre g) in               *)
  (*         let tainted_def_locs = PowLoc.intersect def_locs tainted_use_locs in  *)
  (*         if (not (PowLoc.is_empty tainted_def_locs)) &&                        *)
  (* 	     (String.compare f g) != 0 then                                         *)
  (*           (g :: acc)                                                          *)
  (*         else acc                                                              *)
  (*       in                                                                      *)
  (*       let def_funcs = CG.fold_vertex fold_def_func call_g [] in               *)
  (*       let def_funcs =                                                         *)
  (*         (* these functions are certainly visited, so redundant information *) *)
  (*         let filter_main_g func = (String.compare func "_G_") != 0  in         *)
  (*         List.filter filter_main_g def_funcs                                   *)
  (*       in                                                                      *)
	(* if (List.length def_funcs) > 0 then                                           *)
  (*         (InterNode.entryof f, List.map InterNode.entryof def_funcs) :: acc    *)
	(* else acc                                                                      *)
  (*   in                                                                          *)
  (*   CG.fold_vertex fold_use_def_list call_g []                                  *)
  (* in                                                                            *)
  (* use_def_lists                                                                 *)


let process_alarms glob_g call_g call_g' pre s1_s2_nodes =
  let icfg = G.icfg glob_g in
  let fold_src_sink_nodes (o_cnt, acc) (sink_node, src_nodes_ext) =
    let fold_src_nodes (i_cnt, acc) (src_node, ext) =
      (* Get use -> def list nodes *)
      let prop_info = get_def_nodes call_g pre ext src_node sink_node in
      (* MaxSat *)
      print_scc_decompose call_g src_node sink_node;
      prerr_endline "Do MaxSAT!";
      let t = timer_start () in
      let _ = MaxSat.reset_io_time () in
      let result =
        MaxSatHeuristic.get_path icfg call_g call_g' src_node sink_node
      in
      total_io_time := !total_io_time +. !MaxSat.io_time;
      print_time t;
      let name = (Format.sprintf "alarm%d_%d" o_cnt i_cnt) in
      (* TODO : Marshaling *)
      (i_cnt+1, acc @ [(src_node, sink_node, call_g, result, prop_info)])
    in
    let (_, result) = List.fold_left fold_src_nodes (1,[]) src_nodes_ext in
    (o_cnt+1, acc @ [result])
  in
  let (_, result) = List.fold_left fold_src_sink_nodes (1,[]) s1_s2_nodes in
  result
  

let run glob_g pre res =
  (* AlarmVis.analysis_result_marshal "analysis" glob_g pre res; *)
  let message = "Generating callgraphs for each queries" in
  let icfg = G.icfg glob_g in
  let call_g = make_callgraph glob_g in
  print_scc_decompose call_g;
  (* draw all return edges *)
  let call_g' = make_callgraph' call_g in
  let fold_nodes pid cfg nodes =
    let elements = IntraCfg.NodeSet.elements (IntraCfg.nodes cfg) in
    nodes @ List.map (fun n -> (pid,n)) elements
  in
  let all_nodes = InterCfg.PidMap.fold fold_nodes (InterCfg.cfgs icfg) [] in
  let pos_map = make_nodes2pos glob_g all_nodes in
  let s1_s2_nodes = get_src_sink_nodes glob_g pos_map res in
  let alarm_hunt_result =
    Step.small message process_alarms glob_g call_g call_g' pre s1_s2_nodes 
  in
  let _ =
    prerr_endline ("@@@Total IO time is " ^ (string_of_float !total_io_time))
  in
  let _ = AlarmVis.vis_alarms icfg alarm_hunt_result (call_g, call_g') in
  ()
