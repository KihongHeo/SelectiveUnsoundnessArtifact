open Dug
open BFormula
open CallGraph

module SCC = Components.Make (CG)

let has_vertex v scc =
  List.exists (fun v' -> CG.V.compare v' v = 0) scc

let is_call_scc new_call_g vlist =
	(* it is a call-scc if a single edge in the scc is a call edge *)
	let v1, lst = (List.hd vlist, List.tl vlist) in 
	List.exists (fun v2 -> CG.mem_edge_e new_call_g (v1, "C", v2)) lst

let is_ret_scc new_call_g vlist =
	(* it is a call-scc if a single edge in the scc is a call edge *)
	let v1, lst = (List.hd vlist, List.tl vlist) in 
	List.exists (fun v2 -> CG.mem_edge_e new_call_g (v1, "R", v2)) lst

let allEdges_scc mode new_call_g vlist =
  let fold_v acc v =
    let succ_e = CG.succ_e new_call_g v in
    (* Filter successor edges only in SCC, and fits the type (call/return) *)
    let filter_succ_e e =
      let dst = CG.E.dst e in
      (List.exists (fun v' -> CG.V.compare v' dst = 0) vlist) && 
			((String.compare (CG.E.label e) mode) = 0)
    in
    let succ_e = List.filter filter_succ_e succ_e in
    succ_e @ acc
  in
  List.fold_left fold_v [] vlist

(** Fact (f) get outgoing edges from SCC which has src node
    call scc -> out : call
    ret scc -> out : call or ret
*)
let out_loop modes call_g new_call_g vlist idedges =  
  let fold_v mode acc v =
    let succ_e = CG.succ_e call_g v in
    (* Filter successor edges without in SCC *)
    let filter_succ_e e =
      let dst = CG.E.dst e in
      not (List.exists (fun v' -> CG.V.compare v' dst = 0) vlist)
    in
    let succ_e = List.filter filter_succ_e succ_e in
    let pred_e = CG.pred_e call_g v in
    (* Filter predecessor edges without in SCC *)
    let filter_pred_e e =
      let src = CG.E.src e in
      not (List.exists (fun v' -> CG.V.compare v' src = 0) vlist)
    in
    let pred_e = CG.pred_e call_g v in
    (* Get outgoing return edges *)
    let pred_e = List.map (fun (v1, _, v2) -> (v2, "R", v1)) pred_e in
    succ_e @ (if (String.compare mode "R") = 0 then pred_e else []) @ acc
  in
	List.fold_left (fun acc mode ->
		  let l_e = MaxSat.allEdges (allEdges_scc mode new_call_g vlist) idedges in
      let out_l = MaxSat.atleastOne (List.fold_left (fold_v mode) [] vlist) idedges in
      And(acc, mk_imply l_e out_l)
		) mk_true modes  


(** Fact (g) get incoming edges from SCC which has sink
    call scc -> in : call or ret
    ret scc -> in : ret
*)
let in_loop modes call_g new_call_g vlist idedges = 
  let fold_v mode acc v =
    let pred_e = CG.pred_e call_g v in
    (* Filter predecessor edges without in SCC *)
    let filter_pred_e e =
      let src = CG.E.src e in
      not (List.exists (fun v' -> CG.V.compare v' src = 0) vlist)
    in
    let pred_e = List.filter filter_pred_e pred_e in
    let succ_e = CG.succ_e call_g v in
    (* Filter successor edges without in SCC *)
    let filter_succ_e e =
      let dst = CG.E.dst e in
      not (List.exists (fun v' -> CG.V.compare v' dst = 0) vlist)
    in
    let succ_e = List.filter filter_succ_e succ_e in
    (* Get return edges *)
    let succ_e = List.map (fun (v1, _, v2) -> (v2, "R", v1)) succ_e in
    succ_e @ (if (String.compare mode "C") = 0 then pred_e else []) @ acc
  in
	List.fold_left (fun acc mode ->
		  let l_e = MaxSat.allEdges (allEdges_scc mode new_call_g vlist) idedges in
      let in_l = MaxSat.atleastOne (List.fold_left (fold_v mode) [] vlist) idedges in
      let acc = And(acc, mk_imply l_e in_l) in 
			acc 
		) mk_true modes 


(** Fact (h) call scc -> in : call or ret, out : call
    ret scc -> in : ret, out : call or ret
*)
let in_out_loop modes call_g new_call_g vlist idedges =
	List.fold_left (fun acc mode ->
		  let l_e = MaxSat.allEdges (allEdges_scc mode new_call_g vlist) idedges in
      let in_l = in_loop [mode] call_g new_call_g vlist idedges in
      let out_l = out_loop [mode] call_g new_call_g vlist idedges in
      let acc = And(acc, mk_imply l_e (And (in_l, out_l))) in 
			acc  
		) mk_true modes  

let sccEncoding_backbone call_g new_call_g src sink scc_list idedges =
  let iter acc vlist =
		let is_call_scc = is_call_scc new_call_g vlist in 
		let is_ret_scc = is_ret_scc new_call_g vlist in 
		let modes = (if is_call_scc then ["C"] else []) @ (if is_ret_scc then ["R"] else []) in 
    if has_vertex src vlist then
      (* Fact (f) *)
      let fact_f = out_loop modes call_g new_call_g vlist idedges in
      CNF.ClauseSet.union (cnf_conv fact_f) acc
    else if has_vertex sink vlist then
      (* Fact (g) *)
      let fact_g = in_loop modes call_g new_call_g vlist idedges in
      CNF.ClauseSet.union (cnf_conv fact_g) acc
    else
      (* Fact (h) *)
      let fact_h = in_out_loop modes call_g new_call_g vlist idedges in
      CNF.ClauseSet.union (cnf_conv fact_h) acc
  in
  List.fold_left iter CNF.ClauseSet.empty scc_list

let in_loop_branches call_g new_call_g vlist idedges =
  (* all the edges are call-edges. *)
  let fold_v acc v =
    let pred_e = CG.pred_e call_g v in
    (* Filter predecessor edges without in SCC *)
    let filter_pred_e e =
      let src = CG.E.src e in
      not (List.exists (fun v' -> CG.V.compare v' src = 0) vlist)
    in
    let pred_e = List.filter filter_pred_e pred_e in
    pred_e @ acc
  in
  let l_e = MaxSat.allEdges (allEdges_scc "C" new_call_g vlist) idedges in
  let in_l = MaxSat.atleastOne (List.fold_left fold_v [] vlist) idedges in
  mk_imply l_e in_l

let sccEncoding_branches call_g new_call_g src sink scc_list idedges =
  let iter acc vlist =
    let fact_g = in_loop_branches call_g new_call_g vlist idedges in
    CNF.ClauseSet.union (cnf_conv fact_g) acc
  in
  List.fold_left iter CNF.ClauseSet.empty scc_list
