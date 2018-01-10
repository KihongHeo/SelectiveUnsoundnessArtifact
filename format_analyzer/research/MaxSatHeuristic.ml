open VocabB
open PPIL
open UserInput.Input
open BFormula
open Dug
open SPath
open SCC
open CallGraph
open SanityCheck
let script = ref false
let useriter = ref 0
let scc_cons = ref CNF.ClauseSet.empty
module CGDom = Graph.Dominator.Make (CG)

let make_new_graph g result idedges =
  let collect_edges acc int_e =
    let e = get_edge_from_id idedges (string_of_int (-int_e)) in
    e :: acc
  in
  let edges = List.fold_left collect_edges [] result in
  (* remove all negative variable *)
  List.fold_left CG.remove_edge_e g edges

let remove_alone_vertex g =
  let remove_v v a =
    let preds = List.length (CG.pred a v) in
    let succs = List.length (CG.succ a v) in
    if preds = 0 && succs = 0 then
      CG.remove_vertex a v
    else
      a
  in
  CG.fold_vertex remove_v g g

let is_fixpoint hard_cons soft_cons result total_var_cnt =
  let result_true = List.filter (fun i -> i > 0) result in
  CNF.ClauseSet.fold (fun clause acc ->
      if acc then
        CNF.LiteralSet.fold (fun literal acc ->
            if acc then true
            else
              (match literal with
               | Pos True -> true
               | Pos False -> false
               | Neg True -> false
               | Neg False -> true
               | Pos (Var i) -> List.mem (int_of_string i) result_true
               | Neg (Var i) -> not (List.mem (int_of_string i) result_true)
              )
          ) clause false
      else false
    ) hard_cons true
    
let make_bb_g solve_result (call_g, call_g') (x_idedges, y_idedges) x_cnt =
  let backbone_g =
      (* filter negative variable for backbone *)
      let result_backbone =
        List.filter (fun e -> (e < 0) && ((abs e) <= x_cnt)) solve_result
      in 
      remove_alone_vertex (make_new_graph call_g' result_backbone x_idedges) 
  in
  let branches_g =
    (* filter negative variable for branches *)
    let result_branches =
      List.filter (fun e -> (e < 0) && ((abs e) > x_cnt)) solve_result
    in 
    remove_alone_vertex (make_new_graph call_g result_branches y_idedges) 
  in
  (backbone_g, branches_g)
   

let solving hard_cons soft_cons total_var_cnt  = 
  let result = MaxSat.solve_maxsat total_var_cnt hard_cons soft_cons in
  match result with
  | None ->
    let _ = MaxSat.print_total_io_time () in
    let _ = prerr_endline "UNSAT" in
    assert(false)
  | Some solve_result ->
    let _ = prerr_endline "success solving" in
    solve_result
      
let call_graph_merge
    (src, sink) (backbone_g, branches_g) (x_idedges, y_idedges) =
  prerr_endline "start call graph merging";
  let new_call_g' =
    (* combine backbone and branches *)
    CG.fold_edges_e (fun e g -> CG.add_edge_e g e) branches_g backbone_g  
  in
  let r = DFG.add_vertex (DFG.add_vertex DFG.empty sink) src in
  let _ = prerr_endline
      ("Size of Vertex : " ^ (string_of_int (CG.nb_vertex new_call_g'))) in 
  let _ = prerr_endline
      ("Size of Edges : " ^ (string_of_int (CG.nb_edges new_call_g'))) in
  let _ = MaxSat.print_total_io_time () in
  ((backbone_g, branches_g), (x_idedges, y_idedges))

let make_graph2label label g =
  let fold_edges e new_g =
    if CG.E.label e = label then
      CG.add_edge_e new_g e
    else
      new_g
  in
  CG.fold_edges_e fold_edges g CG.empty

let make_scc_friend_formula
    scc_list scc_g call_g (src, sink) (x_idedges, y_idedges) =
  let fold_sccs filt acc scc =
    let fold_friend acc node =
      let friends = filt call_g node in
      let friends =
        List.filter (fun n -> not ((*List.mem n scc || *)CG.mem_vertex scc_g n)) friends
      in
      friends @ acc			
    in
    (List.fold_left fold_friend scc scc) :: acc
  in
  let friend_list =
    let filt g v = (CG.pred g v) @ (CG.succ g v) in
    List.fold_left (fold_sccs filt) [] scc_list
  in
  (* let preds_neigbor_list =                           *)
  (*   (List.fold_left (fold_sccs CG.pred) [] scc_list) *)
  (* in                                                 *)
  (* let succs_neigbor_list =                           *)
  (*   (List.fold_left (fold_sccs CG.succ) [] scc_list) *)
  (* in                                                 *)
  let fold_friends acc nodes =
    let f =
      MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) nodes
    in
    And(f, acc)
  in
  let f = List.fold_left fold_friends mk_true friend_list in
  f
(* let succ_f = List.fold_left fold_neigbor mk_true preds_neigbor_list in *)
(* let pred_f = List.fold_left fold_neigbor mk_true succs_neigbor_list in *)
(* And(succ_f, pred_f)                                                    *)


let make_branches_neigbor_formula
    backbone_g branches_g call_g (src, sink) (x_idedges, y_idedges) =
  (* collect vertex in braches graph without backbone graph *)
  let get_brances_vertex node acc =
    if CG.mem_vertex backbone_g node then acc else node :: acc
  in
  (* Filter node which is in backbone_g *)
  let branches_nodes =
    CG.fold_vertex get_brances_vertex branches_g []
  in
  (* Get all friend nodes from branches_g *)
  let get_friend_vertex filt acc node =
    let neigbors = filt call_g node in
    let filter_neigbors n =
      not (CG.mem_vertex branches_g n || CG.mem_vertex backbone_g n)
    in
    (List.filter filter_neigbors neigbors) @ acc
  in
  let preds_friends =
    List.fold_left (get_friend_vertex CG.pred) branches_nodes branches_nodes
  in
  (* let succs_friends =                                                        *)
  (*   List.fold_left (get_friend_vertex CG.succ) branches_nodes branches_nodes *)
  (* in                                                                           *)
  let preds_f =
    MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) preds_friends
  in
  preds_f
  (* let succs_f = *)
  (*   MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) succs_friends *)
  (* in *)
  (* And(preds_f, succs_f) *)
    
let union_cc x y parts =
	let xy, parts' = List.partition (fun s -> BatSet.mem x s || BatSet.mem y s) parts in 
	match xy with 
	| [] -> (BatSet.add x (BatSet.singleton y)) :: parts'
	| s :: [] -> (BatSet.add x (BatSet.add y s)) :: parts'
	| s1 :: s2 ::[] -> (BatSet.union s1 s2) :: parts'   
	| _ -> assert false     

let get_cc g = List.map BatSet.elements (CG.fold_edges union_cc g [])    

(* test for get_cc *)
(* let _ =                                                                             *)
(* 	let a = InterNode.entryof "a" in                                                  *)
(* 	let b = InterNode.entryof "b" in                                                  *)
(* 	let c = InterNode.entryof "c" in                                                  *)
(* 	let d = InterNode.entryof "d" in                                                  *)
(* 	let e = InterNode.entryof "e" in                                                  *)
(* 	let g = CG.empty in                                                               *)
(* 	let g = CG.add_edge g a b in                                                      *)
(* 	let g = CG.add_edge g c d in                                                      *)
(* 	let g = CG.add_edge g a e in                                                      *)
(* 	List.iter (fun set ->                                                             *)
(* 		List.iter (fun v -> prerr_endline (InterNode.get_pid v)) (BatSet.elements set); *)
(* 		prerr_endline "") (get_cc g)                                                    *)
		
let graph_to_cc_formula
    backbone_g branches_g call_g (src, sink) (x_idedges, y_idedges) =
  let backbone_cg = make_graph2label "C" backbone_g in
  let backbone_rg = make_graph2label "R" backbone_g in

  let make_friend_formula g filt_cc filt_friend get_friends =
    (* let _ = prerr_endline "" in                                                                                    *)
    (* let _ = CG.iter_edges (fun v1 v2 -> prerr_endline ((InterNode.get_pid v1)^" -> "^(InterNode.get_pid v2))) g in *)
    (* let _ = prerr_endline " ================== " in                                                                *)
    let g =
      CG.fold_vertex
        (fun v g -> if not (filt_cc v) then CG.remove_vertex g v else g) g g
    in

    (* let _ = CG.iter_edges (fun v1 v2 -> prerr_endline ((InterNode.get_pid v1)^" -> "^(InterNode.get_pid v2))) g in *)
    (* let _ = prerr_endline "" in                                                                                    *)
    let ccs = get_cc g in
    (* let _ = List.iter (fun cc -> prerr_endline ("# of CC : "^(string_of_int (List.length cc)))) ccs in *)
    let fold_ccs acc cc =
      let fold_friend acc node =
        let friends = List.filter (fun v -> (filt_cc v) && (filt_friend v)) (get_friends node) in
  	friends @ acc			
      in
      (List.fold_left fold_friend cc cc) :: acc
    in
    let friend_list = List.fold_left fold_ccs [] ccs in

    (* let _ = List.iter (fun friend -> prerr_endline ("# of NB : "^(string_of_int (List.length friend)))) friend_list in *)

    let fold_friend acc nodes =
      let f =
        MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) nodes
      in
      And(f, acc)
    in
    List.fold_left fold_friend mk_true friend_list
  in

  (* let backbone_cg_f =                                                        *)
  (* 	make_friend_formula backbone_cg                                          *)
  (* 		(fun v -> not ((CG.V.compare v src) = 0 || (CG.V.compare v sink) = 0)) *)
  (* 		(fun v -> not (CG.mem_vertex backbone_g v))                            *)
  (* 		(fun v -> (CG.pred call_g v) @ (CG.succ call_g v))                     *)
  (* in                                                                         *)
  (* let backbone_rg_f =                                                        *)
  (* 	make_friend_formula backbone_rg                                          *)
  (* 		(fun v -> not ((CG.V.compare v src) = 0 || (CG.V.compare v sink) = 0)) *)
  (* 		(fun v -> not (CG.mem_vertex backbone_g v))                            *)
  (* 		(fun v -> (CG.pred call_g v) @ (CG.succ call_g v))                     *)
  (* in                                                                         *)
  let branch_f = 
    make_friend_formula branches_g
      (fun v -> not (CG.mem_vertex backbone_g v))
      (fun v -> not (CG.mem_vertex branches_g v))
      (fun v -> CG.pred call_g v) 
  in
  (* And (backbone_cg_f, And(backbone_rg_f, branch_f)) *)
  branch_f


let graph_to_scc_formula
    backbone_g branches_g call_g (src, sink) (x_idedges, y_idedges) =
  let scc_cg = make_graph2label "C" backbone_g in
  let scc_rg = make_graph2label "R" backbone_g in
  let filter_scc scc = List.length scc != 1 in
  let cg_scc_list = List.filter filter_scc (SCC.scc_list scc_cg) in
  let rg_scc_list = List.filter filter_scc (SCC.scc_list scc_rg) in
  
  if !Options.opt_encoding_mode = 3 then
    let cg_neigbor_formula =
      make_scc_friend_formula
        cg_scc_list scc_cg call_g (src, sink) (x_idedges, y_idedges)
    in
    let rg_neigbor_formula =
      make_scc_friend_formula
        rg_scc_list scc_rg call_g (src, sink) (x_idedges, y_idedges)
    in
    let bg_neigbor_formula =
      make_branches_neigbor_formula
        backbone_g branches_g call_g (src, sink) (x_idedges, y_idedges)
    in
    let neigbor_formula = And(cg_neigbor_formula, rg_neigbor_formula) in
    And (bg_neigbor_formula, neigbor_formula)
  else (* TODO : Test *)
    let fold_sccs acc scc =
      let f =
        MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) scc
      in
      And(f, acc)
    in
    let bg_scc_list = List.filter filter_scc (SCC.scc_list branches_g) in
    
    let cg_scc_formula = List.fold_left fold_sccs mk_true cg_scc_list in
    let rg_scc_formula = List.fold_left fold_sccs mk_true rg_scc_list in
    let bg_scc_formula = List.fold_left fold_sccs mk_true bg_scc_list in
    let scc_formula = And(cg_scc_formula, rg_scc_formula) in
    And (bg_scc_formula, scc_formula)
	
  (* let make_scc2f acc scc = *)
  (*   let f = *)
  (*     MaxSat.make_formula call_g (src, sink) (x_idedges, y_idedges) scc *)
  (*   in *)
  (*   And(f, acc) *)
  (* in *)
  (* let scc_formular = *)
  (*   And(List.fold_left make_scc2f mk_true cg_scc_list, *)
  (*       List.fold_left make_scc2f mk_true rg_scc_list) *)
  (* in *)


let process_scc
    icfg (call_g, call_g') (src, sink) (hard_cons, soft_cons)
    (x_idedges, y_idedges) (backbone_g, branches_g) result total_var_cnt x_id_cnt
  =
  let rec process_scc' count (backbone_g, branches_g) hard_cons result =
    let _ = 
    if !script then
      AlarmVis.make_call_g_dot ((string_of_int !useriter)^"_backbone"^(string_of_int count)) icfg
      (src, sink, (backbone_g, branches_g)) (x_idedges, y_idedges)
    else
    AlarmVis.make_call_g_dot ("backbone"^(string_of_int count)) icfg
      (src, sink, (backbone_g, branches_g)) (x_idedges, y_idedges)
    in
    (* Get scc formula (backbone and branches) *)
    let escape_f_scc =
      graph_to_scc_formula backbone_g branches_g call_g (src, sink)
        (x_idedges, y_idedges)
    in
    (* let escape_f_cc = *)
    (*   graph_to_cc_formula backbone_g branches_g call_g (src, sink) *)
    (*     (x_idedges, y_idedges) *)
    (* in     *)
    (* Make scc cnf *)
    let scc_cnf = cnf_conv escape_f_scc in
    let hard_cons' = CNF.ClauseSet.union hard_cons scc_cnf in
    let is_fix = is_fixpoint hard_cons' soft_cons result total_var_cnt in
    if is_fix then
    (
      let is_backbone_ok =
        SanityCheck.sanity_check_backbone backbone_g src sink
      in
      if is_backbone_ok then
        (counter := 1;
         call_graph_merge
           (src, sink) (backbone_g, branches_g) (x_idedges, y_idedges))
      else
	(
          let neg_path = MaxSat.neg_current_path call_g' backbone_g x_idedges in
          let hard_cons'' = CNF.ClauseSet.union hard_cons' neg_path in
          let result = solving hard_cons'' soft_cons total_var_cnt in
          let (backbone_g, branches_g) =
            make_bb_g result (call_g, call_g') (x_idedges, y_idedges) x_id_cnt
          in
          (* AlarmVis.make_call_g_dot ("backbone"^(string_of_int !counter)) icfg *)
          (*   (src, sink, (backbone_g, branches_g)) (x_idedges, y_idedges);     *)
          process_scc' (count+1) (backbone_g, branches_g) hard_cons'' result
	)
    )
    else
      (
        let _ = counter := !counter + 1; prerr_endline (string_of_int !counter) in
        let result = solving hard_cons' soft_cons total_var_cnt in
        let (backbone_g, branches_g) =
          make_bb_g result (call_g, call_g') (x_idedges, y_idedges) x_id_cnt
        in
        (* AlarmVis.make_call_g_dot ("backbone"^(string_of_int !counter)) icfg *)
        (*   (src, sink, (backbone_g, branches_g)) (x_idedges, y_idedges);     *)
        prerr_endline (string_of_float !filter_time);
        (* prerr_endline "enter"; *)
        (* let _ = read_line () in *)
        process_scc' (count+1) (backbone_g, branches_g) hard_cons' result
      )
  in
  process_scc' 1 (backbone_g, branches_g) hard_cons result


(* call_g has only call_edge, call_g' has call_edges and ret_edges *)
let get_path icfg call_g call_g' src sink =
  (* get call, ret edges numbers *)
  let x_nb_edges = (CG.nb_edges call_g') in
  let y_nb_edges = (CG.nb_edges call_g) in
  let total_var_cnt = x_nb_edges + y_nb_edges in
  
  let x_idedges = create_idedges call_g' 1 in
  let y_idedges = create_idedges call_g (x_nb_edges + 1) in
  
  let hard_cons =
    MaxSat.make_hard_constraint call_g (src, sink) (x_idedges, y_idedges)
  in
  let soft_cons =
    MaxSat.make_soft_constraint total_var_cnt
  in
  (* solve_maxsat : varcnt -> cons -> cons -> Some (id list)  *)
  let result = solving hard_cons soft_cons total_var_cnt in
  let (backbone_g, branches_g) =
    make_bb_g result (call_g, call_g') (x_idedges, y_idedges) x_nb_edges
  in
  process_scc
    icfg (call_g, call_g') (src, sink) (hard_cons, soft_cons)
    (x_idedges, y_idedges) (backbone_g, branches_g) result total_var_cnt x_nb_edges
    
(** Only for script mode *)
let get_path' icfg call_g call_g' src sink iter =
  (* get call, ret edges numbers *)
  let x_nb_edges = (CG.nb_edges call_g') in
  let y_nb_edges = (CG.nb_edges call_g) in
  let total_var_cnt = x_nb_edges + y_nb_edges in
  
  let x_idedges = create_idedges call_g' 1 in
  let y_idedges = create_idedges call_g (x_nb_edges + 1) in
  
  let hard_cons =
    MaxSat.make_hard_constraint' call_g (src, sink) (x_idedges, y_idedges) iter
  in
  let soft_cons = MaxSat.make_soft_constraint total_var_cnt in
  (* solve_maxsat : varcnt -> cons -> cons -> Some (id list)  *)
  let result = solving hard_cons soft_cons total_var_cnt in
  let (backbone_g, branches_g) =
    make_bb_g result (call_g, call_g') (x_idedges, y_idedges) x_nb_edges
  in
  process_scc
    icfg (call_g, call_g') (src, sink) (hard_cons, soft_cons)
    (x_idedges, y_idedges) (backbone_g, branches_g) result total_var_cnt x_nb_edges
