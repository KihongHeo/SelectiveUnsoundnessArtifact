open VocabB
open BFormula
open Dug
open CallGraph
open SPath
open UserInputType
open UserInput.Input
module SCC = Components.Make (CG)

let io_time = ref 0.0
let reset_io_time () = io_time := 0.0
let timer_start () = Unix.gettimeofday ()
let add_io_time t =
  let t' = Unix.gettimeofday () in
  io_time := !io_time +. (t' -. t)
let print_total_io_time () =
  prerr_endline ("Total IO time is " ^ (string_of_float !io_time))
let print_solving_time t =
  let t' = Unix.gettimeofday () in
  prerr_endline ("Solving time is " ^ (string_of_float (t'-.t)))
let print_io_time t =
  let t' = Unix.gettimeofday () in
  prerr_endline ("IO time is " ^ (string_of_float (t'-.t)))
    
let maxsat_solver_path () = !Options.opt_solver
let temp_wcnf_file = "tmp.wcnf"
let temp_result_file = "out"

let readMaxSatResult filename =
  let chan = open_in filename in
  let in_chan = Extlib.IO.input_channel chan in
  let str = Extlib.IO.read_all in_chan in
  let pos =
    try Str.search_forward (Str.regexp "s OPTIMUM FOUND\\\nv ") str 0 with
    | Not_found -> -1
  in
  if pos = -1 then
    let unsat_pos =
      try Str.search_forward (Str.regexp "s UNSATISFIABLE") str 0 with
      | Not_found -> -1
    in
    if unsat_pos = -1 then (prerr_endline "System Error!"; None) else
    (prerr_endline "UNSAT"; None)
  else
    let str =
      Str.string_after str (pos + (String.length "s OPTIMUM FOUND\nv "))
    in
    let str = Str.global_replace (Str.regexp " \\\n") "" str in
    let strList = Str.split (Str.regexp " ") str in
    close_in chan;
    Some (List.fold_left (fun a e -> (int_of_string e :: a)) [] strList)

let solve_maxsat var_cnt hard_cons soft_cons =
  let t = timer_start () in
  CNF.p_wcnf var_cnt hard_cons soft_cons temp_wcnf_file;
  add_io_time t;
  print_io_time t;
  let t_solving = timer_start () in
  let _ = Sys.command ((maxsat_solver_path  ())^ " " ^ temp_wcnf_file ^
                       "  > " ^ temp_result_file ) in
  print_solving_time t_solving;
  readMaxSatResult temp_result_file

let neg_current_path call_g' backbone_g idedges' =
  let fold_bb_edges e acc =
    let lit = mk_var (string_of_int (get_edge_id idedges' e)) in
    And(lit, acc)
  in
  let t_e_formula = CG.fold_edges_e fold_bb_edges backbone_g mk_true in
  let fold_c_edges e acc =
    (* Neg edge in call_g' without backbone *)
    if CG.mem_edge_e backbone_g e then acc else
      let lit = Neg(mk_var (string_of_int (get_edge_id idedges' e))) in
      And(lit, acc)
  in
  let f_e_formula = CG.fold_edges_e fold_c_edges call_g' t_e_formula in
  cnf_conv (Neg (f_e_formula))

let allEdges edges idedges =
  let edge_fold acc edge =
    let id = get_edge_id idedges edge in
    let p_e = mk_var (string_of_int id) in
    And (p_e, acc)
  in
  List.fold_left edge_fold mk_true edges

let atleastOne edges idedges =
  let edge_fold acc edge =
    let id = get_edge_id idedges edge in
    let p_e = mk_var (string_of_int id) in
    Or (p_e, acc)
  in
  List.fold_left edge_fold mk_false edges

let make_soft_constraint total_var_cnt =
  (* 1 ~ nb_edges' : x_ij, nb_edges' + 1 ~ nb_edges' + nb_edges' : y_ij *)
  let rec build_soft_constr acc id nb =
    if (id > nb) then acc 
    else 
      let p_e = mk_var (string_of_int id) in
      let not_p_e = Neg p_e in
      build_soft_constr (And (not_p_e, acc)) (id + 1) nb
  in
  cnf_conv (build_soft_constr mk_true 1 total_var_cnt)

let ret_edge2call_edge (v2, lbl, v1) =
  if lbl = "R" then (v1, "C", v2) else
    ( (* Wrong Parameter *)
      prerr_endline "ret_edge2call_edge assertion";
      assert(false)
    )
let call_edge2ret_edge (v1, lbl, v2) =
  if lbl = "C" then (v2, "R", v1) else
    ( (* Wrong Parameter *)
      prerr_endline "call_edge2ret_edge assertion";
      assert(false)
    )

let call_edges2ret_edges edges =
  List.map call_edge2ret_edge edges

let ret_edges2call_edges edges =
  List.map ret_edge2call_edge edges

(** in(n) = {e_c | e \in pred(n)} U {e_r | e \in succ(n)} *)
let in_e call_g node idedges =
  let pred_e = CG.pred_e call_g node in
  let succ_e = CG.succ_e call_g node in
  let succ_e = call_edges2ret_edges succ_e in
  pred_e @ succ_e

let out_e call_g node idedges = 
  let succ_e = CG.succ_e call_g node in
  let pred_e = CG.pred_e call_g node in
  let pred_e = call_edges2ret_edges pred_e in
  pred_e @ succ_e 

let get_outedges_nodes call_g nodes x_idedges is_external = 
  let out_edges =
    List.fold_left (fun lst v -> lst @ (out_e call_g v x_idedges)) [] nodes
  in
  if not is_external then out_edges
  else List.filter (fun (v1, _, v2) -> not (List.mem v2 nodes)) out_edges

let get_inedges_nodes call_g nodes x_idedges is_external = 
  let in_edges =
    List.fold_left (fun lst v -> lst @ (in_e call_g v x_idedges)) [] nodes
  in
  if not is_external then in_edges
  else List.filter (fun (v1, _, v2) -> not (List.mem v1 nodes)) in_edges
    
let get_succedges_nodes call_g nodes is_external = 
  let out_edges =
    List.fold_left (fun lst v -> lst @ (CG.succ_e call_g v)) [] nodes
  in
  if not is_external then out_edges
  else List.filter (fun (v1, _, v2) -> not (List.mem v2 nodes)) out_edges

let get_prededges_nodes call_g nodes is_external = 
  let in_edges =
    List.fold_left (fun lst v -> lst @ (CG.pred_e call_g v)) [] nodes
  in
  if not is_external then in_edges
  else List.filter (fun (v1, _, v2) -> not (List.mem v1 nodes)) in_edges
      
let get_basic_formula call_g (x_idedges, y_idedges) nodes =
  let inC = get_prededges_nodes call_g nodes false in
  let outC = get_succedges_nodes call_g nodes false in
  let inR = call_edges2ret_edges outC in
  let outR = call_edges2ret_edges inC in
  
  let inEC = get_prededges_nodes call_g nodes true in
  let outEC = get_succedges_nodes call_g nodes true in
  let inER = call_edges2ret_edges outEC in
  let outER = call_edges2ret_edges inEC in

  let outYC = get_succedges_nodes call_g nodes false in
  let inEYC = get_prededges_nodes call_g nodes true in
  let f edges = atleastOne edges x_idedges in
  let f' edges = atleastOne edges y_idedges in
  (f inC, f outC, f inR, f outR, f inEC, f outEC, f inER, f outER,
   f' outYC, f' inEYC)

let make_inNout_formula (src, sink) basic_f nodes =
  let is_sink = List.exists (fun n -> CG.V.compare n sink = 0) nodes in
  if is_sink then mk_true
  else
    let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
    let f1 = mk_imply inC outEC in
    let f2 = mk_imply inR (Or(outEC, outER)) in
    And(f1, f2)

let make_outPin_formula (src, sink) basic_f nodes =
  let is_src = List.exists (fun n -> CG.V.compare n src = 0) nodes in
  if is_src then mk_true
  else
    let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
    let f1 = mk_imply outC (Or(inEC, inER)) in
    let f2 = mk_imply outR inER in
    And(f1, f2)

let make_inPout_formula (src, sink) basic_f nodes =
  let is_src = List.exists (fun n -> CG.V.compare n src = 0) nodes in
  if is_src then
    let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
    let f1 = mk_imply inEC (Or(outEC, outER)) in
    let f2 = mk_imply inER outER in
    And(f1, f2)
  else mk_true
    
let make_outNin_formula (src, sink) basic_f nodes =
  let is_sink = List.exists (fun n -> CG.V.compare n sink = 0) nodes in
  if is_sink then
    let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
    let f1 = mk_imply outEC inEC in
    let f2 = mk_imply outER (Or(inEC, inER)) in
    And(f1, f2)
  else mk_true
    

let make_init_formula (src, sink) basic_f nodes =
  let is_exist node = List.exists (fun n -> CG.V.compare n node = 0) nodes in
  let is_src = is_exist src in
  let is_sink = is_exist sink in
  let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
  if is_src && not is_sink then
    Or(outEC, outER)
  else if not is_src && is_sink then
    Or(inEC, inER)
  else
    mk_true

let make_backbone_formula (src, sink) basic_f nodes =
  let f1 = make_inNout_formula (src, sink) basic_f nodes in
  let f2 = make_outPin_formula (src, sink) basic_f nodes in
  (* let f3 = make_inPout_formula (src, sink) basic_f nodes in *)
  (* let f4 = make_outNin_formula (src, sink) basic_f nodes in *)
  let f5 = make_init_formula (src, sink) basic_f nodes in
  And(f1, And(f2, (* And(f3, And(f4, *) f5))

let make_branch_formula (src, sink) basic_f nodes =
  let is_exist node = List.exists (fun n -> CG.V.compare n node = 0) nodes in
  let is_src = is_exist src in
  let is_sink = is_exist sink in
  let (inC, outC, inR, outR, inEC, outEC, inER, outER, outYC, inEYC) = basic_f in
  if not is_src && not is_sink then
    (* TODO : Make Branch formula *)
    let f1 = (Or(inEYC, (Or(inC, Or(inR, Or(outC, outR)))))) in
    mk_imply outYC f1
  else mk_true

let make_formula call_g (src, sink) (x_idedges, y_idedges) nodes =
  let basic_f = get_basic_formula call_g (x_idedges, y_idedges) nodes in
  
  (* let init_f = make_init_formula (src, sink) basic_f nodes in *)
  let backbone_f = make_backbone_formula (src, sink) basic_f nodes in
  let branch_f = make_branch_formula (src, sink) basic_f nodes in
  And(backbone_f, branch_f)

let print_sccs name scc_list =
  prerr_endline ("======" ^  name ^ " ====");
  List.iter (fun l ->
      (* prerr_endline "============"; *)
      prerr_endline (string_of_int (List.length l));
      (* prerr_endline "============"; *)
      (* List.iter (fun v -> print_string (" " ^ (InterNode.get_pid v))) l; *)
      (* print_endline ""; *)
      (* print_endline ""  *)
    ) scc_list;
  prerr_endline "============"
 


let make_scc_formula call_g (src, sink) (x_idedges, y_idedges) user_f =
  let cnf_user_feed = cnf_conv user_f in
  (* leave only negated ones *)	
  let facts =
    let filter_neg fact =
      match fact with
      | CNF.Neg _ -> true
      | _ -> false
    in
    List.filter filter_neg (ground_facts cnf_user_feed)
  in
   let backbone_scc_list =
    let fold_facts (cg, rg) fact =
      match fact with
      | CNF.Neg (Var id) ->
        (try
          let e = get_edge_from_id x_idedges id in
	  let lbl = CG.E.label e in 
	  if (String.compare lbl "C") = 0 then (CG.remove_edge_e cg e, rg)
          else (cg, CG.remove_edge_e rg (ret_edge2call_edge e))
        with _ -> (cg, rg))
      | _ -> (cg, rg)
    in
    let (cg, rg) = List.fold_left fold_facts (call_g, call_g) facts in
    let cg_scc_list =
      List.filter (fun l -> List.length l != 1) (SCC.scc_list cg)
    in
    let rg_scc_list =
      List.filter (fun l -> List.length l != 1) (SCC.scc_list rg)
    in
    print_sccs "Call X" cg_scc_list;
    print_sccs "Ret X" rg_scc_list;
    cg_scc_list @ rg_scc_list
  in
  let branch_scc_list =
    let fold_facts cg fact =
      match fact with
      | CNF.Neg (Var id) ->
        (try
          let e = get_edge_from_id y_idedges id in
	  CG.remove_edge_e cg e
        with _ -> cg)
      | _ -> cg
    in
    let cg = List.fold_left fold_facts call_g facts in
    let cg_scc_list =
      List.filter (fun l -> List.length l != 1) (SCC.scc_list cg)
    in
    print_sccs "Call Y" cg_scc_list;
    cg_scc_list
  in
  let scc_list = backbone_scc_list @ branch_scc_list in
  let scc_f =
    let fold_scc acc scc =
      And(make_formula call_g (src, sink) (x_idedges, y_idedges) scc, acc)
    in
    List.fold_left fold_scc mk_true scc_list
  in
  (* let scc_f = *)
  (*   let rec make_formula_rec scc = *)
  (*     let length = List.length scc in *)
  (*     let rec split_scc n scc1 scc2 = *)
  (*       if n > 0 then *)
  (*         split_scc (n-1) ((List.hd scc2) :: scc1) (List.tl scc2) *)
  (*       else *)
  (*         (scc1, scc2) *)
  (*     in *)
  (*     if length < 8 then *)
  (*       mk_true *)
  (*     else *)
  (*       let (scc1, scc2) = split_scc (length / 2) [] scc in *)
  (*       And(make_formula call_g (src, sink) (x_idedges, y_idedges) scc, *)
  (*           And(make_formula_rec scc1, make_formula_rec scc2)) *)
  (*   in *)
  (*   let fold_scc_list acc scc = *)
  (*     And(make_formula_rec scc, acc) *)
  (*   in *)
  (*   List.fold_left fold_scc_list mk_true scc_list *)
  (* in *)
  scc_f

let make_user_formula call_g (src, sink) (x_idedges, y_idedges) =
  (* for user feedbacks. use idedges for both call and return edges. *)
  (* Encoding_Mode=1 do not use make_user_formula *)
  if !Options.opt_encoding_mode = 1 then mk_true else
  let (call, ret) = ("C", "R") in
  (* return : idedges', call : idedges *)
  (* User input substitution *)
  let get_edge_id mode f g =
    let f = InterNode.entryof f in
    let g = InterNode.entryof g in
    if (String.compare mode call) = 0 then
      
      let x_c = mk_var (string_of_int (get_edge_id x_idedges (f, call, g)))
      in
      let y_c =
        mk_var (string_of_int (get_edge_id y_idedges (f, call, g)))
      in
      Or(x_c, y_c)
    else
      let x_r =
        mk_var (string_of_int (get_edge_id x_idedges (f, ret, g)))
      in
      let y_r =
        mk_var (string_of_int (get_edge_id y_idedges  (g, call, f)))
      in
      Or(x_r, y_r)
  in
  let get_call_preds n =
    let preds = CG.pred call_g n in
    let fold_preds acc pred =
      Or(get_edge_id call (InterNode.get_pid pred) (InterNode.get_pid n), acc)
    in
    List.fold_left fold_preds mk_false preds 
  in
  (** ---- user feedback ---- *)
  
  let user_f = 
    let iter = mk_true in
    iter
  in

  let scc_f =
    make_scc_formula call_g (src, sink) (x_idedges, y_idedges) user_f
  in
  And(user_f, scc_f)

(* let make_x_y_disjoint_formula call_g (x_idedges, y_idedges) = *)
(*   let fold_edges e acc = *)
(*     let (v1, lbl, v2) = e in *)
(*     if lbl = "C" then *)
(*       let x_c = mk_var (get_edge_id x_idedges e) in *)
(*       let y_c = mk_var (get_edge_id y_idedges e) in *)
(*       let f = Or(Neg x_c, Neg y_c) in *)
(*       And(f, acc) *)
(*     else *)
(*       let x_r = mk_var (get_edge_id x_idedges e) in *)
(*       let y_r = mk_var (get_edge_id y_idedges (v2, "C", v1)) in *)
(*       let f = Or(Neg x_r, Neg y_r) in *)
(*       And(f, acc) *)
(*   in *)
(*   CG.fold_edges_e fold_edges call_g mk_true  *)

let make_hard_constraint call_g (src, sink) (x_idedges, y_idedges) =
  let fold_nodes node acc =
    And(make_formula call_g (src, sink) (x_idedges, y_idedges) [node], acc)
  in
  (* Disjoint formula may not sound *)
  (* let disjoint_f = make_x_y_disjoint_formula call_g (x_idedges, y_idedges) in *)
  let call_g_f = CG.fold_vertex fold_nodes call_g mk_true in
  let user_f = make_user_formula call_g (src, sink) (x_idedges, y_idedges) in
  
  let f_list = [call_g_f; user_f; (* disjoint_f *)] in
  let cnf_list = List.map (fun f -> cnf_conv f) f_list in
  prerr_endline "success cnf_conv";
  let union_from_list acc ele =
    CNF.ClauseSet.union ele acc
  in
  List.fold_left union_from_list CNF.ClauseSet.empty cnf_list


let make_user_formula' call_g (src, sink) (x_idedges, y_idedges) iter =
  (* for user feedbacks. use idedges for both call and return edges. *)
  (* Encoding_Mode=1 do not use make_user_formula *)
  if !Options.opt_encoding_mode = 1 then iter else
  let (call, ret) = ("C", "R") in
  (* return : idedges', call : idedges *)
  (* User input substitution *)
  let get_edge_id mode f g =
    let f = InterNode.entryof f in
    let g = InterNode.entryof g in
    if (String.compare mode call) = 0 then
      
      let x_c = mk_var (string_of_int (get_edge_id x_idedges (f, call, g)))
      in
      let y_c =
        mk_var (string_of_int (get_edge_id y_idedges (f, call, g)))
      in
      Or(x_c, y_c)
    else
      let x_r =
        mk_var (string_of_int (get_edge_id x_idedges (f, ret, g)))
      in
      let y_r =
        mk_var (string_of_int (get_edge_id y_idedges  (g, call, f)))
      in
      Or(x_r, y_r)
  in
  let get_call_preds n =
    let preds = CG.pred call_g n in
    let fold_preds acc pred =
      Or(get_edge_id call (InterNode.get_pid pred) (InterNode.get_pid n), acc)
    in
    List.fold_left fold_preds mk_false preds 
  in
  (** ---- user feedback ---- *)
  
  let user_f = iter in
  let scc_f =
    make_scc_formula call_g (src, sink) (x_idedges, y_idedges) user_f
  in
  And(user_f, scc_f)


(** Only for script mode *)
let make_hard_constraint' call_g (src, sink) (x_idedges, y_idedges) iter =
  let fold_nodes node acc =
    And(make_formula call_g (src, sink) (x_idedges, y_idedges) [node], acc)
  in
  (* Disjoint formula may not sound *)
  (* let disjoint_f = make_x_y_disjoint_formula call_g (x_idedges, y_idedges) in *)
  let call_g_f = CG.fold_vertex fold_nodes call_g mk_true in
  let user_f =
    make_user_formula' call_g (src, sink) (x_idedges, y_idedges) iter
  in
  
  let f_list = [call_g_f; user_f; (* disjoint_f *)] in
  let cnf_list = List.map (fun f -> cnf_conv f) f_list in
  prerr_endline "success cnf_conv";
  let union_from_list acc ele =
    CNF.ClauseSet.union ele acc
  in
  List.fold_left union_from_list CNF.ClauseSet.empty cnf_list
