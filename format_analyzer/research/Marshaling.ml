open CallGraph
open BFormula
open MaxSat

let alarm_counter = ref 1
let marshal_dir = "marshal_alarms"
let timer_start () = Unix.gettimeofday ()
let print_time t =
  let t' =  Unix.gettimeofday () in
  prerr_endline ("Total Time is " ^ (string_of_float (t' -. t)))

let start_solving icfg call_g call_g' src sink filename iter =
	(* let _ = read_line () in  *)
  let _ = prerr_endline "=====================" in
  let _ =
    prerr_endline ("Iteration : " ^ (string_of_int !alarm_counter))
  in

  let t = timer_start () in
  let ((backbone, branch), (x_idedges, y_idedges)) =
    (** Only for script mode *)
    MaxSatHeuristic.script := true;
    MaxSatHeuristic.useriter := !alarm_counter;
    MaxSatHeuristic.get_path' icfg call_g call_g' src sink iter
  in
  print_time t;
  AlarmVis.make_call_g_dot filename icfg
    (src, sink, (backbone, branch)) (x_idedges, y_idedges);
  alarm_counter := !alarm_counter + 1;
  let _ = prerr_endline "=====================" in
  (x_idedges, y_idedges)

let print_scc_decompose call_g src sink =
  let scc_list = SCC.scc_list call_g in
  let scc_list = List.filter (fun l -> List.length l > 1) scc_list in
  let scc_list_length = List.length scc_list in
  let flat_scc = List.flatten scc_list in
  let flat_scc_length = List.length flat_scc in
  let nb_v = CG.nb_vertex call_g in
  List.iter (fun l ->
      List.iter (fun v -> print_string (" " ^ (InterNode.get_pid v))) l;
      print_endline "";
      print_endline ""	 
    ) scc_list; 
  prerr_endline ("Number of SCC :" ^ (string_of_int scc_list_length));
  prerr_endline ("Number of SCC vertex : " ^ (string_of_int flat_scc_length));
  prerr_endline ("Before SCC Decompose : " ^ (string_of_int nb_v));
  prerr_endline
    ("After SCC Decompose : " ^
     (string_of_int (nb_v - flat_scc_length + scc_list_length)));
  let src_scc = List.mem src flat_scc in
  let sink_scc = List.mem sink flat_scc in
  if src_scc && sink_scc then
    prerr_endline "@@@@Src Sink in SCC"
  else if src_scc then prerr_endline "@@@@@Src in SCC"
  else if sink_scc then prerr_endline "@@@@@Sink in SCC"
  else ()


let marshal_in filename =
  if filename = "analysis" then
    let chan = open_in_bin filename in
    let file_pos = Str.search_forward (Str.regexp "marshal_alarms") filename 0 in
    prerr_endline (string_of_int file_pos);
    let filename = Str.string_after filename (file_pos+1+14) in
    let (glob_g, pre, res) = Marshal.from_channel chan in
    close_in chan;
    RunAlarmHunt.run glob_g pre res
  else
    let chan = open_in_bin filename in
    let file_pos = Str.search_forward (Str.regexp "marshal_alarms") filename 0 in
    let filename = Str.string_after filename (file_pos+1+14) in
    let (src, sink, icfg, spg, prop_info, (call_g, call_g')) =
      Marshal.from_channel chan
    in
    close_in chan;
    print_scc_decompose call_g src sink;
    prerr_endline "Do MaxSAT!";

    (* Function for iteration *)
    let (call, ret) = ("C", "R") in
    (* return : idedges', call : idedges *)
    (* User input substitution *)
    let get_edge_id_raw = get_edge_id in
    let get_edge_id mode f g (x_idedges, y_idedges) =
      let f = InterNode.entryof f in
      let g = InterNode.entryof g in
      if (String.compare mode call) = 0 then
        let x_c =
          mk_var (string_of_int
                    (get_edge_id x_idedges (f, call, g)))
        in
        let y_c =
          mk_var (string_of_int
                    (get_edge_id y_idedges (f, call, g)))
        in
        Or(x_c, y_c)
      else
        let x_r =
          mk_var (string_of_int
                    (get_edge_id x_idedges (f, ret, g)))
        in
        let y_r =
          mk_var (string_of_int
                    (get_edge_id y_idedges  (g, call, f)))
        in
        Or(x_r, y_r)
    in
    let atleastOne edges xy_idedges =
      let fold_formula acc (v1, lbl, v2) =
        let n1 = (InterNode.get_pid v1) in
        let n2 = (InterNode.get_pid v2) in
        Or(get_edge_id lbl n1 n2 xy_idedges, acc)
      in
      List.fold_left fold_formula mk_false edges
    in
    let get_call_preds n (x_idedges, y_idedges) =
      let preds = CG.pred call_g n in
      let fold_preds acc pred =
        let f1 = get_edge_id call (InterNode.get_pid pred) (InterNode.get_pid n)
            (x_idedges, y_idedges) in
        Or(f1, acc)
      in
      List.fold_left fold_preds mk_false preds 
    in
    let get_call_preds_filt n (x_idedges, y_idedges) filt =
      let preds = CG.pred call_g n in
      let fold_preds acc pred =
        if filt ((InterNode.get_pid pred),(InterNode.get_pid n)) then
          let f1 = get_edge_id call (InterNode.get_pid pred) (InterNode.get_pid n)
              (x_idedges, y_idedges) in
          Or(f1, acc)
        else
          acc
      in
      List.fold_left fold_preds mk_false preds 
    in
    let get_call_succs n (x_idedges, y_idedges) =
      let preds = CG.succ call_g n in
      let fold_preds acc succ =
        let f1 = get_edge_id call (InterNode.get_pid n) (InterNode.get_pid succ)
            (x_idedges, y_idedges) in
        Or(f1, acc)
      in
      List.fold_left fold_preds mk_false preds 
    in
    let get_ret_succs n (x_idedges, y_idedges) =
      let preds = CG.pred call_g n in
      let fold_preds acc pred =
        let f1 = get_edge_id ret (InterNode.get_pid n) (InterNode.get_pid pred)
            (x_idedges, y_idedges) in
        Or(f1, acc)
      in
      List.fold_left fold_preds mk_false preds
    in
    let get_ret_preds n (x_idedges, y_idedges) =
      let succs = CG.succ call_g n in
      let fold_succs acc succ =
        let f1 = get_edge_id ret (InterNode.get_pid succ) (InterNode.get_pid n)
            (x_idedges, y_idedges) in
        Or(f1, acc)
      in
      List.fold_left fold_succs mk_false succs
    in
    let in_e n =
      let preds = CG.pred_e call_g n in
      let succs =
        List.map (fun (v1, _, v2) -> (v2, ret, v1)) (CG.succ_e call_g n)
      in
      preds @ succs
    in
    let out_e n =
      let succs = CG.succ_e call_g n in
      let preds =
        List.map (fun (v1, _, v2) -> (v2, ret, v1)) (CG.pred_e call_g n)
      in
      preds @ succs
    in
		(** texinfo-4.13a.dfsg.1/ginstall-info *)
   (* if String.compare "alarm1_1" filename = 0 then                                           *)
   (*    let _ = prerr_endline "alarm1_1" in                                                   *)
   (*    let iter = mk_true in                                                                 *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
      
   (*    let iter =                                                                            *)
   (*      And(iter, get_edge_id call "fatal" "error" xy_idedges)                              *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)

   (*    let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "main" "fatal" xy_idedges))                          *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
			
		(*	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "open_possibly_compressed_file" "fatal" xy_idedges)) *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
			
		(*	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "pfatal_with_name" "fatal" xy_idedges))              *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
	
		(*	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "parse_input" "fatal" xy_idedges))                   *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
      
   (*    exit 0;                                                                               *)
   (*  else if String.compare "alarm1_2" filename = 0 then                                     *)
   (*    let _ = prerr_endline "alarm1_2" in                                                   *)
   (*    let iter = mk_true in                                                                 *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
   (*    let iter =                                                                            *)
   (*      And(iter, get_edge_id call "fatal" "error" xy_idedges)                              *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
   (*    let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "main" "fatal" xy_idedges))                          *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
   (*  	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "parse_input" "fatal" xy_idedges))                   *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
		(*	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "pfatal_with_name" "fatal" xy_idedges))              *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)
   (*  	let iter =                                                                            *)
   (*      And(iter, Neg(get_edge_id call "open_possibly_compressed_file" "fatal" xy_idedges)) *)
   (*    in                                                                                    *)
   (*    let xy_idedges =                                                                      *)
   (*      start_solving icfg call_g call_g' src sink filename iter                            *)
   (*    in                                                                                    *)

   (*    exit 0;                                                                               *)
	
	(** vtprint-2.0.2-vtprint*)
    (* if String.compare "alarm1_1" filename = 0 then                     *)
    (*   let _ = prerr_endline "alarm1_1" in                              *)
    (*   let iter = mk_true in                                            *)
    (*   let xy_idedges =                                                 *)
    (*     start_solving icfg call_g call_g' src sink filename iter       *)
    (*   in                                                               *)
		(* 	let iter =                                                       *)
    (*     And(iter, Neg(get_edge_id ret "vtprintcap" "main" xy_idedges)) *)
    (*   in                                                               *)
    (*   let xy_idedges =                                                 *)
    (*     start_solving icfg call_g call_g' src sink filename iter       *)
    (*   in                                                               *)
      
      
    (*   exit 0;                                                          *)
		
		(** blktrace-1.0.5 *)
		(* if String.compare "alarm1_1" filename = 0 then                                  *)
    (*   let _ = prerr_endline "alarm1_1" in                                           *)
    (*   let iter = mk_true in                                                         *)
    (*   let xy_idedges =                                                              *)
    (*     start_solving icfg call_g call_g' src sink filename iter                    *)
    (*   in                                                                            *)
		(* 	let iter =                                                                    *)
    (*     And(iter, Neg(get_call_preds (InterNode.entryof "print_field") xy_idedges)) *)
    (*   in                                                                            *)
    (*   let xy_idedges =                                                              *)
    (*     start_solving icfg call_g call_g' src sink filename iter                    *)
    (*   in                                                                            *)
      
    (*   exit 0;                                                                       *)
    (* else if String.compare "alarm1_2" filename = 0 then                             *)
    (*   let _ = prerr_endline "alarm1_1" in                                           *)
    (*   let iter = mk_true in                                                         *)
    (*   let xy_idedges =                                                              *)
    (*     start_solving icfg call_g call_g' src sink filename iter                    *)
    (*   in                                                                            *)
		(* 	let iter =                                                                    *)
    (*     And(iter, Neg(get_call_preds (InterNode.entryof "print_field") xy_idedges)) *)
    (*   in                                                                            *)
    (*   let xy_idedges =                                                              *)
    (*     start_solving icfg call_g call_g' src sink filename iter                    *)
    (*   in                                                                            *)
      
    (*   exit 0;                                                                       *)
		
		(* enum-1.1@enum *)
		(* if String.compare "alarm1_1" filename = 0 then                                               *)
    (*   let _ = prerr_endline "alarm1_1" in                                                        *)
    (*   let iter = mk_true in                                                                      *)
    (*   let xy_idedges =                                                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                 *)
    (*   in                                                                                         *)
		(* 	let iter =                                                                                 *)
    (*     And(iter, Neg(get_edge_id call "multi_printf_internal" "single_cast_printf" xy_idedges)) *)
    (*   in                                                                                         *)
    (*   let xy_idedges =                                                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                 *)
    (*   in                                                                                         *)
      
    (*   exit 0;                                                                                    *)
		
		(** zoem *)
		(* if String.compare "alarm1_1" filename = 0 then                              *)
    (*   let _ = prerr_endline "alarm1_1" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
			
    (*   exit 0;                                                                   *)
    (* else if String.compare "alarm1_2" filename = 0 then                         *)
    (*   let _ = prerr_endline "alarm1_2" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, Neg(get_call_preds (InterNode.entryof "yamErrx") xy_idedges)) *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
			
    (*   exit 0;                                                                   *)
		(* else if String.compare "alarm1_3" filename = 0 then                         *)
    (*   let _ = prerr_endline "alarm1_3" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
      
    (*   exit 0                                                                    *)
    (* else if String.compare "alarm1_4" filename = 0 then                         *)
    (*   let _ = prerr_endline "alarm1_4" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
        
    (*   exit 0                                                                    *)
    (* else if String.compare "alarm1_5" filename = 0 then                         *)
    (*   let _ = prerr_endline "alarm1_5" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)

    (*   exit 0                                                                    *)
    (* else if String.compare "alarm1_6" filename = 0 then                         *)
    (*   let _ = prerr_endline "alarm1_6" in                                       *)
    (*   let iter = mk_true in                                                     *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)

    (*   let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "yamErr" xy_idedges)                   *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, get_edge_id call "ptp" "mcxTingPrint" xy_idedges)             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)
		(* 	let iter =                                                                *)
    (*     And(iter, Neg (get_edge_id call "ptp" "yamErr" xy_idedges))             *)
    (*   in                                                                        *)
    (*   let xy_idedges =                                                          *)
    (*     start_solving icfg call_g call_g' src sink filename iter                *)
    (*   in                                                                        *)

    (*   exit 0                                                                    *)
		
		(** urjtag *)	
    (* if String.compare "alarm1_1" filename = 0 then                                                                                             *)
    (*   let _ = prerr_endline "alarm1_1" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* params 에 값이 쓰여야 하는데 안 쓰임 *)                                                                                                           *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id ret "cmd_writemem_run" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
            
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_2" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_2" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   (* params 에 값이 쓰여야 하는데 안 쓰임 *)                                                                                                           *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id ret "cmd_flashmem_run" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_3" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_3" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   (* params 에 값이 쓰여야 하는데 안 쓰임 *)                                                                                                           *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id ret "cmd_flashmem_run" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_4" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_4" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, (get_edge_id call "prototype_bus_new" "prototype_bus_signal_parse" xy_idedges))                                              *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   (* seems to be true *)                                                                                                                   *)
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_5" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_5" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, (get_edge_id call "svf_run" "svf_bison_init" xy_idedges))                                                                    *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, (get_edge_id call "svf_bison_init" "svf_flex_init" xy_idedges))                                                              *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, (get_call_preds (InterNode.entryof "progress_nl") xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_call_preds (InterNode.entryof "progress_nl") xy_idedges))                                                           *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   exit 0                                                                                                                                   *)
    (* else if String.compare "alarm1_6" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_6" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, (get_edge_id call "prototype_bus_new" "prototype_bus_signal_parse" xy_idedges))                                              *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   (* seems to be true *)                                                                                                                   *)
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_7" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_7" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "svfparse" "svf_run" xy_idedges, get_edge_id call "svf_run" "cmd_run" xy_idedges)))                *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* according to the contents of cmds *)                                                                                                  *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_svf_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges)))     *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_8" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_8" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
    (*   let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_emit_ports" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_process_cell_info" "cmd_run" xy_idedges))                                                        *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* jc->instr_list, jc->ainfo_list 에 임의의 입력이 들어가야 하지만, parse_vhdl_elem에 jc가 들어가지 않으므로 불가*)                                                *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_process_register_access" "cmd_run" xy_idedges))                                                  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_set_instruction_length" "cmd_run" xy_idedges))                                                   *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* according to the contents of cmds *)                                                                                                  *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_bsdl_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges)))    *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_include_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges))) *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_script_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges)))  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "create_register" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* cmd in detect_parts is related to other source *)                                                                                     *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "detect_parts" "cmd_run" xy_idedges))                                                                  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* cmd_detect_run does not modify the value of params *)                                                                                 *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id ret "cmd_detect_run" "cmd_run" xy_idedges))                                                                 *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* jtag_parse_stream involves other source *)                                                                                            *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "jtag_parse_stream" "jtag_parse_line" xy_idedges))                                                     *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
			
    (*   exit 0                                                                                                                                   *)
		(* else if String.compare "alarm1_9" filename = 0 then                                                                                        *)
    (*   let _ = prerr_endline "alarm1_9" in                                                                                                      *)
    (*   let iter = mk_true in                                                                                                                    *)
    (*   let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_bsdl_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges)))    *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_include_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges))) *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (And (get_edge_id ret "cmd_script_run" "cmd_run" xy_idedges, get_edge_id call "cmd_run" "cmd_initbus_run" xy_idedges)))  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "detect_parts" "cmd_run" xy_idedges))                                                                  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_emit_ports" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_process_cell_info" "cmd_run" xy_idedges))                                                        *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_process_register_access" "cmd_run" xy_idedges))                                                  *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "bsdl_set_instruction_length" "cmd_run" xy_idedges))                                                   *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id ret "cmd_detect_run" "cmd_run" xy_idedges))                                                                 *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "create_register" "cmd_run" xy_idedges))                                                               *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
		(* 	(* jtag_parse_stream involves other source *)                                                                                            *)
		(* 	let iter =                                                                                                                               *)
    (*     And(iter, Neg (get_edge_id call "jtag_parse_stream" "jtag_parse_line" xy_idedges))                                                     *)
    (*   in                                                                                                                                       *)
		(* 	let xy_idedges =                                                                                                                         *)
    (*     start_solving icfg call_g call_g' src sink filename iter                                                                               *)
    (*   in                                                                                                                                       *)
            
    (*   exit 0                                                                                                                                   *)
		(* else                                                                                                                                       *)
    (*   assert(false)                                                                                                                            *)


		(** pal *)
		(* if String.compare "alarm1_1" filename = 0 then               *)
    (*   let _ = prerr_endline "alarm1_1" in                        *)
    (*   let iter = mk_true in                                      *)
		(* 	(* ./pal -p %n *)                                          *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm2_1" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm2_1" in                        *)
    (*   let iter = mk_true in                                      *)
		(* 	(*./pal -f ../share/pal/pal.conf *)                        *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm2_2" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm2_2" in                        *)
    (*   let iter = mk_true in                                      *)
		(* 	(* ./pal -p /home/wslee/pal-0.4.3/share/pal/%n *)          *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)


		(** shntool-3.0.10@shntool *)
		if String.compare "alarm1_1" filename = 0 then
      let _ = prerr_endline "alarm1_1" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
		else if String.compare "alarm1_2" filename = 0 then
      let _ = prerr_endline "alarm1_2" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
		else if String.compare "alarm1_3" filename = 0 then
      let _ = prerr_endline "alarm1_3" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
		else if String.compare "alarm1_4" filename = 0 then
      let _ = prerr_endline "alarm1_4" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
		else if String.compare "alarm1_5" filename = 0 then
      let _ = prerr_endline "alarm1_5" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
    else if String.compare "alarm1_6" filename = 0 then
      let _ = prerr_endline "alarm1_6" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* is not source *)
			let iter =
        And(iter, Neg (get_call_preds (InterNode.entryof "parse") xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
            
      exit 0
		else if String.compare "alarm1_7" filename = 0 then
      let _ = prerr_endline "alarm1_7" in
      let iter = mk_true in
      let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, Or (get_edge_id call "split_file" "st_snprintf" xy_idedges, get_edge_id call "cue_sprintf" "st_snprintf" xy_idedges))
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			let iter =
        And(iter, get_edge_id call "split_main" "parse___7" xy_idedges)
      in
			let xy_idedges =
        start_solving icfg call_g call_g' src sink filename iter
      in
			(* ./shntool split -n "%n" -f test.cue test.wav *)
            
      exit 0
		else
      assert(false)


(* if String.compare "alarm1_1" filename = 0 then               *)
    (*   let _ = prerr_endline "alarm1_1" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_2" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_2" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_3" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_3" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_4" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_4" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_5" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_5" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
    (* else if String.compare "alarm1_6" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_6" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_7" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_7" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_8" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_8" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else if String.compare "alarm1_9" filename = 0 then          *)
    (*   let _ = prerr_endline "alarm1_9" in                        *)
    (*   let iter = mk_true in                                      *)
    (*   let xy_idedges =                                           *)
    (*     start_solving icfg call_g call_g' src sink filename iter *)
    (*   in                                                         *)
            
    (*   exit 0                                                     *)
		(* else                                                         *)
    (*   assert(false)                                              *)
