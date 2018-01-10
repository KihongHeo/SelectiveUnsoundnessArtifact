open Cil
open Format
open VocabA
open VocabB
open PPVocab
open UserInput.Input
open Vali.Vali
open Pos
open DPos
open Dug
open CallGraph
module PMap = BatMap.PMap

let query_find pos m = try PMap.find pos m with Not_found -> []
let query_add pos (q, status) m =
  PMap.add pos ((q, status) :: query_find pos m) m

let rec partition_rec acc queries =
  match queries with
  | [] -> acc
  | ((q, pos), status) :: tl ->
    partition_rec (query_add pos (q, status) acc) tl
let partition queries = partition_rec PMap.empty queries

let is_clean _ v = List.for_all (fun (_, status) -> status = Clean) v
let print_stats m =
  let all_n = PMap.cardinal m in
  let clean_n = PMap.cardinal (PMap.filter is_clean m) in
  prerr_endline ("#all: " ^ string_of_int all_n);
  prerr_endline ("#clean: " ^ string_of_int clean_n);
  prerr_endline ("#tainted: " ^ string_of_int (all_n - clean_n))

let string_of_query e = PPIL.string_of_exp false e

let print_status = function
  | Tainted exts ->
    (open_box 0;
     print_string "tainted: ";
     PPMem.print_pow_proc_pos exts;
     close_box ())
  | Clean ->
    (open_box 0;
     print_string "clean";
     close_box ())

let rec print_detail_1 pos v =
  match v with
  | [] -> ()
  | (q, status) :: tl ->
    if status = Clean then ()
    else
    (open_box 0;
     print_string (Pos_as_OT.string_of_pos pos ^ ":" ^ string_of_query q ^ ":");
     print_status status;
     close_box ();
     print_cut ();
     print_detail_1 pos tl)

let print_detail m =
  open_vbox 0;
  PMap.iter print_detail_1 m;
  close_box ();
  print_flush ()

let bug_mp3rename =
  BatSet.empty 
  |> BatSet.add "mp3rename.c:554<-argv@mp3rename.c:25"
let bug_uni = 
  BatSet.empty 
  |> BatSet.add "uni2ascii.c:3111<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3120<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3130<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3148<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3152<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3165<-argv@uni2ascii.c:2722"
  |> BatSet.add  "uni2ascii.c:3167<-argv@uni2ascii.c:2722"

let bug_ghost =
  BatSet.empty 
  |> BatSet.add "genconf.c:773<-argv@genconf.c:338"
  |> BatSet.add "genconf.c:865<-argv@genconf.c:338"
let bug_latex =
  BatSet.empty 
  |> BatSet.add "main.c:873<-_IO_getc@parser.c:438"
  |> BatSet.add "main.c:873<-_IO_getc@parser.c:449"
let bug_daemon = 
  BatSet.empty 
  |> BatSet.add "prog.c:1502<-argv@daemon.c:3299"
let bug_rptp = 
  BatSet.empty
  |> BatSet.add "rptp.c:320<-recv@rptp.c:358"
  |> BatSet.add "rptp.c:320<-fread@rptp.c:687"
  |> BatSet.add "rptp.c:261<-argv@rptp.c:153"
let bug_a2ps = 
  BatSet.empty 
  |> BatSet.add "dstring.c:326<-getenv@userdata.c:62" (* 4 *)
  |> BatSet.add "dstring.c:326<-getenv@userdata.c:70" 
  |> BatSet.add "dstring.c:326<-getenv@userdata.c:72" 
  |> BatSet.add "dstring.c:326<-getenv@metaseq.c:538" (* 5 *)
  |> BatSet.add "dstring.c:326<-getenv@metaseq.c:568"
  |> BatSet.add "dstring.c:326<-argv@main.c:928"     (* 8 *)
let bug_dico = 
  BatSet.empty
  |> BatSet.add "diag.c:80<-getenv@connect.c:135"  (* 2*)

let bug_dicod = 
  BatSet.empty
  |> BatSet.add "main.c:1291<-argv@main.c:1355"

let bug_gnuplot = 
  BatSet.empty 
  |> BatSet.add "mouse.c:439<-fgets@misc.c:197"

let bug_putty = 
  BatSet.empty
  |> BatSet.add "misc.c:422<-fgetc@x11fwd.c:496"
  |> BatSet.add "misc.c:422<-fgetc@x11fwd.c:497"
  |> BatSet.add "misc.c:422<-fgetc@x11fwd.c:502"
  |> BatSet.add "misc.c:422<-fgetc@x11fwd.c:503"
  |> BatSet.add "misc.c:422<-fgetc@x11fwd.c:506"

let bug_urjtag = 
  BatSet.empty
  |> BatSet.add "prototype.c:174<-fgets@jtag.c:226"
  |> BatSet.add "prototype.c:179<-fgets@jtag.c:226"
  |> BatSet.add "prototype.c:191<-fgets@jtag.c:226"
  |> BatSet.add "prototype.c:202<-fgets@jtag.c:226"
  |> BatSet.add "prototype.c:207<-fgets@jtag.c:226"
  |> BatSet.add "prototype.c:219<-fgets@jtag.c:226"

  |> BatSet.add "prototype.c:174<-fgetc@parse.c:130"
  |> BatSet.add "prototype.c:179<-fgetc@parse.c:130"
  |> BatSet.add "prototype.c:191<-fgetc@parse.c:130"
  |> BatSet.add "prototype.c:202<-fgetc@parse.c:130"
  |> BatSet.add "prototype.c:207<-fgetc@parse.c:130"
  |> BatSet.add "prototype.c:219<-fgetc@parse.c:130"

let bug_pal = 
  BatSet.empty
  |> BatSet.add "input.c:466<-argv@main.c:703"
  |> BatSet.add "input.c:466<-fgets@input.c:633"
  |> BatSet.add "input.c:621<-argv@main.c:703"

let bug_shntool = 
  BatSet.empty 
  |> BatSet.add "core_mode.c:766<-argv@core_shntool.c:357"

let bug_rrd = 
  BatSet.empty
  |> BatSet.add "rrd_info.c:28<-argv@rrd_tool.c:400"

let bug_sdop = 
  BatSet.empty 
  |> BatSet.add "write.c:1347<-fgets@read.c:772" (* 1-1 *)
  |> BatSet.add "number.c:123<-fgets@read.c:772" (* 2-1 *)
  |> BatSet.add "number.c:127<-fgets@read.c:772" 
  |> BatSet.add "number.c:131<-fgets@read.c:772"
  |> BatSet.add "number.c:142<-fgets@read.c:772"
  |> BatSet.add "number.c:166<-fgets@read.c:772"
  |> BatSet.add "number.c:170<-fgets@read.c:772"
  |> BatSet.add "number.c:174<-fgets@read.c:772"
  |> BatSet.add "number.c:185<-fgets@read.c:772"
  |> BatSet.add "number.c:209<-fgets@read.c:772"
  |> BatSet.add "number.c:213<-fgets@read.c:772"
  |> BatSet.add "number.c:217<-fgets@read.c:772"
  |> BatSet.add "number.c:228<-fgets@read.c:772"
  |> BatSet.add "number.c:123<-fgets@read.c:320"  (* 2-2 *)
  |> BatSet.add "number.c:123<-fgets@read.c:351"
  |> BatSet.add "number.c:123<-fgets@read.c:394"
  |> BatSet.add "number.c:123<-fgets@read.c:553"
  |> BatSet.add "number.c:127<-fgets@read.c:320"
  |> BatSet.add "number.c:127<-fgets@read.c:351"
  |> BatSet.add "number.c:127<-fgets@read.c:394"
  |> BatSet.add "number.c:127<-fgets@read.c:553"
  |> BatSet.add "number.c:131<-fgets@read.c:320"
  |> BatSet.add "number.c:131<-fgets@read.c:351"
  |> BatSet.add "number.c:131<-fgets@read.c:394"
  |> BatSet.add "number.c:131<-fgets@read.c:553"
  |> BatSet.add "number.c:142<-fgets@read.c:320"
  |> BatSet.add "number.c:142<-fgets@read.c:351"
  |> BatSet.add "number.c:142<-fgets@read.c:394"
  |> BatSet.add "number.c:142<-fgets@read.c:553"
  |> BatSet.add "number.c:166<-fgets@read.c:320"
  |> BatSet.add "number.c:166<-fgets@read.c:351"
  |> BatSet.add "number.c:166<-fgets@read.c:394"
  |> BatSet.add "number.c:166<-fgets@read.c:553"
  |> BatSet.add "number.c:170<-fgets@read.c:320"
  |> BatSet.add "number.c:170<-fgets@read.c:351"
  |> BatSet.add "number.c:170<-fgets@read.c:394"
  |> BatSet.add "number.c:170<-fgets@read.c:553"
  |> BatSet.add "number.c:174<-fgets@read.c:320"
  |> BatSet.add "number.c:174<-fgets@read.c:351"
  |> BatSet.add "number.c:174<-fgets@read.c:394"
  |> BatSet.add "number.c:174<-fgets@read.c:553"
  |> BatSet.add "number.c:185<-fgets@read.c:320"
  |> BatSet.add "number.c:185<-fgets@read.c:351"
  |> BatSet.add "number.c:185<-fgets@read.c:394"
  |> BatSet.add "number.c:185<-fgets@read.c:553"
  |> BatSet.add "number.c:209<-fgets@read.c:320"
  |> BatSet.add "number.c:209<-fgets@read.c:351"
  |> BatSet.add "number.c:209<-fgets@read.c:394"
  |> BatSet.add "number.c:209<-fgets@read.c:553"
  |> BatSet.add "number.c:213<-fgets@read.c:320"
  |> BatSet.add "number.c:213<-fgets@read.c:351"
  |> BatSet.add "number.c:213<-fgets@read.c:394"
  |> BatSet.add "number.c:213<-fgets@read.c:553"
  |> BatSet.add "number.c:217<-fgets@read.c:320"
  |> BatSet.add "number.c:217<-fgets@read.c:351"
  |> BatSet.add "number.c:217<-fgets@read.c:394"
  |> BatSet.add "number.c:217<-fgets@read.c:553"
  |> BatSet.add "number.c:228<-fgets@read.c:320"
  |> BatSet.add "number.c:228<-fgets@read.c:351"
  |> BatSet.add "number.c:228<-fgets@read.c:394"
  |> BatSet.add "number.c:228<-fgets@read.c:553"
  |> BatSet.add "write.c:1347<-fgets@read.c:320"
  |> BatSet.add "write.c:1347<-fgets@read.c:351"
  |> BatSet.add "write.c:1347<-fgets@read.c:394"
  |> BatSet.add "write.c:1347<-fgets@read.c:553"

let get_alarm m = 
  PMap.foldi (fun pos v set -> 
      List.fold_left (fun set (q,status) ->
        match status with
          Clean -> set 
        | Tainted exts ->
          PowExtProcPos.fold (fun (proc,pos') set -> 
            BatSet.add 
              ((Pos_as_OT.string_of_pos pos)^"<-"^(proc^"@"^(DPos.string_of_pos pos')))
              set) exts set) set v) m BatSet.empty

let check_alarm f alarm =
  let bug = 
    if Str.string_match (Str.regexp ".*mp3rename.*") f.fileName 0 then bug_mp3rename
    else if Str.string_match (Str.regexp ".*ghost.*") f.fileName 0 then bug_ghost
    else if Str.string_match (Str.regexp ".*latex.*") f.fileName 0 then bug_latex
    else if Str.string_match (Str.regexp ".*uni.*") f.fileName 0 then bug_uni
    else if Str.string_match (Str.regexp ".*daemon.*") f.fileName 0 then bug_daemon
    else if Str.string_match (Str.regexp ".*rptp.*") f.fileName 0 then bug_rptp
    else if Str.string_match (Str.regexp ".*a2ps.*") f.fileName 0 then bug_a2ps
    else if Str.string_match (Str.regexp ".*dico-2.0.c.*") f.fileName 0 then bug_dico
    else if Str.string_match (Str.regexp ".*ddico.*") f.fileName 0 then bug_dicod
    else if Str.string_match (Str.regexp ".*gnuplot.*") f.fileName 0 then bug_gnuplot
    else if Str.string_match (Str.regexp ".*putty.*") f.fileName 0 then bug_putty
    else if Str.string_match (Str.regexp ".*urjtag.*") f.fileName 0 then bug_urjtag
    else if Str.string_match (Str.regexp ".*pal.*") f.fileName 0 then bug_pal
    else if Str.string_match (Str.regexp ".*shntool.*") f.fileName 0 then bug_shntool
    else if Str.string_match (Str.regexp ".*rrd.*") f.fileName 0 then bug_rrd
    else if Str.string_match (Str.regexp ".*sdop.*") f.fileName 0 then bug_sdop
    else BatSet.empty 
  in
  prerr_endline "Alarms";
  BatSet.iter (fun x -> prerr_endline x) alarm;
  prerr_newline ();
  prerr_endline ("#bugs : "^(string_of_int (BatSet.cardinal bug)));
  prerr_endline ("#unproven : "^(string_of_int (BatSet.cardinal alarm)));
  prerr_endline ("#found bugs : "^(string_of_int (BatSet.cardinal (BatSet.intersect bug alarm))));
  ()

let make_nodes2pos g nodes =
  let node2pos map node =
    let cmd = Run.get_cmd node g in 
    match cmd with 
    | Syn.Ccall (_, _, _, pos) -> 
      PMap.add (Pos_as_OT.string_of_pos pos) node map
    | _ -> map
  in
  List.fold_left node2pos PMap.empty nodes

exception NoSink

let handling_unguarded_calls g icfg =
	(* g : callgraph *)
	(* info : (InterNode.t * InterNode.t list) list *)
	(* return value will be added to prop_info, which represents analysis result and DU information *)
	let call_nodes = InterCfg.callnodesof icfg in 
	CG.fold_edges (fun v1 v2 info ->
			let f1, f2 = (InterNode.get_pid v1, InterNode.get_pid v2) in 
			let f1_cfg =
				let cfg = get_some (InterCfg.PidMap.find f1 (InterCfg.cfgs icfg)) in 
				let succ_map = IntraCfg.succ cfg in
				IntraCfg.NodeMap.fold (fun node succset g ->
        	IntraCfg.NodeSet.fold (fun succ g -> CG.add_edge g (f1, node) (f1, succ)) succset g
      	) succ_map CG.empty	 
			in 
			let entry_node = (f1, IntraNode.Entry) in 
			let idom = MaxSatHeuristic.CGDom.compute_idom f1_cfg entry_node in
			(* among callnodes at f1, if none satisfies the following conditions, add (f1, [f2]) into info *)
			(* 1) f2 is one of succs in icfg. 2) the node is dominated by an assume stmt.  *)
			let call_nodes = InterCfg.NodeSet.filter (fun v -> String.compare (InterNode.get_pid v) f1 = 0) call_nodes in 
			let calls_at_f1 =
				let succ_map = InterCfg.succ icfg in 
				InterCfg.NodeSet.filter (fun v ->
					let succs = default InterCfg.NodeSet.empty (InterCfg.NodeMap.find v succ_map) in 
					InterCfg.NodeSet.mem (f2, IntraNode.Entry) succs  		 
					) call_nodes    
			in
			let rec is_unguarded v = 
				let cmd = default (Syn.Cskip Pos.Pos_as_OT.unknown_pos) (InterCfg.get_cmd icfg v) in 
    	  if (InterNode.eq_dec (f1, IntraNode.Entry) v) then true
				else 
					(match cmd with 
					| Syn.Cassume _ -> false 
					| _ -> is_unguarded (idom v) )    
    	in
			if (InterCfg.NodeSet.for_all is_unguarded calls_at_f1) then
				let _ = prerr_endline (f1^" => "^f2) in  
				((f1, IntraNode.Entry), [(f2, IntraNode.Entry)])::info 
			else info  
		) g []	   

let run : G.t * Pre.t * Mem.t * Dug.t * Table.t * Table.t * Worklist.Workorder.t * Cil.file-> unit
  = fun (glob, pre, mem, dug, inputof, outputof, ord, file) ->
    let icfg = G.icfg glob in
    let spg = 
      let g = 
      	let call_map = Global.Callgraph.calls (G.callgraph glob) in
      	let call_graph =  
        	  InterCfg.PidMap.fold (fun caller callees g ->
             InterCfg.PidSet.fold (fun callee g ->
                 if String.compare callee caller = 0 then g else
                 CG.add_edge g (caller, IntraNode.Entry) (callee, IntraNode.Entry)) callees g  
        	    ) call_map CG.empty
      	in
      	call_graph 
      in 
      g
    in 
		let unguarded_calls_info = [] (*handling_unguarded_calls spg icfg*) in 
    let res = collect_alarm_result glob inputof |> partition in
    Step.small "Query status" print_stats res;
    Step.small_side !Options.opt_print_all_query "All queries" print_detail res;
    let alarm = get_alarm res in
    let _ = check_alarm file alarm in

    let all_nodes : InterNode.t list =  
      InterCfg.PidMap.fold 
    	(fun pid cfg nodes -> nodes@(List.map (fun n -> (pid,n)) (IntraCfg.NodeSet.elements (IntraCfg.nodes cfg))) ) 
    	(InterCfg.cfgs icfg) []
	(* (CG.fold_vertex (fun node acc -> node :: acc) spg [])  *)
	(*(Dug.nodesof dug)*) 
    in 
    let pos_map = make_nodes2pos glob all_nodes in
    RunAlarmHunt.run glob pre res;
    (* let dugraphs_of_alarms = Step.small "Generating du graphs for each queries" make_dugraphs (pos_map, pre, dug, res, mem, inputof, outputof, ord) glob spg unguarded_calls_info in *)
    (* let _ = AlarmVis.vis_alarms icfg dugraphs_of_alarms in *)
    (* let _ = Vis.print_alarm_dugs (G.icfg g) dugraphs_of_alarms dug in *)
    ()
