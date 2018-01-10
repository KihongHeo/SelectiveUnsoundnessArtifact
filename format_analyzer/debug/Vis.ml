open VocabA
open VocabB
open PPVocab
open UserInput.Input
open PPIL
open Syn
open Pos
open Dug
open CallGraph
    
module Weight = struct
  type edge = Dug.DFG.E.t
  type t = int
  let weight e =
    let v = Dug.DFG.E.dst e in
    if InterNode.get_pid v = "pfatal_with_name" &&
       InterNode.is_entry_node v then
      1
    else
      1
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end
module SPath = Graph.Path.Dijkstra (Dug.DFG) (Weight)

module Weight_CG = struct
  type edge = CG.E.t
  type t = int
  let weight e =
    let v = CG.E.dst e in
    1
  let compare = Pervasives.compare
  let add = (+)
  let zero = 0
end
module SPath_CG = Graph.Path.Dijkstra (CG) (Weight_CG)

let marshal_dir = "marshal_alarms"
let dir = "vis_result"
let callgraph_file = "__callgraph"
let dot_path s = dir ^ "/" ^ s ^ ".dot"
let svg_path_file s = s ^ ".svg"
let svg_path s = dir ^ "/" ^ s ^ ".svg"

let make_marshal_dir () =
  try Unix.mkdir marshal_dir 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | e -> raise e


let make_dir () =
  try Unix.mkdir dir 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | e -> raise e

let make_dot_file file contents =
  let dot_intro graph_name =
    Format.print_string ("digraph " ^ graph_name ^ " {");
    Format.print_cut ();
    Format.print_string "node [shape=box];";
    Format.print_cut () in
  let dot_ending () = Format.print_string "}" in
  let chan = open_out (dot_path file) in
  Format.set_formatter_out_channel chan;
  Format.open_vbox 0;
  dot_intro file;
  contents ();
  dot_ending ();
  Format.close_box ();
  Format.print_newline ();
  close_out chan;
  Format.set_formatter_out_channel stderr

let make_svg src tgt =
  Unix.create_process "dot" [|"dot"; "-Tsvg"; "-o" ^ tgt; src|]
    Unix.stdin Unix.stdout Unix.stderr
  |> ignore;
  Unix.wait () |> ignore

let dump_nodes f cfg =
  let print_node node _ =
    let cmd = default (Syn.Cskip Pos_as_OT.unknown_pos) (IntraCfg.get_cmd cfg node) in
    let callnode = IntraCfg.is_call_node cfg node in
    let node_string = string_of_intra_node node in
    let s = node_string ^ " [label=\"" ^ node_string ^ ": "
            ^ (if IntraNode.is_entry_node node then
                 f ^ string_of_list "(" ")" "," id (IntraCfg.args cfg)
               else String.escaped (string_of_cmd cmd))
            ^ "\""
            ^ (if callnode then " style=filled color=grey" else "")
            ^ "];" in
    Format.print_string s;
    Format.print_cut () in
  IntraCfg.NodeSet.fold print_node (IntraCfg.nodes cfg) ()

let dump_edges f cfg =
  let print_edge node succ _ =
    let s = string_of_intra_node node ^ " -> " ^ string_of_intra_node succ
            ^ ";" in
    Format.print_string s;
    Format.print_cut () in
  let print_succs node succs _ =
    IntraCfg.NodeSet.fold (print_edge node) succs () in
  IntraCfg.NodeMap.fold print_succs (IntraCfg.succ cfg) ()

let make_f_dot f cfg =
  let contents () =
    dump_nodes f cfg;
    Format.print_cut ();
    dump_edges f cfg in
  make_dot_file f contents

let dump_cg_nodes cfgs =
  let print_f_node f _ _ =
    let s = f ^ " [URL=\"" ^ svg_path_file f ^ "\"];" in
    Format.print_string s;
    Format.print_cut () in
  InterCfg.PidMap.fold print_f_node cfgs ()

let dump_cg_edges cg =
  let print_call caller callee _ =
    let s = caller ^ " -> " ^ callee ^ ";" in
    Format.print_string s;
    Format.print_cut () in
  let print_callees caller callees _ =
    InterCfg.PidSet.fold (print_call caller) callees () in
  InterCfg.PidMap.fold print_callees (Global.Callgraph.calls cg) ()

let make_cg_dot cfgs cg =
  let contents () = dump_cg_nodes cfgs; dump_cg_edges cg in
  make_dot_file callgraph_file contents

let vis_f f cfg _ =
  prerr_endline ("Visualize: " ^ f);
  make_f_dot f cfg;
  make_svg (dot_path f) (svg_path f)

let vis_callgraph cfgs cg =
  make_cg_dot cfgs cg;
  make_svg (dot_path callgraph_file) (svg_path callgraph_file)

let run g f =
  make_dir ();
  match InterCfg.PidMap.find f (InterCfg.cfgs (G.icfg g)) with
  | None -> prerr_endline ("No function:" ^ f)
  | Some cfg -> vis_f f cfg ()

let run_all g =
  let cfgs = InterCfg.cfgs (G.icfg g) in
  make_dir ();
	vis_callgraph cfgs (G.callgraph g);
  InterCfg.PidMap.fold vis_f cfgs ();
  ()

let node2pos icfg node =
  let cmd = (default (Syn.Cskip Pos.Pos_as_OT.unknown_pos)
                          (InterCfg.get_cmd icfg node)) in
  match cmd with
  | Cset (l, e, pos) -> pos
  | Cexternal (l, pos) -> pos
  | Calloc (l, a, pos) -> pos
  | Csalloc (l, s, pos) -> pos
  | Cfalloc (l, name, pos) -> pos
  | Ccall (ret_opt, f, args0, pos) -> pos
  | Creturn (ret_opt, pos) -> pos
  | _ -> Pos.Pos_as_OT.unknown_pos

let make_du_dot name icfg (source, sink, spg) orig_dug =
	let dump_nodes spg =
		CG.iter_vertex (fun node ->
			let (f, node) = node in 
			let cmd = string_of_cmd (default (Syn.Cskip Pos_as_OT.unknown_pos) (InterCfg.get_cmd icfg (f, node))) in
			let cmd = Str.global_replace (Str.regexp "\"") "\\\"" cmd in 
			let s = Format.sprintf "%s_%s [label=\"%s:%s at %s\" %s];"			
				(string_of_intra_node node) f (*(DPos.DPos.string_of_pos (node2pos icfg (f,node)))*) 
				(string_of_intra_node node) cmd f
				(if (Pervasives.compare (f, node) source) = 0 then "style=filled color=red"
				else if (Pervasives.compare (f, node) sink) = 0 then "style=filled color=blue"
				else "") 
			in 
			Format.print_string s;
      Format.print_cut ()	  
			) spg
	in 
	let dump_edges spg =
		CG.iter_edges 
			(fun node succ -> 
				let (f, node) = node in
				let (f', succ) = succ in
		(* let succs = Dug.succ node dug in                 *)
    (* let make_dus_succ succ =                         *)
      (* let abslocs = Dug.get_abslocs (f, node) (f', succ) orig_dug in *)
    (*   (node, succ, abslocs) in                       *)
    (* let dus = List.map make_dus_succ succs in        *)
    (* let print_edge (node ,succ, abslocs) =           *)
			let s = Format.sprintf "%s_%s -> %s_%s;" 
				(string_of_intra_node node) f 
				(string_of_intra_node succ) f' (*(PPMem.tostring_pow_loc abslocs)*) in
      Format.print_string s;
      Format.print_cut ()
			) spg
	in
  let contents () = dump_nodes spg; dump_edges spg in
  make_dot_file name contents

let print_pos pos pid =
  if pos.pos_line = -1 then
    ()
  else
    (prerr_endline (pid ^ "@" ^ (DPos.DPos.string_of_pos pos));
    )
      

let print_path spg src_node sink_node icfg =
	(* let supergraph =                                *)
	(* 	let g =                                       *)
  (* 		let succ_map = InterCfg.succ icfg in        *)
  (* 		InterCfg.NodeMap.fold (fun node succs g ->  *)
  (* 			InterCfg.NodeSet.fold (fun succ g ->      *)
  (* 				DFG.add_edge g node succ                *)
  (* 				) succs g                               *)
  (* 			) succ_map DFG.empty                      *)
	(* 	in                                            *)
	(* 	let intracfgs = InterCfg.cfgs icfg in         *)
	(* 	InterCfg.PidMap.fold (fun pid cfg g ->        *)
	(* 		let succ_map = IntraCfg.succ cfg in         *)
  (* 		IntraCfg.NodeMap.fold (fun node succs g ->  *)
  (* 			IntraCfg.NodeSet.fold (fun succ g ->      *)
  (* 				DFG.add_edge g (pid, node) (pid, succ)  *)
  (* 				) succs g                               *)
  (* 			) succ_map g                              *)
	(* 		) intracfgs g                               *)
	(* in                                              *)
  let (path_list, w) =
    try
      SPath_CG.shortest_path spg src_node sink_node
    with
    | Not_found ->
      (prerr_endline "Shortest path not found Error";([],0))
    | Invalid_argument _ ->
      (prerr_endline "Invalid argument Error";([],0))
  in
  let edge_iter acc e =
    let src = CG.E.src e in
    let dst = CG.E.dst e in
		(* if (DFG.mem_vertex dug src) then   *)
    	print_pos (node2pos icfg src) (InterNode.get_pid src);
		(* if (DFG.mem_vertex dug dst) then *)
    	print_pos (node2pos icfg dst) (InterNode.get_pid dst)
  in
  List.fold_left edge_iter () path_list
    

let print_alarm_dugs icfg nodes_and_spgs_list orig_dug =
  make_dir ();  
  List.fold_left (fun alarm nodes_and_spgs ->
      prerr_endline ("Visualizing DUGraphs of Alarm #" ^ (string_of_int alarm));
      let _ = 
        List.fold_left (fun source (src_node, sink_node, spg) ->
     prerr_endline ("\t source #" ^ (string_of_int source));
     make_du_dot (Format.sprintf "alarm%d_%d" alarm source) icfg (src_node, sink_node, spg) orig_dug;
     (* make_marshal_dir ();                                                                                      *)
     (* DugCut.marshal_out (Format.sprintf "marshal_alarms/alarm%d_%d" alarm source) src_node sink_node icfg dug; *)
     (* let _ = print_path spg src_node sink_node icfg in *)
     source + 1
	  ) 1 nodes_and_spgs
      in 
      alarm + 1	
    ) 1 nodes_and_spgs_list 
