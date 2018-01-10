open Dug
open CallGraph
open Syn
open Pos
open PPIL
open VocabA
open SPath

let marshal_dir = "marshal_alarms"
let dir = "alarm_vis_result"
let dot_path s = dir ^ "/" ^ s ^ ".dot"

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
    Format.print_string ("digraph " ^ "G" ^ " {");
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

let node2cmd icfg (f, node) =
  (default (Syn.Cskip Pos_as_OT.unknown_pos) (InterCfg.get_cmd icfg (f, node)))

let node2pos icfg node =
  let cmd = node2cmd icfg node in
  match cmd with
  | Cset (l, e, pos) -> pos
  | Cexternal (l, pos) -> pos
  | Calloc (l, a, pos) -> pos
  | Csalloc (l, s, pos) -> pos
  | Cfalloc (l, name, pos) -> pos
  | Ccall (ret_opt, f, args0, pos) -> pos
  | Creturn (ret_opt, pos) -> pos
  | _ -> Pos.Pos_as_OT.unknown_pos

let print_pos pos pid =
  if pos.pos_line = -1 then
    ()
  else
    Format.print_string (pid ^ "@" ^ (DPos.DPos.string_of_pos pos) ^ "\n")
    
let make_supg_dot name icfg (src, sink, (dug, supg)) =
  make_dir ();
  let draw_nodes (f, node) =
    let cmdstr = PPIL.string_of_cmd (node2cmd icfg (f, node)) in
    let cmdstr = Str.global_replace (Str.regexp "\"") "\\\"" cmdstr in
    let color_str =
      let exist_in_dug node =
	DFG.mem_vertex dug node
      in
      if CG.V.compare src (f, node) = 0 then "style=filled color=\"#ff4d4\"" else
      if CG.V.compare sink (f, node) = 0 then "style=filled color=\"#3385ff\"" else
      if exist_in_dug (f, node) then "style=filled color=green"
      else ""
    in
    let node_str = string_of_intra_node node in
    let pos_str = DPos.DPos.string_of_pos (node2pos icfg (f, node)) in  
    let sprintf = Format.sprintf "%s_%s [label=\"%s:%s at %s \t %s\" %s];" in
    let str = sprintf node_str f node_str cmdstr f pos_str color_str in
    Format.print_string str;
    Format.print_cut ()	  
  in
  let draw_edges (f, node) (f', node') =
    let node_str node = string_of_intra_node node in
    let sprintf = Format.sprintf "%s_%s -> %s_%s;" in
    let str = sprintf (node_str node) f (node_str node') f' in
    Format.print_string str;
    Format.print_cut ()
  in
  let dump_nodes supg = CG.iter_vertex draw_nodes supg in
  let dump_edges supg = CG.iter_edges draw_edges supg in
  let contents () = dump_nodes supg; dump_edges supg in
  make_dot_file name contents
    
let make_call_g_dot
    name icfg (src, sink, (backbone, branch)) (x_idedges, y_idedges) =
  make_dir ();
  let draw_nodes (f, node) =
    let cmdstr = PPIL.string_of_cmd (node2cmd icfg (f, node)) in
    let cmdstr = Str.global_replace (Str.regexp "\"") "\\\"" cmdstr in
    let color_str =
      if CG.V.compare src (f, node) = 0 then "style=filled color=\"#ff4d4\"" else
      if CG.V.compare sink (f, node) = 0 then "style=filled color=\"#3385ff\"" else
      ""
    in
    let node_str = string_of_intra_node node in
    let sprintf = Format.sprintf "%s_%s [label=\"%s:%s at %s\" %s];" in
    let str = sprintf node_str f node_str cmdstr f color_str in
    Format.print_string str;
    Format.print_cut ()	  
  in
  let draw_edges idedges is_back ((f, node), lbl, (f', node')) =
    let node_str node = string_of_intra_node node in
    let e =  ((f, node), lbl, (f', node')) in
    let sprintf =
      if (String.compare lbl "C") = 0 then
        (if is_back then Format.sprintf "%s_%s -> %s_%s[label=\"%s\"];"
         else Format.sprintf "%s_%s -> %s_%s[color=green, style=bold, label=\"%s\"];")
    else if (String.compare lbl "R") = 0 then
      Format.sprintf "%s_%s -> %s_%s[style=dotted, label=\"%s\"];"
    else (* Exception *)
      (prerr_endline "Can't classify edge(Call or Ret)";
      assert(false))
    in
    let str =
      sprintf (node_str node) f (node_str node') f'
        (string_of_int (get_edge_id idedges e))
    in
    Format.print_string str;
    Format.print_cut ()
  in
  let dump_nodes call_g = CG.iter_vertex draw_nodes call_g in
  let dump_edges call_g is_back =
    CG.iter_edges_e (draw_edges x_idedges is_back) call_g
  in
  
  let contents () =
    dump_nodes backbone; dump_edges backbone true;
    dump_nodes branch; dump_edges branch false
  in
  make_dot_file name contents


let make_supg_dot_test name icfg (src, sink, supg) idedges =
  make_dir ();
  let draw_nodes (f, node) =
    let cmdstr = PPIL.string_of_cmd (node2cmd icfg (f, node)) in
    let cmdstr = Str.global_replace (Str.regexp "\"") "\\\"" cmdstr in
    let color_str =
      if CG.V.compare src (f, node) = 0 then "style=filled color=red" else
      if CG.V.compare sink (f, node) = 0 then "style=filled color=blue" else
      ""
    in
    let node_str = string_of_intra_node node in
    let sprintf = Format.sprintf "%s_%s [label=\"%s:%s at %s\" %s];" in
    let str = sprintf node_str f node_str cmdstr f color_str in
    Format.print_string str;
    Format.print_cut ()	  
  in
  let draw_edges (f, node) (f', node') =
    let node_str node = string_of_intra_node node in
    let e = CG.find_edge supg (f, node) (f', node') in
    let sprintf = Format.sprintf "%s_%s -> %s_%s[label=\"%s\"];" in
    let str = sprintf (node_str node) f (node_str node') f' (string_of_int (get_edge_id idedges e)) in
    Format.print_string str;
    Format.print_cut ()
  in
  let dump_nodes supg = CG.iter_vertex draw_nodes supg in
  let dump_edges supg = CG.iter_edges draw_edges supg in
  let contents () = dump_nodes supg; dump_edges supg in
  make_dot_file name contents

let make_supg_dot_test2 name icfg (src, sink, supg) =
  make_dir ();
  let draw_nodes (f, node) =
    let cmdstr = PPIL.string_of_cmd (node2cmd icfg (f, node)) in
    let cmdstr = Str.global_replace (Str.regexp "\"") "\\\"" cmdstr in
    let color_str =
      if CG.V.compare src (f, node) = 0 then "style=filled color=red" else
      if CG.V.compare sink (f, node) = 0 then "style=filled color=blue" else
      ""
    in
    let node_str = string_of_intra_node node in
    let sprintf = Format.sprintf "%s_%s [label=\"%s:%s at %s\" %s];" in
    let str = sprintf node_str f node_str cmdstr f color_str in
    Format.print_string str;
    Format.print_cut ()	  
  in
  let draw_edges (f, node) (f', node') =
    let node_str node = string_of_intra_node node in
    let sprintf = Format.sprintf "%s_%s -> %s_%s;" in
    let str = sprintf (node_str node) f (node_str node') f' in
    Format.print_string str;
    Format.print_cut ()
  in
  let dump_nodes supg = CG.iter_vertex draw_nodes supg in
  let dump_edges supg = CG.iter_edges draw_edges supg in
  let contents () = dump_nodes supg; dump_edges supg in
  make_dot_file name contents




let print_alarm icfg (src, sink, (dug, spg)) =
  let (edges, _) =
    try
      SPath.shortest_path spg src sink
    with
    | Not_found ->
      (prerr_endline "print_alarm : Shortest path not found";([],0))
    | Invalid_argument _ ->
      (prerr_endline "print_alarm : Shortest path invalid argument";([],0))
  in
  let edge_iter acc e =
    (* if acc mod 2 = 1 then *)
      let src = CG.E.src e in
      let dst = CG.E.dst e in
      let exist_in_dug node =
        DFG.fold_vertex (fun n b -> (InterNode.eq_dec node n) || b) dug false
      in
      let print () =
        if exist_in_dug src then
          print_pos (node2pos icfg src) (InterNode.get_pid src)
        else if exist_in_dug dst then
          print_pos (node2pos icfg dst) (InterNode.get_pid dst)
        else
          ()
      in
      print ();
      acc+1
    (* else    *)
    (*   acc+1 *)
  in
  let _ = List.fold_left edge_iter 1 edges in
  ()

let analysis_result_marshal filename glob_g pre res =
  make_marshal_dir ();
  let chan = open_out_bin (marshal_dir ^ "/" ^ filename) in
  Marshal.to_channel chan (glob_g, pre, res)
    [Marshal.No_sharing];
  close_out chan
    
let marshal_out filename src sink icfg spg prop_info (call_g, call_g') =
  make_marshal_dir ();
  let chan = open_out_bin (marshal_dir ^ "/" ^ filename) in
  Marshal.to_channel chan (src, sink, icfg, spg, prop_info, (call_g, call_g'))
    [Marshal.No_sharing];
  close_out chan

let vis_alarms icfg g_alarm_list (call_g, call_g') =
  let fold_alarm_list acc g_alarm =
    prerr_endline ("Visualizing DUGraphs of Alarm #" ^ (string_of_int acc));
    let fold_alarm
        alarm_cnt
        (src, sink, spg, ((backbone, branch), (x_idedges, y_idedges)), prop_info) =
			(* print_idedges idedges;                                                                    *)
      prerr_endline ("  source #" ^ (string_of_int alarm_cnt));
      (* Save Dot File *)
      let name = (Format.sprintf "alarm%d_%d" acc alarm_cnt) in
      make_call_g_dot name icfg
        (src, sink, (backbone, branch)) (x_idedges, y_idedges) ;
      (* Save Marshal File *)
      marshal_out name src sink icfg spg prop_info (call_g, call_g');
      
      alarm_cnt + 1
    in
    let _ = List.fold_left fold_alarm 1 g_alarm in
    acc + 1
  in
  List.fold_left fold_alarm_list 1 g_alarm_list
