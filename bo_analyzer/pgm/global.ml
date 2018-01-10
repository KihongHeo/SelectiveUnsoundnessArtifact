open Yojson.Safe
open Vocab
open BasicDom
open InterCfg

type t = {
  file : Cil.file;
  icfg : InterCfg.t;
  callgraph : CallGraph.t;
  dump : Dump.t;
  mem : ItvDom.Mem.t;
}

let get_icfg : t -> InterCfg.t
=fun pgm -> pgm.icfg

let get_callgraph : t -> CallGraph.t
=fun pgm -> pgm.callgraph

let remove_node : InterCfg.node -> t -> t = fun node global ->
  { global with
    icfg = InterCfg.remove_node node global.icfg }

let remove_function : InterCfg.Proc.t -> t -> t = fun pid global ->
  { global with
    icfg = InterCfg.remove_function pid global.icfg
  ; callgraph = CallGraph.remove_function pid global.callgraph
  ; dump = Dump.remove pid global.dump }

let is_rec : InterCfg.pid -> t -> bool = fun pid global ->
  CallGraph.is_rec (get_callgraph global) pid

let remove_unreachable_nodes : t -> t
=fun global ->
  let nodes_all = InterCfg.nodesof global.icfg in
  let unreachable = InterCfg.unreachable_node global.icfg in
  let global = BatSet.fold remove_node unreachable global in
  my_prerr_newline ();
  my_prerr_string "#nodes all   : ";
  my_prerr_endline (string_of_int (List.length nodes_all));
  my_prerr_string "#unreachable : ";
  my_prerr_endline (string_of_int (BatSet.cardinal unreachable));
(*    prerr_string "unreachable nodes : ";
  prerr_endline (string_of_set InterCfg.Node.to_string unreachable); *)
  global

let remove_unreachable_functions : t -> t 
=fun global -> 
  let pids_all = list2set (InterCfg.pidsof global.icfg) in
  let reachable = CallGraph.trans_callees InterCfg.global_proc global.callgraph in
  let unreachable = BatSet.diff pids_all reachable |> BatSet.remove InterCfg.global_proc in
  let recursive = BatSet.filter (fun pid -> is_rec pid global) reachable in
  let global = if !Options.opt_bugfinder >= 2 then global else BatSet.fold remove_function unreachable global in
  my_prerr_newline ();
  my_prerr_endline ("#functions all : " ^ string_of_int (BatSet.cardinal pids_all));
  my_prerr_endline ("#recursive : " ^ string_of_int (BatSet.cardinal recursive));
  my_prerr_endline (string_of_set id recursive);
  my_prerr_endline ("#unreachable   : " ^ string_of_int (BatSet.cardinal unreachable));
  my_prerr_string "unreachable functions : ";
  my_prerr_endline (string_of_set id unreachable);
  global

let init file = 
  { file = file;
    icfg = InterCfg.init file;
    callgraph = CallGraph.empty;
    dump = Dump.empty; 
    mem = ItvDom.Mem.bot } 
  |> remove_unreachable_nodes

let update_icfg icfg global = { global with icfg = icfg }
let update_callgraph callgraph global = { global with callgraph = callgraph }
let update_mem mem global = { global with mem = mem }

let is_undef : InterCfg.pid -> t -> bool = fun pid global ->
  InterCfg.is_undef pid global.icfg

let get_callees : InterCfg.node -> t -> InterCfg.pid list
=fun node t -> 
  BatSet.elements (InterCfg.get_callees node t.icfg) 

let get_leaf_procs : t -> string BatSet.t
=fun global ->
  let icfg = get_icfg global in
  let pids = list2set (InterCfg.pidsof icfg) in
  let cg = get_callgraph global in
  let leafs = BatSet.fold (fun fid -> 
        if BatSet.cardinal (CallGraph.trans_callees fid cg) = 1 
        then BatSet.add fid
        else id) pids BatSet.empty
  in  leafs
 
let to_json : t -> json
= fun g ->
  `Assoc 
      [ ("callgraph", CallGraph.to_json g.callgraph); 
        ("cfgs", InterCfg.to_json g.icfg)
      ]

let print_json : out_channel -> t -> unit 
= fun chan g ->
  Yojson.Safe.pretty_to_channel chan (to_json g)
