open Graph
open Cil
open Global
open Vocab

let do_inline global = 
  if Frontend.inline global then (* something inlined *)
    Frontend.makeCFGinfo global.file       (* NOTE: CFG must be re-computed after inlining *)
    |> StepManager.stepf true "Translation to graphs (after inline)" Global.init
    |> StepManager.stepf true "Pre-analysis (after inline)" ItvSem.do_preanalysis 
    |> snd
  else global (* nothing inlined *)
 
let init_analysis : Cil.file -> Global.t
= fun file ->
  file
  |> StepManager.stepf true "Translation to graphs" Global.init 
  |> StepManager.stepf true "Pre-analysis" ItvSem.do_preanalysis 
  |> snd 
  |> do_inline

let print_pgm_info : Global.t -> Global.t 
= fun global ->
  let pids = InterCfg.pidsof (Global.get_icfg global) in
  let nodes = InterCfg.nodesof (Global.get_icfg global) in
  prerr_endline ("#Procs : " ^ string_of_int (List.length pids));
  prerr_endline ("#Nodes : " ^ string_of_int (List.length nodes));
  global
 
let print_il global =
  Cil.dumpFile !Cil.printerForMaincil stdout "" global.file; 
  exit 0

let print_cfg global =
  `Assoc 
    [ ("callgraph", CallGraph.to_json global.callgraph); 
      ("cfgs", InterCfg.to_json global.icfg)]
  |> Yojson.Safe.pretty_to_channel stdout; exit 0

let finish t0 () = 
  prerr_endline "Finished properly.";
  Profiler.report stdout;
  prerr_endline (string_of_float (Sys.time () -. t0))

let octagon_analysis : Global.t * ItvDom.Table.t * ItvDom.Table.t -> Report.query list 
= fun x -> 
  StepManager.stepf true "Oct Sparse Analysis" OctAnalysis.do_analysis x
  |> OctAnalysis.inspect_alarm

let main () =
  let t0 = Sys.time () in
  let _ = Profiler.start_logger () in

  let usageMsg = "Usage: main.native [options] source-files" in

  Printexc.record_backtrace true;

  (* process arguments *)
  Arg.parse Options.opts Frontend.args usageMsg;
  List.iter (fun f -> prerr_string (f ^ " ")) !Frontend.files;
  prerr_endline "";

  BatSet.iter (fun x -> prerr_string (x ^ ", ")) !Options.opt_unsound_global;

  Cil.initCIL ();

  try 
    StepManager.stepf true "Front-end" Frontend.parse ()
    |> Frontend.makeCFGinfo
    |> UnsoundLoop.remove_loop 
    |> init_analysis
    |> print_pgm_info
    |> opt !Options.opt_magic (fun x -> 
        UnsoundGlobal.extract_global x.mem;
        exit(0);
        x)
(*
    |> opt !Options.opt_magic (fun x -> ItvDom.Mem.iter (fun l _ -> 
        if BatSet.mem (BasicDom.Loc.to_string l) !Options.opt_unsound_update then
          prerr_endline (BasicDom.Loc.to_string l)
        else ()) x.mem; prerr_endline "test"; exit(0); x)
*)
    |> opt !Options.opt_il print_il
    |> opt !Options.opt_cfg print_cfg
    |> StepManager.stepf_cond !Options.opt_marshal_in true "Itv Sparse Analysis" ItvAnalysis.marshal_in ItvAnalysis.do_sparse_analysis
    |> opt !Options.opt_marshal_out ItvAnalysis.marshal_out
    |> cond !Options.opt_oct octagon_analysis ItvAnalysis.inspect_alarm
    |> Report.print !Options.opt_noalarm
    |> finish t0
  with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace())

let _ = main ()
