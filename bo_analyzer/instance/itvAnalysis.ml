open Graph
open Cil
open Global
open BasicDom
open Vocab
open Frontend
open ItvDom
open ItvSem

module ItvPre = Pre.Make(ItvSem)
module DUGraph = Dug.Make(ItvPre)
module ItvSparseAnalysis = SparseAnalysis.Make(ItvPre)(ItvSem)(Table)(DUGraph)

let print_abslocs_info locs = 
  let lvars = BatSet.filter Loc.is_lvar locs in
  let gvars = BatSet.filter Loc.is_gvar locs in
  let allocsites = BatSet.filter Loc.is_allocsite locs in
  let fields = BatSet.filter Loc.is_field locs in
    prerr_endline ("#abslocs    : " ^ i2s (BatSet.cardinal locs));
    prerr_endline ("#lvars      : " ^ i2s (BatSet.cardinal lvars));
    prerr_endline ("#gvars      : " ^ i2s (BatSet.cardinal gvars));
    prerr_endline ("#allocsites : " ^ i2s (BatSet.cardinal allocsites));
    prerr_endline ("#fields     : " ^ i2s (BatSet.cardinal fields))

let draw_dug (pre, global) = 
  let locset = ItvPre.get_total_abslocs pre in
  let dug = StepManager.stepf false "Def-use graph construction" DUGraph.make (global, pre, locset) in
  (pre,global,dug)

let print_dug (pre,global,dug) = 
  if !Options.opt_dug then 
  begin
    `Assoc 
      [ ("callgraph", CallGraph.to_json global.callgraph); 
        ("cfgs", InterCfg.to_json global.icfg);
        ("dugraph", DUGraph.to_json_intra dug pre);
        ("dugraph-inter", DUGraph.to_json_inter dug pre);
      ]
    |> Yojson.Safe.pretty_to_channel stdout;
    exit 0
  end
  else
  begin
    prerr_memory_usage ();
    prerr_endline ("#Nodes in def-use graph : " ^ i2s (BatSet.cardinal (DUGraph.nodesof dug)));
    prerr_endline ("#Locs on def-use graph : " ^ i2s (DUGraph.sizeof dug));
    (pre,global,dug)
  end

let do_sparse_analysis global = 
  let _ = prerr_memory_usage () in
  global 
  |> ItvPre.do_preanalysis 
  |> draw_dug
  |> print_dug 
  |> (fun (_,global,dug) -> (global,dug,Table.empty))
  |> ItvSparseAnalysis.perform  

let inspect_alarm (global, inputof, _) = 
  (if !Options.opt_bo then 
      StepManager.stepf true "Generate Buffer overrun report" Report.generate (global,inputof,Report.BO)
   else [])
  @
  (if !Options.opt_nd then
      StepManager.stepf true "Generate Null dereference report" Report.generate (global,inputof,Report.ND)
   else [])
  @
  (if !Options.opt_dz then 
      StepManager.stepf true "Generate Divide by zero report" Report.generate (global,inputof,Report.DZ)
   else [])

(* ********** *
 * Marshaling *
 * ********** *)

let marshal_in : Global.t -> Global.t * Table.t * Table.t 
= fun global ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".itv.global") in
  let input = MarshalManager.input (filename ^ ".itv.input") in
  let output = MarshalManager.input (filename ^ ".itv.output") in
  (global,input,output)

let marshal_out : Global.t * Table.t * Table.t -> Global.t * Table.t * Table.t
= fun (global,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".itv.global") global;
  MarshalManager.output (filename ^ ".itv.input") input;
  MarshalManager.output (filename ^ ".itv.output") output;
  (global,input,output)
