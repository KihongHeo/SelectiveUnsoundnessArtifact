open Graph
open Cil
open Global
open BasicDom
open Vocab
open ArrayBlk
open IntraCfg
open InterCfg
open ItvAnalysis
open OctDom
open AlarmExp 
open Report

module Pre = Pre.Make(OctSem)
module DUGraph = Dug.Make(Pre)
module SparseAnalysis = SparseAnalysis.Make(Pre)(OctSem)(OctDom.Table)(DUGraph)
module Table = OctDom.Table 
module Mem = OctDom.Mem

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

let draw_dug locset (pre, global) = 
  let dug = StepManager.stepf false "Def-use graph construction" DUGraph.make (global, pre, locset) in
  (pre,global,dug)

let init_state locset (_, global, dug) = 
  locset
  |> Mem.top
  |> (fun i -> Table.add InterCfg.start_node i Table.empty)
  |> (fun x -> (global, dug, x))

let string_of_alarminfo offset size diff = 
  "offset: " ^ Itv.to_string offset ^ ", size: " ^ Itv.to_string size ^ ", size - offset: "^ Itv.to_string diff

let check packconf pid v1 v2opt v2exp ptrmem mem : (status * Allocsite.t option * string) list =
  let arr = ItvDom.Val.array_of_val v1 in 
  if ArrayBlk.eq arr ArrayBlk.bot || ArrayBlk.cardinal arr > 1 then Report.check_bo v1 v2opt 
  else
    ArrayBlk.foldi (fun a arr lst ->
      let offset_idx = 
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (ItvDom.Val.itv_of_val v2) in

      let diff =
        match v2exp with 
          None -> Itv.minus arr.ArrInfo.size offset_idx
        | Some e -> OctSem.check_bo pid packconf a arr.ArrInfo.offset e ptrmem mem
      in 
      let status =
        if Itv.is_bot offset_idx || Itv.is_bot arr.ArrInfo.size then BotAlarm
        (* proven by interval *) 
        else if try Itv.lower offset_idx >= 0 && Itv.upper offset_idx < Itv.lower arr.ArrInfo.size with _ -> false then Proven 
        (* proven by octagon *) 
        else if try Itv.lower offset_idx >= 0 && Itv.lower arr.ArrInfo.size > 0 && Itv.lower diff >= 1 with _ -> false then Proven
        else UnProven
      in
      (status, Some a, string_of_alarminfo offset_idx arr.ArrInfo.size diff)::lst
    ) arr []

let inspect_aexp : PackConf.t -> InterCfg.node -> AlarmExp.t -> ItvDom.Mem.t -> Mem.t -> query list -> query list
=fun packconf node aexp ptrmem mem queries ->
  let pid = InterCfg.Node.get_pid node in
  (if !Options.opt_oct_debug then 
  begin
    prerr_endline "query";
    prerr_endline (AlarmExp.to_string aexp)
  end);
  (match aexp with
  | ArrayExp (lv,e,loc) ->
      let v1 = ItvDom.Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv ptrmem) ptrmem in
      let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e ptrmem in
      check packconf pid v1 (Some v2) (Some e) ptrmem mem
      |> List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc })
  | DerefExp (Cil.BinOp (op, e1, e2, _) ,loc) when op = Cil.PlusPI || op = Cil.IndexPI ->
      let v1 = ItvSem.eval (InterCfg.Node.get_pid node) e1 ptrmem in
      let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e2 ptrmem in
      check packconf pid v1 (Some v2) (Some e2) ptrmem mem
      |> List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc })
  | DerefExp (e,loc) -> 
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e ptrmem in
      check packconf pid v None None ptrmem mem
      |> cond (ItvDom.Val.eq ItvDom.Val.bot v) 
          (List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }))
          (List.map (fun (status,a,desc) -> 
                     if status = Report.BotAlarm then 
                        { node = node; exp = aexp; loc = loc; allocsite = a; status = Proven; desc = "valid pointer dereference" }
                     else 
                        { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }))
  | _ -> []) @ queries

let rec collect : PackConf.t -> Node.t -> Cmd.t -> (Global.t * ItvDom.Mem.t * Mem.t) ->
  (AlarmExp.t * ItvDom.Mem.t * Mem.t) list
= fun packconf node cmd (global,ptrmem,mem) ->
  match cmd with 
    IntraCfg.Cmd.Cseq cmds ->
      List.fold_left (fun (l,ptrmem,mem,pos) c -> 
        let aexps = collect packconf node cmd (global,ptrmem,mem) in
        let ptrmem = ItvSem.run_cmd ~position:pos AbsSem.Strong node c (ptrmem,global) |> fst in
        let mem = OctSem.run_cmd ~position:pos AbsSem.Strong packconf node c ptrmem (mem,global) in
        (aexps@l,ptrmem,mem,pos+1)) ([],ptrmem,mem,0) cmds 
      |> (fun (l,_,_,_) -> l)
  | _ -> AlarmExp.collect cmd |> List.map (fun x -> (x, ptrmem, mem))

let generate (global, packconf, itvinputof, inputof) = 
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let ptrmem = ItvDom.Table.find node itvinputof in
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof icfg node in
    let aexps = collect packconf node cmd (global,ptrmem,mem) in
    let qs = list_fold (fun (aexp,ptrmem,mem) ->
      if ptrmem = ItvDom.Mem.bot then id (* dead code *)
      else inspect_aexp packconf node aexp ptrmem mem) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst

let inspect_alarm : Global.t * PackConf.t * ItvDom.Table.t * Table.t * Table.t -> Report.query list
= fun (global,packconf,itvinputof,inputof,_) ->
  generate (global,packconf,itvinputof,inputof) 

(* x = y *)
let sparrow_relation_set pid mem exps rel = 
  match exps with 
    (Cil.Lval x)::(Cil.Lval y)::_ ->
      let lv_x = ItvSem.eval_lv pid x mem in
      let lv_y = ItvSem.eval_lv pid y mem in
      PowLoc.fold (fun x ->
          PowLoc.fold (fun y ->
            OctImpactDom.Relation.add_edge (OctLoc.of_loc x) (OctLoc.of_loc y)) lv_y) lv_x rel
  | _ -> rel

(* x = malloc(y) *)
let sparrow_relation_malloc pid mem exps rel = 
  match exps with 
    x::(Cil.Lval y)::_ ->
      let lv_x = ItvSem.eval pid x mem |> ItvDom.Val.allocsites_of_val in
      let lv_y = ItvSem.eval_lv pid y mem in
      BatSet.fold (fun x ->
          PowLoc.fold (fun y ->
            OctImpactDom.Relation.add_edge (OctLoc.of_size x) (OctLoc.of_loc y)) lv_y) lv_x rel
  | _ -> rel
       
(* x = strlen(y) *)
let sparrow_relation_strlen pid mem exps rel = 
  match exps with 
    (Cil.Lval x)::y::_ ->
      let lv_x = ItvSem.eval_lv pid x mem in
      let lv_y = ItvSem.eval pid y mem |> ItvDom.Val.allocsites_of_val in
      PowLoc.fold (fun x ->
          BatSet.fold (fun y ->
            OctImpactDom.Relation.add_edge (OctLoc.of_loc x) (OctLoc.of_size y)) lv_y) lv_x rel
  | _ -> rel

let manual_packing : Global.t * ItvDom.Table.t -> PackConf.t
= fun (global, itvinputof) ->
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
  list_fold (fun n a -> 
      let mem = ItvDom.Table.find n itvinputof in
      let pid = InterCfg.Node.get_pid n in
      match InterCfg.cmdof icfg n with 
      | IntraCfg.Cmd.Ccall (None, Cil.Lval (Cil.Var f, Cil.NoOffset), exps, _) -> 
        if f.vname = "sparrow_relation_set" then sparrow_relation_set pid mem exps a
        else if f.vname = "sparrow_relation_malloc" then sparrow_relation_malloc pid mem exps a
        else if f.vname = "sparrow_relation_strlen" then sparrow_relation_strlen pid mem exps a
        else a 
      | _ -> a
  ) nodes OctImpactDom.Relation.empty
  |> OctImpactDom.Relation.get_packconf
  |> PackConf.make itvinputof 

let do_analysis : Global.t * ItvDom.Table.t * ItvDom.Table.t -> Global.t * PackConf.t * ItvDom.Table.t * Table.t * Table.t
= fun (global, itvinputof, _) ->
  let packconf = StepManager.stepf_switch true "Compute Packing Configuration" 
      [(!Options.opt_pack_manual, manual_packing); 
       (!Options.opt_pack_impact, OctImpactAnalysis.packing)] (global, itvinputof)
  in
  PackConf.print_info packconf;
  global
  |> (fun x -> Options.opt_oct_debug := false; x)
  |> Pre.do_preanalysis ~locset:packconf ~ptrinfo:itvinputof
  |> draw_dug packconf
  |> print_dug 
  |> init_state packconf
  |> SparseAnalysis.perform ~locset:packconf ~ptrinfo:itvinputof
  |> (fun (g, i,o) -> (g, packconf, itvinputof, i, o))


(* ********** *
 * Marshaling *
 * ********** *)

let marshal_in : Global.t * ItvDom.Table.t * ItvDom.Table.t 
  -> Global.t * PackConf.t * ItvDom.Table.t * Table.t * Table.t 
= fun (global,itvinputof,_) ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".oct.global") in
  let packconf = MarshalManager.input (filename ^ ".oct.packconf") in
  let input = MarshalManager.input (filename ^ ".oct.input") in
  let output = MarshalManager.input (filename ^ ".oct.output") in
  (global,packconf,itvinputof,input,output)

let marshal_out : Global.t * PackConf.t * ItvDom.Table.t * Table.t * Table.t 
  -> Global.t * PackConf.t * ItvDom.Table.t * Table.t * Table.t
= fun (global,packconf,itvinputof,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".oct.global") global;
  MarshalManager.output (filename ^ ".oct.packconf") packconf;
  MarshalManager.output (filename ^ ".oct.input") input;
  MarshalManager.output (filename ^ ".oct.output") output;
  (global,packconf,itvinputof,input,output)
