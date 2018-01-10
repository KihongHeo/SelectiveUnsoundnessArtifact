open Graph
open Cil
open Global
open BasicDom
open Vocab
open IntraCfg
open InterCfg
open ItvAnalysis
open OctDom
open OctImpactDom
open AlarmExp 
open Report
open ArrayBlk

module Pre = Pre.Make(OctImpactSem)
module DUGraph = Dug.Make(Pre)
module SparseAnalysis = SparseAnalysis.Make(Pre)(OctImpactSem)(OctImpactDom.Table)(DUGraph)
module Table = OctImpactDom.Table 
module Mem = OctImpactDom.Mem

let check pid v1 v2opt v2exp ptrmem mem : (AbsOct.t option * Allocsite.t option * string) list =
  let arr = ItvDom.Val.array_of_val v1 in 
  if ArrayBlk.eq arr ArrayBlk.bot || ArrayBlk.cardinal arr > 1 then 
    Report.check_bo v1 v2opt |> List.map (fun (s, a, d) -> (None, a, d))
  else
    ArrayBlk.foldi (fun a arr lst ->
      let offset = 
        match v2opt with
        | None -> arr.ArrInfo.offset
        | Some v2 -> Itv.plus arr.ArrInfo.offset (ItvDom.Val.itv_of_val v2) in
      let size_loc = OctLoc.of_size a in
      let oct_status =
        try 
          match v2exp, arr.ArrInfo.size with 
           Some (Cil.Lval x), size
         | Some (Cil.BinOp (_, Cil.Lval x, Cil.Const _, _)), size 
            when Itv.lower size >0 && Itv.lower offset >= 0 ->
            let idx_set = ItvSem.eval_lv pid x ptrmem in
            if PowLoc.cardinal idx_set = 1 then
              let idx = PowLoc.choose idx_set |> OctLoc.of_loc in
              let absoct = Mem.lookup mem in
              if AbsOct.check idx size_loc absoct then Some absoct
              else None
            else None
          | _, _ -> None
        with _ -> None
      in
      (oct_status, Some a, Report.string_of_alarminfo offset arr.ArrInfo.size)::lst
    ) arr []

let inspect_aexp : InterCfg.node -> AlarmExp.t -> ItvDom.Mem.t -> Mem.t -> (query * AbsOct.t option) list -> (query * AbsOct.t option) list
=fun node aexp ptrmem mem queries ->
  let pid = InterCfg.Node.get_pid node in
  (match aexp with
  | ArrayExp (lv,e,loc) ->
      let v1 = ItvDom.Mem.lookup (ItvSem.eval_lv (InterCfg.Node.get_pid node) lv ptrmem) ptrmem in
      let v2 = ItvSem.eval (InterCfg.Node.get_pid node) e ptrmem in
      check pid v1 (Some v2) (Some e) ptrmem mem
      |> List.map (fun (status,a,desc) -> 
        match status with 
          Some p -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = Proven; desc = desc }, status) 
        | None -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = UnProven; desc = desc }, status))
  | DerefExp ((Cil.BinOp (op, e1, e2, _)) as e,loc) when op = Cil.PlusPI || op = Cil.IndexPI ->
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e ptrmem in
      check pid v None (Some e2) ptrmem mem
      |> List.map (fun (status,a,desc) -> 
        match status with 
          Some p -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = Proven; desc = desc }, status) 
        | None -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = UnProven; desc = desc }, status))
  | DerefExp (e,loc) -> (* dummy *) 
      let v = ItvSem.eval (InterCfg.Node.get_pid node) e ptrmem in
      check pid v None None ptrmem mem
      |> List.map (fun (status,a,desc) -> 
        match status with 
          Some p -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = Proven; desc = desc }, status) 
        | None -> ({node = node; exp = aexp; loc= loc; allocsite = a; status = UnProven; desc = desc }, status))
  | _ -> []) @ queries

let rec collect : Node.t -> Cmd.t -> (Global.t * ItvDom.Mem.t * Mem.t) ->
  (AlarmExp.t * ItvDom.Mem.t * Mem.t) list
= fun node cmd (global,ptrmem,mem) ->
  match cmd with 
    IntraCfg.Cmd.Cseq cmds ->
      List.fold_left (fun (l,ptrmem,mem,pos) c -> 
        let aexps = collect node cmd (global,ptrmem,mem) in
        let ptrmem = ItvSem.run_cmd ~position:pos AbsSem.Strong node c (ptrmem,global) |> fst in
        let mem = OctImpactSem.run_cmd ~position:pos AbsSem.Strong node c ptrmem (mem,global) in
        (aexps@l,ptrmem,mem,pos+1)) ([],ptrmem,mem,0) cmds 
      |> (fun (l,_,_,_) -> l)
  | _ -> AlarmExp.collect cmd |> List.map (fun x -> (x, ptrmem, mem))

let display_alarms title alarms_part = 
  prerr_endline "";
  prerr_endline ("= " ^ title ^ " =");
  let alarms_part = BatMap.bindings alarms_part in
  let alarms_part = Report.sort_partition alarms_part in
  ignore (List.fold_left (fun k (part_unit, qs) ->
    prerr_string (string_of_int k ^ ". " ^ CilHelper.s_location part_unit ^ " "); 
    prerr_string (string_of_set id (list2set (List.map (fun q -> InterCfg.Node.get_pid q.node) qs)));
(*    prerr_string (" " ^ Report.status_to_string (get_status qs));*)
    prerr_newline ();
    List.iter (fun q ->
      (match q.status with 
         Proven -> prerr_string "  (O)"
       | _ -> prerr_string "  (X)");
      prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
      prerr_string (InterCfg.Node.to_string q.node);
      prerr_string ( ":  " ^ q.desc);
      (match q.allocsite with Some a ->
        prerr_endline ( ", allocsite: " ^ Allocsite.to_string a^" ")
       | _ -> prerr_newline ());
      ) qs;
   k+1
  ) 1 alarms_part) 

let filter : query list -> query list -> query list 
= fun itv_queries oct_queries ->
  List.fold_left2 (fun qs iq oq ->
      match iq.status, oq.status with
        Proven, _ -> qs
      | _, _ -> oq::qs) [] itv_queries oct_queries

let diff : query list -> query list -> query list 
= fun itv_queries oct_queries ->
  List.fold_left2 (fun qs iq oq ->
      match iq.status, oq.status with
        UnProven, Proven -> oq::qs
      | _, _ -> qs) [] itv_queries oct_queries

let print : query list -> query list -> unit
=fun itv_queries oct_queries ->
(*  let itv_proven = get itv_queries Proven in1*)
  filter itv_queries oct_queries
  |> List.filter (fun q -> q.status <> BotAlarm)
  |> partition 
  |> display_alarms "Impact Pre-analysis Results"

let draw_dug (pre, global) = 
  let locset = Pre.get_total_abslocs pre in
  let dug = StepManager.stepf false "Def-use graph construction" DUGraph.make (global, pre, locset) in
  (pre,global,dug)

let generate (global, itvinputof, itv_queries, inputof) = 
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
  let total = List.length nodes in
  list_fold (fun node (qs,k) ->
    prerr_progressbar ~itv:1000 k total;
    let ptrmem = ItvDom.Table.find node itvinputof in
    let mem = Table.find node inputof in
    let cmd = InterCfg.cmdof icfg node in
    let aexps = collect node cmd (global,ptrmem,mem) in
    let qs = list_fold (fun (aexp,ptrmem,mem) ->
      if ptrmem = ItvDom.Mem.bot then id (* dead code *)
      else inspect_aexp node aexp ptrmem mem) aexps qs
    in
    (qs, k+1)
  ) nodes ([],0)
  |> fst


let inspect_alarm (global,itvinputof,inputof,outputof) =
  let itv_queries = Report.generate (global,itvinputof,Report.BO) in
  let (oct_queries, absocts) = generate (global, itvinputof, itv_queries, inputof) |> List.split in
  print itv_queries oct_queries;
  List.fold_left (fun rel aoct -> 
    match aoct with 
      Some p -> Relation.add_absoct p rel
    | None -> rel) Relation.empty absocts
  |> Relation.get_packconf
  |> PackConf.make itvinputof

let init_state (_, global, dug) = 
  OctImpactDom.pack
  |> Mem.init
  |> (fun i -> Table.add InterCfg.start_node i Table.empty)
  |> (fun x -> (global, dug, x))

let do_analysis (global,itvinputof) = 
  global 
  |> Pre.do_preanalysis
  |> draw_dug
  |> init_state
  |> SparseAnalysis.perform ~ptrinfo:itvinputof
  |> (fun (g, i,o) -> (g, itvinputof, i, o))

(* ********** *
 * Marshaling *
 * ********** *)
let marshal_in : Global.t * ItvDom.Table.t -> Global.t * ItvDom.Table.t * Table.t * Table.t 
= fun (global,itvinputof) ->
  let filename = Filename.basename global.file.fileName in
  let global = MarshalManager.input (filename ^ ".octimpact.global") in
  let input = MarshalManager.input (filename ^ ".octimpact.input") in
  let output = MarshalManager.input (filename ^ ".octimpact.output") in
  (global,itvinputof,input,output)

let marshal_out : Global.t * ItvDom.Table.t * Table.t * Table.t -> Global.t * ItvDom.Table.t * Table.t * Table.t
= fun (global,itvinputof,input,output) ->
  let filename = Filename.basename global.file.fileName in
  MarshalManager.output (filename ^ ".octimpact.global") global;
  MarshalManager.output (filename ^ ".octimpact.input") input;
  MarshalManager.output (filename ^ ".octimpact.output") output;
  (global,itvinputof,input,output)



let packing : Global.t * ItvDom.Table.t -> PackConf.t
= fun x ->
  x
  |> StepManager.stepf_cond !Options.opt_marshal_in true "Oct Impact Analysis" marshal_in do_analysis
  |> opt !Options.opt_marshal_out marshal_out
  |> inspect_alarm

