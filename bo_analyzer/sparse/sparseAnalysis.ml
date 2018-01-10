open Vocab
open BasicDom
open AbsSem
open Pre
open Dug

let total_iterations = ref 0
let g_clock = ref 0.0

module Make(Pre:Pre.S)
           (MainSem:AbsSem.S with type Dom.A.t = Pre.Access.Loc.t)
           (Table:MapDom.S with type A.t = Node.t and type B.t = MainSem.Dom.t)
           (DUGraph:Dug.S with type vertex = Table.A.t and type loc = Pre.Access.Loc.t)
=
struct

  module Mem = MainSem.Dom
  module Worklist = Worklist.Make(DUGraph)

  let needwidening : DUGraph.vertex -> Worklist.t -> bool
  =fun idx wl -> Worklist.is_loopheader idx wl

  let def_locs_cache = Hashtbl.create 251
  let get_def_locs idx dug =
    try Hashtbl.find def_locs_cache idx with Not_found ->
    let def_locs =
      let union_locs succ = BatSet.union (DUGraph.get_abslocs idx succ dug) in
      list_fold union_locs (DUGraph.succ idx dug) BatSet.empty in
    Hashtbl.add def_locs_cache idx def_locs; def_locs

  let print_iteration () = 
    total_iterations := !total_iterations + 1;
    if !total_iterations = 1 then g_clock := Sys.time()
    else if !total_iterations mod 10000 = 0
    then 
    begin
      my_prerr_endline ("#iters: "^(string_of_int !total_iterations)
                        ^" took "^string_of_float (Sys.time()-.(!g_clock)));
      my_prerr_endline ""; flush stderr;
      g_clock := Sys.time ();
    end

  let propagate dug idx (works,inputof,outputof) (unstables,new_output,global)= 
    let (works, inputof) =
      let update_succ succ (works, inputof) =
        let old_input = Table.find succ inputof in
        let locs_on_edge = DUGraph.get_abslocs idx succ dug in
        let is_on_edge (x, _) = DUGraph.mem_duset x locs_on_edge in
        let to_join = List.filter is_on_edge unstables in
        if to_join = [] then (works, inputof) 
        else
          let new_input = Mem.join_pairs to_join old_input in
          (Worklist.push idx succ works, Table.add succ new_input inputof) 
      in
      let succs = DUGraph.succ idx dug in
      list_fold update_succ succs (works, inputof) 
    in
    (works, global, inputof, Table.add idx new_output outputof)

  let get_unstable dug idx works old_output (new_output, global) =
    let def_locs = Profiler.event "SparseAnalysis.widening_get_def_locs" get_def_locs idx dug in
    let is_unstb v1 v2 = not (Mem.B.le v2 v1) in
    let u = Profiler.event "SparseAnalysis.widening_unstable" Mem.unstables old_output new_output is_unstb def_locs in
    if u = [] then None
    else
      let op = if needwidening idx works then Mem.B.widen else (fun _ y -> y) in
      let _ = Profiler.start_event "SparseAnalysis.widening_new_output" in
      let u = List.map (fun (k, v1, v2) -> (k, op v1 v2)) u in
      let new_output = list_fold (fun (k, v) -> Mem.add k v) u old_output in
      let _ = Profiler.finish_event "SparseAnalysis.widening_new_output" in
      (* update unstable locations's values by widened values *)
      let u = List.map (fun (k, _) -> (k, Mem.find k new_output)) u in
      Some (u, new_output, global)

  let prdbg_input : Node.t -> (Mem.t * Global.t) -> (Mem.t * Global.t) 
  = fun node (mem, global) -> 
    prerr_endline (Node.to_string node);
    prerr_endline (IntraCfg.Cmd.to_string (InterCfg.cmdof (Global.get_icfg global) node));
    prerr_endline "== Input ==";
    prerr_endline (Mem.to_string mem);
    (mem, global)

  let prdbg_output : Mem.t -> (Mem.t * Global.t) -> (Mem.t * Global.t) 
  = fun old_output (new_output, global) ->
    prerr_endline "== Old Output ==";
    prerr_endline (Mem.to_string old_output);
    prerr_endline "== New Output ==";
    prerr_endline (Mem.to_string new_output);
    (new_output, global)

  (* fixpoint iterator specialized to the widening phase *)
  let analyze_node ?locset ?ptrinfo : DUGraph.t -> DUGraph.vertex
    -> (Worklist.t * Global.t * Table.t * Table.t)
    -> (Worklist.t * Global.t * Table.t * Table.t)
  = fun dug idx (works, global, inputof, outputof) ->
    print_iteration ();
    let old_output = Table.find idx outputof in
    (Table.find idx inputof, global)
    |> opt !Options.opt_debug (prdbg_input idx)
    |> Profiler.event "SparseAnalysis.run" (MainSem.run ~mode:Strong ?locset ?ptrinfo idx)
    |> opt !Options.opt_debug (prdbg_output old_output)
    |> Profiler.event "SparseAnalysis.get_unstable" get_unstable dug idx works old_output
    &> Profiler.event "SparseAnalysis.propagating" propagate dug idx (works,inputof,outputof)
    |> (function None -> (works, global, inputof, outputof) | Some x -> x)


  (* fixpoint iterator that can be used in both widening and narrowing phases *)
  let analyze_node_with_otable (widen,join,le) : DUGraph.t -> 
    DUGraph.vertex
    -> (Worklist.t * Global.t * Table.t * Table.t)
    -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun dug idx (works, global, inputof, outputof) ->
    print_iteration ();                                                  
    let pred = DUGraph.pred idx dug in
    let input = List.fold_left (fun m p ->
          let pmem = Table.find p outputof in
          let locs_on_edge = DUGraph.get_abslocs p idx dug in 
          BatSet.fold (fun l m -> 
              let v1 = Mem.find l pmem in
              let v2 = Mem.find l m in
              Mem.add l (Mem.B.join v1 v2) m) locs_on_edge m
          ) Mem.bot pred in
    let inputof = Table.add idx input inputof in
    let old_output = Table.find idx outputof in
    (input, global)
    |> Profiler.event "SparseAnalysis.run" (MainSem.run ~mode:Strong idx)
    |> Profiler.event "SparseAnalysis.get_unstable" get_unstable dug idx works old_output
    |> (function None -> (works, global, inputof, outputof) 
        | Some (_,new_output,global) ->
          let works = Worklist.push_set idx (BatSet.of_list (DUGraph.succ idx dug)) works in
          (works, global, inputof, Table.add idx new_output outputof))


  let rec iterate f : DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t) 
     -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun dug (works, global, inputof, outputof) ->
    match Worklist.pick works with
    | None -> (works, global, inputof, outputof)
    | Some (idx, rest) -> 
      (rest, global, inputof, outputof) 
      |> f dug idx
      |> iterate f dug

  let widening ?locset ?ptrinfo : DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t)
      -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun dug (worklist, global, inputof, outputof) ->
    total_iterations := 0;
    worklist
    |> Worklist.push_set InterCfg.start_node (DUGraph.nodesof dug)
    |> (fun init_worklist -> iterate (analyze_node ?locset ?ptrinfo) dug (init_worklist, global, inputof, outputof))
    |> (fun x -> my_prerr_endline ("#iteration in widening : " ^ string_of_int !total_iterations); x)
 
  let narrowing ?(initnodes=BatSet.empty) : DUGraph.t -> (Worklist.t * Global.t * Table.t * Table.t) 
      -> (Worklist.t * Global.t * Table.t * Table.t)
  =fun dug (worklist, global, inputof, outputof) ->
    total_iterations := 0;
    worklist
    |> Worklist.push_set InterCfg.start_node (if (BatSet.is_empty initnodes) then DUGraph.nodesof dug else initnodes)
    |> (fun init_worklist -> iterate (analyze_node_with_otable (Mem.B.narrow, Mem.B.join, fun x y -> Mem.B.le y x)) 
        dug (init_worklist, global, inputof, outputof))
    |> (fun x -> my_prerr_endline ("#iteration in narrowing : " ^ string_of_int !total_iterations); x)

  let perform ?(locset=BatSet.empty) ?(ptrinfo=ItvDom.Table.bot) : Global.t * DUGraph.t * Table.t -> Global.t * Table.t * Table.t
  =fun (global, dug, inputof) -> 
    let worklist = StepManager.stepf false "Workorder computation" Worklist.init dug in
    (worklist,global,inputof,Table.empty) 
    |> StepManager.stepf false "Fixpoint iteration with widening" (widening dug ~locset:locset ~ptrinfo:ptrinfo)
    |> StepManager.stepf_opt !Options.opt_narrow false "Fixpoint iteration with narrowing" (narrowing dug)
    |> (fun (_,global,inputof,outputof) -> (global, inputof, outputof))
end
