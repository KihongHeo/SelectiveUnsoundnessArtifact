(* Access Pre-Analysis Framework *)
open Vocab
open Global
open BasicDom
open AbsSem
open Access
open ItvSem
open ItvDom

module type S = 
sig
  module Access : ACCESS
  type t
  val empty : t
  val get_total_abslocs : t -> Access.Loc.t BatSet.t
  val get_access : t -> Node.t -> Access.t
  val get_access_proc : t -> Proc.t -> Access.t
  val get_access_reach : t -> Proc.t -> Access.t
  val get_access_local : t -> Proc.t -> Access.Loc.t BatSet.t
  val get_access_local_program : t -> Access.Loc.t BatSet.t
  val get_defs_of : t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  val get_uses_of : t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  val get_single_defs : (Access.Loc.t, Node.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  val restrict_access   : t -> Access.Loc.t BatSet.t -> t
  val do_preanalysis : ?locset:Access.Loc.t BatSet.t -> ?ptrinfo : ItvDom.Table.t -> Global.t -> t * Global.t
end

module Make(Sem : AbsSem.S)  =
struct
  module Access = Sem.Access
  module Loc = Access.Loc
  module Mem = Sem.Dom
  type t = {
    mem : Mem.t;
    total_abslocs : Access.Loc.t BatSet.t;
    access : (Node.t, Access.t) BatMap.t;
    access_proc : (Proc.t, Access.t) BatMap.t;
    access_reach : (Proc.t, Access.t) BatMap.t;
    access_local_proc : (Proc.t, Access.Loc.t BatSet.t) BatMap.t;
    access_local_program : Access.Loc.t BatSet.t;
    defs_of : (Access.Loc.t, Node.t BatSet.t) BatMap.t;
    uses_of : (Access.Loc.t, Node.t BatSet.t) BatMap.t
  }

  let empty = {
    mem = Mem.bot;
    total_abslocs = BatSet.empty;
    access = BatMap.empty;
    access_proc = BatMap.empty;
    access_reach = BatMap.empty;
    access_local_proc = BatMap.empty;
    access_local_program = BatSet.empty;
    defs_of = BatMap.empty;
    uses_of = BatMap.empty
  }

  let get_total_abslocs : t -> Loc.t BatSet.t
  =fun i -> i.total_abslocs

  let get_access : t -> Node.t -> Access.t
  =fun i n -> try BatMap.find n i.access with _ -> Access.empty

  let get_access_proc : t -> Proc.t -> Access.t
  =fun i pid -> try BatMap.find pid i.access_proc with _ -> Access.empty

  (* abstract locations exclusively accessed in the given function *)
  let get_access_local : t -> Proc.t -> Loc.t BatSet.t
  =fun i pid -> try BatMap.find pid i.access_local_proc with _ -> BatSet.empty

  let get_access_local_program : t -> Loc.t BatSet.t
  =fun i -> i.access_local_program

  let get_access_reach : t -> Proc.t -> Access.t
  =fun i pid -> try BatMap.find pid i.access_reach with _ -> Access.empty

  let get_defs_of : t -> (Loc.t, Node.t BatSet.t) BatMap.t
  =fun t -> t.defs_of

  let get_uses_of : t -> (Loc.t, Node.t BatSet.t) BatMap.t
  =fun t -> t.uses_of

  let restrict : Loc.t BatSet.t -> (Node.t, Access.t) BatMap.t -> (Node.t, Access.t) BatMap.t
  =fun abslocs map ->
    BatMap.map (fun access -> Access.restrict abslocs access) map

  let init_access ?(locset=BatSet.empty) ?(ptrinfo=ItvDom.Table.bot): Global.t -> Mem.t
    -> Access.Loc.t BatSet.t * (Node.t, Access.t) BatMap.t
  =fun global mem -> 
    let nodes = InterCfg.nodesof (Global.get_icfg global) in
    let map =
      list_fold (fun node ->
        BatMap.add node (Sem.accessof ~mode:Strong ~locset:locset ~ptrinfo:ptrinfo global node mem)
      ) nodes BatMap.empty in
    let abslocs = BatMap.foldi (fun _ access acc ->
        let acc = set_union_small_big (Access.useof access) acc in
       set_union_small_big (Access.defof access) acc) map BatSet.empty in
    (abslocs, restrict abslocs map)


  let filter_out_access : (Node.t, Access.t) BatMap.t -> Access.Loc.t BatSet.t -> (Node.t, Access.t) BatMap.t
  =fun access locs ->  
    BatMap.foldi (fun node access ->
      BatMap.add node (Access.filter_out locs access) 
    ) access BatMap.empty

  let restrict_access : t -> Access.Loc.t BatSet.t -> t
  =fun t locs ->  
    { t with access = 
        BatMap.foldi (fun node access ->
          BatMap.add node (Access.restrict locs access) 
        ) t.access BatMap.empty }

  let init_access_proc : (Node.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  =fun access ->
    let add_access_proc pid access m =
      BatMap.modify_def Access.empty pid (Access.union access) m in
    let add_access_node node access m =
      add_access_proc (Node.get_pid node) access m in
    BatMap.foldi add_access_node access BatMap.empty

  let init_access_reach : InterCfg.pid list -> CallGraph.t
    -> (Proc.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  = fun pids callgraph access_proc ->
    list_fold (fun pid ->
        let trans = CallGraph.trans_callees pid callgraph in
        Access.empty 
        |> PowProc.fold (fun callee -> 
             Access.union (try BatMap.find callee access_proc with _ -> Access.empty)) trans
        |> Access.union (try BatMap.find pid access_proc with _ -> Access.empty)
        |> BatMap.add pid) pids BatMap.empty
 
  let init_access_local_proc : (Proc.t, Access.t) BatMap.t
      -> (Proc.t, Access.Loc.t BatSet.t) BatMap.t
  = fun access_proc ->
    let update_loc2proc_1 pid loc (loc2proc, nonlocals) =
      if BatMap.mem loc loc2proc then
        let loc2proc = BatMap.remove loc loc2proc in
        let nonlocals = BatSet.add loc nonlocals in
        (loc2proc, nonlocals)
      else
        let loc2proc = BatMap.add loc pid loc2proc in
        (loc2proc, nonlocals) in
    let update_loc2proc pid acc_of_pid (loc2proc, nonlocals) =
      let locs = BatSet.diff (Access.accessof acc_of_pid) nonlocals in
      BatSet.fold (update_loc2proc_1 pid) locs (loc2proc, nonlocals) in
    let (loc2proc, _) =
      BatMap.foldi update_loc2proc access_proc (BatMap.empty, BatSet.empty) in
    let proc2locs = BatMap.map (fun _ -> BatSet.empty) access_proc in
    let add_loc_pid loc pid = BatMap.modify pid (BatSet.add loc) in
    BatMap.foldi add_loc_pid loc2proc proc2locs

  let init_access_local_program : (Proc.t, Access.Loc.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  =fun access_local_proc ->
    BatMap.foldi (fun proc access -> BatSet.union access) access_local_proc BatSet.empty

  let init_defs_of : (Node.t, Access.t) BatMap.t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  =fun access_map ->
    BatMap.foldi (fun node access defs_of ->
      BatSet.fold (fun loc defs_of ->
        let old_nodes = try BatMap.find loc defs_of with _ -> BatSet.empty
        in  BatMap.add loc (BatSet.add node old_nodes) defs_of
      ) (Access.defof access) defs_of
    ) access_map BatMap.empty

  let init_uses_of : (Node.t, Access.t) BatMap.t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  =fun access_map ->
    BatMap.foldi (fun node access uses_of ->
      BatSet.fold (fun loc uses_of ->
        let old_nodes = try BatMap.find loc uses_of with _ -> BatSet.empty
        in  BatMap.add loc (BatSet.add node old_nodes) uses_of
      ) (Access.useof access) uses_of
    ) access_map BatMap.empty

  let get_single_defs : (Access.Loc.t, Node.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  =fun defs_of -> 
    BatMap.foldi (fun loc nodes -> 
      if BatSet.cardinal nodes = 1 then BatSet.add loc else id
    ) defs_of BatSet.empty

  let do_preanalysis ?(locset=BatSet.empty) ?(ptrinfo=Table.empty) : Global.t -> t * Global.t
  =fun global ->
    let pids = InterCfg.pidsof (get_icfg global) in
    let (mem,_) = Sem.do_preanalysis ~locset:locset ~ptrinfo:ptrinfo global in
    let (total_abslocs, access) = init_access ~locset:locset ~ptrinfo:ptrinfo global mem in
    let _ = my_prerr_endline ("#total abstract locations  = " ^ string_of_int (BatSet.cardinal total_abslocs)) in
    let access_proc = init_access_proc access in
    let access_local_proc = init_access_local_proc access_proc in
    let access_local_program = init_access_local_program access_local_proc in
    let info = { mem = mem;
                 total_abslocs = total_abslocs;
                 access = access;
                 access_proc = access_proc;
                 access_reach = init_access_reach pids global.callgraph access_proc;
                 access_local_proc = access_local_proc;
                 access_local_program = access_local_program;
                 defs_of = init_defs_of access;
                 uses_of = init_uses_of access;
                 } in

    (info, global)
end
