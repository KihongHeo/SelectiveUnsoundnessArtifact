open DPow
open Datatypes
open InterCfg
open InterNode
open IntraCfg
open IntraNode
open List0
open Syn
open TStr
open UserInputType
open VocabA

(** val fail_cmd_mem : pid_t -> t -> bool **)

let fail_cmd_mem = fun f node ->
prerr_string "fail on ";
PPVocab.pp_string PPIL.print_inter_node (f, node);
prerr_endline " by unsound abstract memory";
false

(** val fail_access : pid_t -> t -> bool **)

let fail_access = fun f node ->
prerr_string "fail on ";
PPVocab.pp_string PPIL.print_inter_node (f, node);
prerr_endline " by unsound accessibility";
false

(** val fail_dump : pid_t -> t -> bool **)

let fail_dump = fun f node ->
prerr_string "fail on ";
PPVocab.pp_string PPIL.print_inter_node (f, node);
prerr_endline " by unsound dump";
false

(** val fail_intra_edge : pid_t -> t -> t -> bool **)

let fail_intra_edge = fun f from_node to_node ->
prerr_string "fail on edge ";
PPVocab.pp_string PPIL.print_inter_node (f, from_node);
prerr_string "->";
PPVocab.pp_endline PPIL.print_inter_node (f, to_node);
false

(** val fail_inter_edge : InterNode.t -> InterNode.t -> bool **)

let fail_inter_edge = fun from_node to_node ->
prerr_string "fail on edge ";
PPVocab.pp_string PPIL.print_inter_node from_node;
prerr_string "->";
PPVocab.pp_endline PPIL.print_inter_node to_node;
false

(** val fail_inter_edge_invalid : InterNode.t -> InterNode.t -> bool **)

let fail_inter_edge_invalid = fun from_node to_node ->
prerr_string "fail on edge ";
PPVocab.pp_string PPIL.print_inter_node from_node;
prerr_string "->";
PPVocab.pp_string PPIL.print_inter_node to_node;
prerr_endline " by cmd invalidity";
false

(** val fail_inter_edge_no_callof : InterNode.t -> InterNode.t -> bool **)

let fail_inter_edge_no_callof = fun from_node to_node ->
prerr_string "fail on edge ";
PPVocab.pp_string PPIL.print_inter_node from_node;
prerr_string "->";
PPVocab.pp_string PPIL.print_inter_node to_node;
prerr_endline " by call node not found";
false

module Make = 
 functor (I:INPUT) ->
 struct 
  (** val get_access : pid_t -> I.access_map -> I.Acc.t **)
  
  let get_access pid m =
    default I.Acc.empty (PidMap.find pid m)
  
  (** val get_def_access : pid_t -> I.access_map -> I.PowLoc.t **)
  
  let get_def_access pid m =
    I.Acc.defof (get_access pid m)
  
  (** val get_use_access : pid_t -> I.access_map -> I.PowLoc.t **)
  
  let get_use_access pid m =
    I.Acc.useof (get_access pid m)
  
  (** val get_all_access : pid_t -> I.access_map -> I.PowLoc.t **)
  
  let get_all_access pid m =
    I.Acc.accessof (get_access pid m)
  
  (** val mem_restrict : I.PowLoc.t -> I.Mem.t -> I.Mem.t **)
  
  let mem_restrict ls m =
    I.Mem.filteri (fun k x -> I.PowLoc.coq_ISet.set_mem k ls) m
  
  (** val mem_removes : I.PowLoc.t -> I.Mem.t -> I.Mem.t **)
  
  let mem_removes ls m =
    I.Mem.filteri (fun k x -> negb (I.PowLoc.coq_ISet.set_mem k ls)) m
  
  (** val icfg : I.G.t -> InterCfg.t **)
  
  let icfg g =
    I.G.icfg g
  
  (** val dump : I.G.t -> I.Dump.t **)
  
  let dump g =
    I.G.dump g
  
  (** val valid_cmd :
      I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t
      -> t -> cmd -> bool **)
  
  let valid_cmd g fs_locs access_map0 inputof outputof f node cmd0 =
    let i_m = I.Table.find (f, node) inputof in
    let o_m = I.Table.find (f, node) outputof in
    let access_f = get_access f access_map0 in
    let p = ((Strong, ValiPhase), fs_locs) in
    let (p0, access_f') = I.run_access p (f, node) cmd0 (i_m, g) in
    let (o_m', g') = p0 in
    let dump' = I.G.dump g' in
    if I.Dump.le_dec dump' (dump g)
    then if I.Acc.le_dec access_f' access_f
         then if I.Mem.strong_le o_m' o_m then true else fail_cmd_mem f node
         else fail_access f node
    else fail_dump f node
  
  (** val valid_cmds :
      I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t
      -> IntraCfg.t -> bool **)
  
  let valid_cmds g fs_locs access_map0 inputof outputof f cfg =
    NodeMap.for_all print2
      (valid_cmd g fs_locs access_map0 inputof outputof f) cfg.cmds
  
  (** val valid_intra_edge :
      I.Table.t -> I.Table.t -> pid_t -> IntraCfg.t -> t -> t -> bool **)
  
  let valid_intra_edge inputof outputof f cfg from_node to_node =
    let from_m = I.Table.find (f, from_node) outputof in
    let to_m = I.Table.find (f, to_node) inputof in
    if is_call_node cfg from_node
    then true
    else if I.Mem.strong_le from_m to_m
         then true
         else fail_intra_edge f from_node to_node
  
  (** val valid_intra_edges :
      I.Table.t -> I.Table.t -> pid_t -> IntraCfg.t -> bool **)
  
  let valid_intra_edges inputof outputof f cfg =
    let valid_intra_edges' = fun from_node to_nodes ->
      NodeSet.for_all (valid_intra_edge inputof outputof f cfg from_node)
        to_nodes
    in
    NodeMap.for_all print2 valid_intra_edges' cfg.succ
  
  (** val valid_cfg :
      I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> pid_t
      -> IntraCfg.t -> bool **)
  
  let valid_cfg g fs_locs access_map0 inputof outputof f cfg =
    (&&) (valid_cmds g fs_locs access_map0 inputof outputof f cfg)
      (valid_intra_edges inputof outputof f cfg)
  
  (** val valid_cfgs :
      I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> bool **)
  
  let valid_cfgs g fs_locs access_map0 inputof outputof =
    PidMap.for_all print2 (valid_cfg g fs_locs access_map0 inputof outputof)
      (icfg g).cfgs
  
  (** val valid_inter_edge :
      I.G.t -> I.access_map -> I.Table.t -> I.Table.t -> InterNode.t ->
      InterNode.t -> bool **)
  
  let valid_inter_edge g access_map0 inputof outputof from_node to_node =
    let from_m = I.Table.find from_node outputof in
    let to_m = I.Table.find to_node inputof in
    if InterCfg.is_call_node (icfg g) from_node
    then let callee = get_pid to_node in
         let access = get_all_access callee access_map0 in
         let from_m0 = mem_restrict access from_m in
         if I.Mem.strong_le from_m0 to_m
         then true
         else fail_inter_edge from_node to_node
    else if I.G.is_exit_node from_node
         then let callee = get_pid from_node in
              let def = get_def_access callee access_map0 in
              (match InterCfg.callof (icfg g) to_node with
               | Some call_node ->
                 let caller_m = I.Table.find call_node outputof in
                 let caller_m0 = mem_removes def caller_m in
                 let from_m0 = mem_restrict def from_m in
                 if I.Mem.strong_le from_m0 to_m
                 then if I.Mem.strong_le caller_m0 to_m
                      then true
                      else fail_inter_edge call_node to_node
                 else fail_inter_edge from_node to_node
               | None -> fail_inter_edge_no_callof from_node to_node)
         else fail_inter_edge_invalid from_node to_node
  
  (** val valid_inter_edges :
      I.G.t -> I.access_map -> I.Table.t -> I.Table.t -> bool **)
  
  let valid_inter_edges g access_map0 inputof outputof =
    let valid_inter_edges' = fun from_node to_nodes ->
      InterCfg.NodeSet.for_all
        (valid_inter_edge g access_map0 inputof outputof from_node) to_nodes
    in
    InterCfg.NodeMap.for_all print2 valid_inter_edges' (icfg g).InterCfg.succ
  
  (** val valid :
      I.G.t -> I.PowLoc.t -> I.access_map -> I.Table.t -> I.Table.t -> bool **)
  
  let valid g fs_locs access_map0 inputof outputof =
    (&&) (valid_cfgs g fs_locs access_map0 inputof outputof)
      (valid_inter_edges g access_map0 inputof outputof)
  
  (** val check_query_access :
      pid_t -> I.Mem.t -> I.query -> I.status list I.coq_AccPair **)
  
  let check_query_access p m aexp =
    I.check_query (Obj.magic I.coq_AccMem) (Obj.magic I.coq_MAcc) p m aexp
  
  (** val check_query_only : pid_t -> I.Mem.t -> I.query -> I.status list **)
  
  let check_query_only p m aexp =
    I.check_query (Obj.magic I.coq_IdMem) (Obj.magic I.coq_MId) p m aexp
  
  (** val check_queries :
      pid_t -> I.Mem.t -> (I.query * DPos.DPos.t) ->
      (I.query * DPos.DPos.t) * I.status list **)
  
  let check_queries pid m = function
  | (q, pos) ->
    let statuses = check_query_only pid m q in ((q, pos), statuses)
  
  (** val query_flatten' :
      I.query -> DPos.DPos.t -> I.status list ->
      ((I.query * DPos.DPos.t) * I.status) list **)
  
  let rec query_flatten' q pos = function
  | [] -> []
  | status0 :: tl -> ((q, pos), status0) :: (query_flatten' q pos tl)
  
  (** val query_flatten :
      ((I.query * DPos.DPos.t) * I.status list) list ->
      ((I.query * DPos.DPos.t) * I.status) list **)
  
  let rec query_flatten = function
  | [] -> []
  | p :: tl ->
    let (p0, statuses) = p in
    let (q, pos) = p0 in
    app (query_flatten' q pos statuses) (query_flatten tl)
  
  (** val collect_alarm_result_node :
      I.Table.t -> string_t -> t' -> cmd ->
      ((I.query * DPos.DPos.t) * I.status) list ->
      ((I.query * DPos.DPos.t) * I.status) list **)
  
  let collect_alarm_result_node inputof pid node cmd0 acc =
    let query_list = I.collect_query cmd0 in
    let m = I.Table.find (pid, node) inputof in
    let query_status_list = map (check_queries pid m) query_list in
    app (query_flatten query_status_list) acc
  
  (** val collect_alarm_result_intra :
      I.Table.t -> string_t -> IntraCfg.t ->
      ((I.query * DPos.DPos.t) * I.status) list ->
      ((I.query * DPos.DPos.t) * I.status) list **)
  
  let collect_alarm_result_intra inputof pid cfg acc =
    let cmds0 = cfg.cmds in
    NodeMap.fold (collect_alarm_result_node inputof pid) cmds0 acc
  
  (** val collect_alarm_result :
      I.G.t -> I.Table.t -> ((I.query * DPos.DPos.t) * I.status) list **)
  
  let collect_alarm_result g inputof =
    let icfg0 = I.G.icfg g in
    let cfgs0 = icfg0.cfgs in
    PidMap.fold (collect_alarm_result_intra inputof) cfgs0 []
 end

