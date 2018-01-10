open CallGraph
module SCC = Components.Make (CG)
module NSet = BatSet.Make (CG.V)
module N2G = BatMap.Make (CG.V)
module G2N = BatMap.Make(CG.V)
exception CheckSuccess

let check_groupgraph gg =
  let iter_one_group g (opt_start, opt_end, rst) =
    let failed = (opt_start, opt_end, false) in
    if rst then
      let income_edges = List.length (CG.pred_e gg g) in
      let outcome_edges = List.length (CG.succ_e gg g) in
      if income_edges = 0 && outcome_edges = 1 then
        match opt_start with
        | None -> (Some g, opt_end, rst)
        | _ -> failed
      else if income_edges = 1 && outcome_edges = 0 then
        match opt_end with
        | None -> (opt_start, Some g, rst)
        | _ -> failed
      else if income_edges = 1 && outcome_edges = 1 then
        (opt_start, opt_end, rst)
      else failed
    else failed
  in
  match CG.fold_vertex iter_one_group gg (None, None, true) with
  | (Some s, Some e, true) -> Some (s, e)
  | _ -> None

let make_scc_decompose_graph src sink edge_lbl backbone_g =
  let g =
    let fold_edges e acc =
      if CG.E.label e = edge_lbl then CG.add_edge_e acc e else acc
    in
    CG.fold_edges_e fold_edges backbone_g CG.empty
  in
  if CG.nb_edges g = 0 then
    if edge_lbl = "R" then
      let only_node = NSet.singleton src in
      Some (only_node, only_node)
    else
    if edge_lbl = "C" then
      let only_node = NSet.singleton sink in
      Some (only_node, only_node)
    else
      invalid_arg "lbl is not valid"
  else
  let scc_list = SCC.scc_list g in
  let init_scc_data (n2g, g2n) scc =
    let r_n = List.hd scc in
    let nset = NSet.of_list scc in
    let n2g' = NSet.fold (fun n acc -> N2G.add n r_n acc) nset n2g in
    let g2n' = G2N.add r_n nset g2n in
    (n2g', g2n')
  in
  let (n2g, g2n) =
    List.fold_left init_scc_data (N2G.empty, G2N.empty) scc_list
  in
  let init_groupgraph (v1, lbl, v2) acc =
    let g1 = N2G.find v1 n2g in
    let g2 = N2G.find v2 n2g in
    if CG.V.compare g1 g2 <> 0 then
      CG.add_edge_e acc (g1, lbl, g2)
    else
      acc
  in
  let rst_g = CG.fold_edges_e init_groupgraph g CG.empty in
  match check_groupgraph rst_g with
  | Some (s, e) -> Some (G2N.find s g2n, G2N.find e g2n)
  | _ -> None

let sanity_check_backbone backbone_g src sink =
  let _ = prerr_endline "In sanity_check" in
  match make_scc_decompose_graph src sink "R" backbone_g
      , make_scc_decompose_graph src sink "C" backbone_g with
  | Some (ret_s, ret_e), Some (call_s, call_e) ->
    NSet.mem src ret_s
    && NSet.mem sink call_e
    && not (NSet.is_empty (NSet.inter ret_e call_s))
  | _, _ -> false
