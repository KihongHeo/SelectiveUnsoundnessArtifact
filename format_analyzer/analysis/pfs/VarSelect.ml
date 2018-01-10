open Graph
open VocabB
open UserInput.Input

module C = Cil
module F = Frontc
module E = Errormsg

let rank_randomly loclist preinfo global = 
  let len = List.length loclist in
  let lst = List.map (fun l -> (l, Random.int len)) loclist in
  let lst_sorted = List.sort (fun (_,x) (_,y) -> compare x y) lst in
    List.map (fun (l,_) -> l) lst_sorted

(* take top X-percent-ranked locations, where x : 0 ~ 100 *)
let take_top x loclist =
  let len = List.length loclist in
  let _end = x * len / 100 in
  list2powloc (BatList.take _end loclist)

let rank (locset, pre, g) =
  let locs_ranked =
    if !Options.opt_rank_random then
      rank_randomly (powloc2list locset) pre g
    else VSHeuristic.rank locset pre g in
  let fs_locs = take_top !Options.opt_pfs locs_ranked in
  prerr_endline
    ("#selected locations: " ^ string_of_int (PowLoc.cardinal fs_locs)
     ^ "/" ^ string_of_int (PowLoc.cardinal (Pre.get_total_abslocs pre)));
  fs_locs
