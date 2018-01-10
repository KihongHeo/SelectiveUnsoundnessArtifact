open AUGER_Monad
open Compare_dec
open DPow
open Datatypes
open DomAbs
open DomBasic
open DomMem
open List0
open Peano_dec
open UserInputType
open VocabA

(** val bind_lvar :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    G.t -> param -> LVar.t -> Val.t -> 'a1 -> 'a1 **)

let bind_lvar mB mBInst g p lvar v m_a =
  bind mBInst m_a (fun m ->
    mem_update mB mBInst p g
      (PowLoc.coq_ISet.set_singleton (loc_of_var (var_of_lvar lvar))) v m)

(** val bind_lvars :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    G.t -> param -> Val.t list -> LVar.t list -> 'a1 -> 'a1 **)

let bind_lvars mB mBInst g p vs lvars m_a =
  list_fold2 (bind_lvar mB mBInst g p) lvars vs m_a

(** val bind_lvars_list :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    G.t -> param -> LVar.t list list -> Val.t list -> Mem.t -> 'a1 **)

let bind_lvars_list mB mBInst g p lvars_list vs m =
  let vs_length = length vs in
  let is_same_length = fun l ->
    if eq_nat_dec (length l) vs_length then true else false
  in
  let lvars_list0 = (* filter is_same_length *) lvars_list in
  let mode =
    if gt_dec (length lvars_list0) (Pervasives.succ 0)
    then Weak
    else param_mode p
  in
  let p0 = ((mode, (param_phase p)), (param_locs p)) in
  list_fold (bind_lvars mB mBInst g p0 vs) lvars_list0 (ret mBInst m)

