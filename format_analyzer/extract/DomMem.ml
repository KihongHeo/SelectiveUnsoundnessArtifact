open AUGER_Monad
open DLat
open DMap
open DMemSum
open DPow
open Datatypes
open DomAbs
open DomBasic
open InterCfg
open InterNode
open IntraNode
open Syn
open TStr
open UserInputType

type __ = Obj.t

module Acc = Access.Access(PowLoc)

module Mem = MemSum(Loc)(Val)(PowLoc)

module G = Global.Global(Dump)

type access_map = Acc.t PidMap.t

module Index = InterNode

module Table = Map(Index)(Mem)

type param = (update_mode * phase_t) * Mem.PowA.t

(** val param_mode : param -> update_mode **)

let param_mode p =
  fst (fst p)

(** val param_phase : param -> phase_t **)

let param_phase p =
  snd (fst p)

(** val param_locs : param -> Mem.PowA.t **)

let param_locs p =
  snd p

type 't coq_AccPair = 't * Acc.t

(** val coq_MAcc : __ coq_AccPair coq_Monad **)

let coq_MAcc =
  { ret = (fun _ x -> (x, Acc.bot)); bind = (fun _ m _ f ->
    let (x, a1) = m in let (y, a2) = f x in (y, (Acc.join a1 a2))) }

(** val get_v : 'a1 coq_AccPair -> 'a1 **)

let get_v x =
  fst x

(** val get_acc : 'a1 coq_AccPair -> Acc.t **)

let get_acc x =
  snd x

(** val coq_MId : __ coq_Monad **)

let coq_MId =
  { ret = (fun _ x -> x); bind = (fun _ m _ f -> f m) }

(** val coq_AccMem :
    (__ coq_AccPair, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic **)

let coq_AccMem =
  { mem_find = (fun l m -> ((Obj.magic (Mem.find l m)),
    (Acc.add Acc.USE l Acc.coq_ILat.bot))); mem_add = (fun fs_locs l v m ->
    ((Obj.magic (Mem.add fs_locs l v m)),
    (Acc.add Acc.DEF l Acc.coq_ILat.bot))); mem_pre_add =
    (fun fs_locs l v m -> ((Obj.magic (Mem.pre_add fs_locs l v m)),
    (Acc.add Acc.DEF l Acc.coq_ILat.bot))); mem_main_add =
    (fun fs_locs l v m -> ((Obj.magic (Mem.main_add fs_locs l v m)),
    (Acc.add Acc.DEF l Acc.coq_ILat.bot))); mem_weak_add =
    (fun fs_locs l v m -> ((Obj.magic (Mem.weak_add fs_locs l v m)),
    (Acc.add Acc.ALL l Acc.coq_ILat.bot))); mem_pre_weak_add =
    (fun fs_locs l v m -> ((Obj.magic (Mem.pre_weak_add fs_locs l v m)),
    (Acc.add Acc.ALL l Acc.coq_ILat.bot))); mem_main_weak_add =
    (fun fs_locs l v m -> ((Obj.magic (Mem.main_weak_add fs_locs l v m)),
    (Acc.add Acc.ALL l Acc.coq_ILat.bot))) }

(** val coq_IdMem : (__, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic **)

let coq_IdMem =
  { mem_find = (fun l m -> Obj.magic (Mem.find l m)); mem_add =
    (fun fs_locs l v m -> Obj.magic (Mem.add fs_locs l v m)); mem_pre_add =
    (fun fs_locs l v m -> Obj.magic (Mem.pre_add fs_locs l v m));
    mem_main_add = (fun fs_locs l v m ->
    Obj.magic (Mem.main_add fs_locs l v m)); mem_weak_add =
    (fun fs_locs l v m -> Obj.magic (Mem.weak_add fs_locs l v m));
    mem_pre_weak_add = (fun fs_locs l v m ->
    Obj.magic (Mem.pre_weak_add fs_locs l v m)); mem_main_weak_add =
    (fun fs_locs l v m -> Obj.magic (Mem.main_weak_add fs_locs l v m)) }

(** val can_strong_update_lv : (pid_t -> bool) -> Loc.t -> bool **)

let can_strong_update_lv is_rec0 lv =
  match classify_loc lv with
  | GVarLoc -> true
  | LVarLoc f -> negb (is_rec0 f)
  | OtherLoc -> false

(** val can_strong_update : param -> G.t -> Mem.PowA.t -> bool **)

let can_strong_update p =
  let mode = param_mode p in
  (fun g lvs ->
  match mode with
  | Weak -> false
  | Strong ->
    let is_rec0 = fun pid -> G.is_rec pid g in
    (match PowLoc.coq_ISet.set_choose_only lvs with
     | Some lv -> can_strong_update_lv is_rec0 lv
     | None -> false))

(** val mem_lookup :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    Mem.PowA.t -> Mem.t -> 'a1 **)

let mem_lookup mB mBInst lvs m =
  let find_join = fun loc acc_a ->
    bind mBInst acc_a (fun acc ->
      bind mBInst (mB.mem_find loc m) (fun v -> ret mBInst (Val.join acc v)))
  in
  set_fold PowLoc.coq_ISet find_join lvs (ret mBInst Val.bot)

(** val add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1 **)

let add p memBasic fs_locs l v m =
  let phase = param_phase p in
  (match phase with
   | PrePhase -> memBasic.mem_pre_add fs_locs l v m
   | MainPhase -> memBasic.mem_main_add fs_locs l v m
   | ValiPhase -> memBasic.mem_add fs_locs l v m)

(** val weak_add :
    param -> ('a1, 'a2, 'a3, 'a4, 'a5) coq_MemBasic -> 'a5 -> 'a2 -> 'a4 ->
    'a3 -> 'a1 **)

let weak_add p memBasic fs_locs l v m =
  let phase = param_phase p in
  (match phase with
   | PrePhase -> memBasic.mem_pre_weak_add fs_locs l v m
   | MainPhase -> memBasic.mem_main_weak_add fs_locs l v m
   | ValiPhase -> memBasic.mem_weak_add fs_locs l v m)

(** val mem_update :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> G.t -> Mem.PowA.t -> Val.t -> Mem.t -> 'a1 **)

let mem_update mB mBInst p =
  let locs = param_locs p in
  (fun g lvs v m ->
  let strong_add_v = fun lv m_a ->
    bind mBInst m_a (fun m0 -> add p mB locs lv v m0)
  in
  let weak_add_v = fun lv m_a ->
    bind mBInst m_a (fun m0 -> weak_add p mB locs lv v m0)
  in
  if can_strong_update p g lvs
  then set_fold PowLoc.coq_ISet strong_add_v lvs (ret mBInst m)
  else set_fold PowLoc.coq_ISet weak_add_v lvs (ret mBInst m))

