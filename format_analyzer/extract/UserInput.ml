open AUGER_Monad
open DLat
open DMap
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomCon
open DomMem
open InterCfg
open InterNode
open IntraNode
open List0
open Pos
open Query
open SemAbs
open SemEval
open SemMem
open StringFun
open Sumbool
open Syn
open TStr
open UserInputType
open VocabA

type __ = Obj.t

module Input = 
 struct 
  module Proc = DStr.DStr
  
  module PowProc = PowProc
  
  module ProcMap = ProcMap
  
  module GVar = DStr.DStr
  
  module LVar = DomBasic.LVar
  
  module Var = DomBasic.Var
  
  module Field = DStr.DStr
  
  module Fields = DomBasic.Fields
  
  module ExtAllocsite = DomBasic.ExtAllocsite
  
  module Allocsite = DomBasic.Allocsite
  
  module VarAllocsite = VarAllocsite
  
  module Loc = DomBasic.Loc
  
  module PowLoc = PowLoc
  
  module LocMap = LocMap
  
  module ExtProcPos = ExtProcPos
  
  module PowExtProcPos = PowExtProcPos
  
  module Dump = Dump
  
  (** val var_of_gvar : GVar.t -> Var.t **)
  
  let var_of_gvar x =
    Coq_inl x
  
  (** val var_of_lvar : LVar.t -> Var.t **)
  
  let var_of_lvar x =
    Coq_inr x
  
  (** val classify_loc : Loc.t -> loc_type **)
  
  let classify_loc = function
  | (s, l0) ->
    (match s with
     | Coq_inl s0 ->
       (match s0 with
        | Coq_inl s1 -> GVarLoc
        | Coq_inr p -> let (f, s1) = p in LVarLoc f)
     | Coq_inr s0 -> OtherLoc)
  
  (** val loc_of_var : Var.t -> Loc.t **)
  
  let loc_of_var x =
    ((Coq_inl x), [])
  
  (** val loc_of_allocsite : Allocsite.t -> Loc.t **)
  
  let loc_of_allocsite x =
    ((Coq_inr x), [])
  
  (** val proc_of_allocsite : Allocsite.t -> Proc.t option **)
  
  let proc_of_allocsite = function
  | Coq_inl p -> None
  | Coq_inr s ->
    (match s with
     | Coq_inl u -> None
     | Coq_inr p -> Some p)
  
  (** val append_field : Loc.t -> Field.t -> Loc.t **)
  
  let append_field l f =
    ((fst l), (app (snd l) (f :: [])))
  
  (** val pow_loc_append_field : PowLoc.t -> Field.t -> PowLoc.t **)
  
  let pow_loc_append_field ls f =
    let add_appended = fun l acc -> PowLoc.add (append_field l f) acc in
    PowLoc.fold add_appended ls PowLoc.bot
  
  (** val allocsite_of_node : InterNode.t -> Allocsite.t **)
  
  let allocsite_of_node node =
    Coq_inl node
  
  (** val allocsite_of_ext : Proc.t option -> Allocsite.t **)
  
  let allocsite_of_ext = function
  | Some fid -> Coq_inr (Coq_inr fid)
  | None -> Coq_inr (Coq_inl ())
  
  module Val = Val
  
  (** val pow_proc_pos_of_val : Val.t -> DomBasic.PowExtProcPos.t **)
  
  let pow_proc_pos_of_val =
    Val.fst
  
  (** val pow_loc_of_val : Val.t -> DomBasic.PowLoc.t **)
  
  let pow_loc_of_val =
    Val.snd
  
  (** val array_of_val : Val.t -> ArrayBlk.t **)
  
  let array_of_val =
    Val.thrd
  
  (** val pow_proc_of_val : Val.t -> DomBasic.PowProc.t **)
  
  let pow_proc_of_val =
    Val.frth
  
  (** val val_of_pow_proc_pos : DomBasic.PowExtProcPos.t -> Val.t **)
  
  let val_of_pow_proc_pos x =
    (((x, DomBasic.PowLoc.coq_ILat.bot), ArrayBlk.coq_ILat.bot),
      DomBasic.PowProc.coq_ILat.bot)
  
  (** val val_of_pow_loc : DomBasic.PowLoc.t -> Val.t **)
  
  let val_of_pow_loc x =
    (((DomBasic.PowExtProcPos.coq_ILat.bot, x), ArrayBlk.coq_ILat.bot),
      DomBasic.PowProc.coq_ILat.bot)
  
  (** val val_of_array : ArrayBlk.t -> Val.t **)
  
  let val_of_array x =
    (((DomBasic.PowExtProcPos.coq_ILat.bot, DomBasic.PowLoc.coq_ILat.bot),
      x), DomBasic.PowProc.coq_ILat.bot)
  
  (** val val_of_pow_proc : DomBasic.PowProc.t -> Val.t **)
  
  let val_of_pow_proc x =
    (((DomBasic.PowExtProcPos.coq_ILat.bot, DomBasic.PowLoc.coq_ILat.bot),
      ArrayBlk.coq_ILat.bot), x)
  
  (** val modify_pow_proc_pos :
      Val.t -> DomBasic.PowExtProcPos.t -> Val.t **)
  
  let modify_pow_proc_pos x l =
    (((l, (pow_loc_of_val x)), (array_of_val x)), (pow_proc_of_val x))
  
  (** val modify_pow_loc : Val.t -> DomBasic.PowLoc.t -> Val.t **)
  
  let modify_pow_loc x l =
    ((((pow_proc_pos_of_val x), l), (array_of_val x)), (pow_proc_of_val x))
  
  (** val modify_array : Val.t -> ArrayBlk.t -> Val.t **)
  
  let modify_array x a =
    ((((pow_proc_pos_of_val x), (pow_loc_of_val x)), a), (pow_proc_of_val x))
  
  (** val modify_pow_proc : Val.t -> DomBasic.PowProc.t -> Val.t **)
  
  let modify_pow_proc x p =
    ((((pow_proc_pos_of_val x), (pow_loc_of_val x)), (array_of_val x)), p)
  
  (** val coq_Var_g : DomCon.Var.t -> DomBasic.Var.t **)
  
  let coq_Var_g = function
  | Coq_inl x -> Coq_inl x
  | Coq_inr p -> let (p0, x) = p in let (cid, f) = p0 in Coq_inr (f, x)
  
  (** val coq_VarAlloc_g : VarRegion.t -> DomBasic.VarAllocsite.t **)
  
  let coq_VarAlloc_g = function
  | Coq_inl v -> Coq_inl (coq_Var_g v)
  | Coq_inr p -> let (p0, oss) = p in let (step, alloc) = p0 in Coq_inr alloc
  
  (** val coq_Loc_g : DomCon.Loc.t -> DomBasic.Loc.t **)
  
  let coq_Loc_g = function
  | (con_va, fs) -> ((coq_VarAlloc_g con_va), fs)
  
  (** val coq_Val_g'_rect : val_t -> DomAbs.Val.t -> 'a1 **)
  
  let coq_Val_g'_rect v t0 =
    assert false (* absurd case *)
  
  (** val coq_Val_g'_rec : val_t -> DomAbs.Val.t -> 'a1 **)
  
  let coq_Val_g'_rec v t0 =
    assert false (* absurd case *)
  
  module Acc = Acc
  
  module Mem = Mem
  
  module G = G
  
  type access_map = Acc.t PidMap.t
  
  module Index = InterNode
  
  module Table = Table
  
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
    { AUGER_Monad.ret = (fun _ x -> (x, Acc.bot)); bind = (fun _ m _ f ->
      let (x, a1) = m in let (y, a2) = f x in (y, (Acc.join a1 a2))) }
  
  (** val get_v : 'a1 coq_AccPair -> 'a1 **)
  
  let get_v x =
    fst x
  
  (** val get_acc : 'a1 coq_AccPair -> Acc.t **)
  
  let get_acc x =
    snd x
  
  (** val coq_MId : __ coq_Monad **)
  
  let coq_MId =
    { AUGER_Monad.ret = (fun _ x -> x); bind = (fun _ m _ f -> f m) }
  
  (** val coq_AccMem :
      (__ coq_AccPair, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t)
      coq_MemBasic **)
  
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
  
  (** val coq_IdMem :
      (__, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic **)
  
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
  
  (** val can_strong_update_lv :
      (pid_t -> bool) -> DomBasic.Loc.t -> bool **)
  
  let can_strong_update_lv is_rec0 lv =
    match DomBasic.classify_loc lv with
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
      (match DomBasic.PowLoc.coq_ISet.set_choose_only lvs with
       | Some lv -> can_strong_update_lv is_rec0 lv
       | None -> false))
  
  (** val mem_lookup :
      ('a1, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic ->
      'a1 coq_Monad -> Mem.PowA.t -> Mem.t -> 'a1 **)
  
  let mem_lookup mB mBInst lvs m =
    let find_join = fun loc acc_a ->
      bind mBInst acc_a (fun acc ->
        bind mBInst (mB.mem_find loc m) (fun v ->
          AUGER_Monad.ret mBInst (DomAbs.Val.join acc v)))
    in
    set_fold DomBasic.PowLoc.coq_ISet find_join lvs
      (AUGER_Monad.ret mBInst DomAbs.Val.bot)
  
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
      ('a1, DomBasic.Loc.t, Mem.t, DomAbs.Val.t, Mem.PowA.t) coq_MemBasic ->
      'a1 coq_Monad -> param -> G.t -> Mem.PowA.t -> DomAbs.Val.t -> Mem.t ->
      'a1 **)
  
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
    then set_fold DomBasic.PowLoc.coq_ISet strong_add_v lvs
           (AUGER_Monad.ret mBInst m)
    else set_fold DomBasic.PowLoc.coq_ISet weak_add_v lvs
           (AUGER_Monad.ret mBInst m))
  
  (** val str_realloc : string_t **)
  
  let str_realloc = "realloc"
  
  (** val str_strlen : string_t **)
  
  let str_strlen = "strlen"
  
  (** val str_scanf : string_t **)
  
  let str_scanf = "scanf"
  
  (** val str_printf : string_t **)
  
  let str_printf = "printf"
  
  (** val str_vprintf : string_t **)
  
  let str_vprintf = "vprintf"
  
  (** val str_fprintf : string_t **)
  
  let str_fprintf = "fprintf"
  
  (** val str_sprintf : string_t **)
  
  let str_sprintf = "sprintf"
  
  (** val str_vfprintf : string_t **)
  
  let str_vfprintf = "vfprintf"
  
  (** val str_vsprintf : string_t **)
  
  let str_vsprintf = "vsprintf"
  
  (** val str_vasprintf : string_t **)
  
  let str_vasprintf = "vasprintf"
  
  (** val str___asprintf : string_t **)
  
  let str___asprintf = "__asprintf"
  
  (** val str_asprintf : string_t **)
  
  let str_asprintf = "asprintf"
  
  (** val str_vdprintf : string_t **)
  
  let str_vdprintf = "vdprintf"
  
  (** val str_dprintf : string_t **)
  
  let str_dprintf = "dprintf"
  
  (** val str_obstack_printf : string_t **)
  
  let str_obstack_printf = "obstack"
  
  (** val str_obstack_vprintf : string_t **)
  
  let str_obstack_vprintf = "obstack"
  
  (** val str_easprintf : string_t **)
  
  let str_easprintf = "easprintf"
  
  (** val str_evasprintf : string_t **)
  
  let str_evasprintf = "evasprintf"
  
  (** val str_snprintf : string_t **)
  
  let str_snprintf = "snprintf"
  
  (** val str_vsnprintf : string_t **)
  
  let str_vsnprintf = "vsnprintf"
  
  (** val str_argv : string_t **)
  
  let str_argv = "argv"
  
  (** val ext_api : t M.t **)
  
  let ext_api () = APIMap.read ()
  
  (** val is_printf1 : DStr.DStr.t -> bool **)
  
  let is_printf1 s =
    if sumbool_or (DStr.DStr.eq_dec s str_printf)
         (DStr.DStr.eq_dec s str_vprintf)
    then true
    else false
  
  (** val is_printf2 : DStr.DStr.t -> bool **)
  
  let is_printf2 s =
    if sumbool_or (DStr.DStr.eq_dec s str_fprintf)
         (sumbool_or (DStr.DStr.eq_dec s str_sprintf)
           (sumbool_or (DStr.DStr.eq_dec s str_vfprintf)
             (sumbool_or (DStr.DStr.eq_dec s str_vsprintf)
               (sumbool_or (DStr.DStr.eq_dec s str_vasprintf)
                 (sumbool_or (DStr.DStr.eq_dec s str___asprintf)
                   (sumbool_or (DStr.DStr.eq_dec s str_asprintf)
                     (sumbool_or (DStr.DStr.eq_dec s str_vdprintf)
                       (sumbool_or (DStr.DStr.eq_dec s str_dprintf)
                         (sumbool_or (DStr.DStr.eq_dec s str_obstack_printf)
                           (sumbool_or
                             (DStr.DStr.eq_dec s str_obstack_vprintf)
                             (sumbool_or (DStr.DStr.eq_dec s str_easprintf)
                               (DStr.DStr.eq_dec s str_evasprintf))))))))))))
    then true
    else false
  
  (** val is_printf3 : DStr.DStr.t -> bool **)
  
  let is_printf3 s =
    if sumbool_or (DStr.DStr.eq_dec s str_snprintf)
         (DStr.DStr.eq_dec s str_vsnprintf)
    then true
    else false
  
  (** val is_ppointer_printf : DStr.DStr.t -> bool **)
  
  let is_ppointer_printf s =
    if sumbool_or (DStr.DStr.eq_dec s str_vasprintf)
         (sumbool_or (DStr.DStr.eq_dec s str___asprintf)
           (sumbool_or (DStr.DStr.eq_dec s str_asprintf)
             (sumbool_or (DStr.DStr.eq_dec s str_easprintf)
               (DStr.DStr.eq_dec s str_evasprintf))))
    then true
    else false
  
  (** val is_no_assign_printf : DStr.DStr.t -> bool **)
  
  let is_no_assign_printf s =
    if sumbool_or (DStr.DStr.eq_dec s str_vdprintf)
         (DStr.DStr.eq_dec s str_dprintf)
    then true
    else false
  
  (** val is_printf : exp -> pid_t option **)
  
  let is_printf = function
  | Lval (lv, pos) ->
    let Coq_lval_intro (lh, o, pos0) = lv in
    (match lh with
     | VarLhost (f, is_global) ->
       (match o with
        | NoOffset ->
          if (||) ((||) (is_printf1 f) (is_printf2 f)) (is_printf3 f)
          then Some f
          else None
        | _ -> None)
     | MemLhost e0 -> None)
  | _ -> None
  
  (** val update_dump :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> InterNode.t -> DomBasic.PowProc.t ->
      lval option -> DomMem.Mem.t -> DomMem.G.t -> 'a1 **)
  
  let update_dump mB mBInst node =
    let pid = get_pid node in
    (fun callees ret_opt m g ->
    bind mBInst
      (match ret_opt with
       | Some ret_lv -> eval_lv mB mBInst pid ret_lv m
       | None -> AUGER_Monad.ret mBInst DomBasic.PowLoc.coq_ILat.bot)
      (fun rets ->
      let add_dump1 = fun callee d ->
        DomBasic.Dump.coq_IMap.map_weak_add callee rets d
      in
      AUGER_Monad.ret mBInst { DomMem.G.icfg = g.DomMem.G.icfg;
        DomMem.G.callgraph = g.DomMem.G.callgraph; DomMem.G.dump =
        (set_fold DomBasic.PowProc.coq_ISet add_dump1 callees
          g.DomMem.G.dump) }))
  
  (** val locals_of_mem :
      InterNode.t -> DomMem.Mem.t -> DomBasic.PowLoc.t **)
  
  let locals_of_mem node =
    let pid = get_pid node in
    (fun m ->
    let local_of_mem = fun l ->
      match fst l with
      | Coq_inl y ->
        (match y with
         | Coq_inl y0 -> DomBasic.PowLoc.coq_ISet.set_empty
         | Coq_inr y0 ->
           let (pid', y1) = y0 in
           if DStr.DStr.eq_dec pid pid'
           then DomBasic.PowLoc.coq_ISet.set_singleton l
           else DomBasic.PowLoc.coq_ISet.set_empty)
      | Coq_inr y -> DomBasic.PowLoc.coq_ISet.set_empty
    in
    DomMem.Mem.foldi (fun k x acc ->
      DomBasic.PowLoc.coq_ISet.set_union acc (local_of_mem k)) m
      DomBasic.PowLoc.coq_ISet.set_empty)
  
  (** val remove_local_variables :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> DomMem.Mem.t -> 'a1 **)
  
  let remove_local_variables mB mBInst p =
    let mode = DomMem.param_mode p in
    (fun node ->
    let pid = get_pid node in
    (fun g m ->
    match mode with
    | Weak -> AUGER_Monad.ret mBInst m
    | Strong ->
      if DomMem.G.is_rec pid g
      then AUGER_Monad.ret mBInst m
      else let remove0 = fun x m_a ->
             bind mBInst m_a (fun m0 ->
               DomMem.mem_update mB mBInst p g
                 (DomBasic.PowLoc.coq_ISet.set_singleton x)
                 DomAbs.Val.coq_ILat.bot m0)
           in
           set_fold DomBasic.PowLoc.coq_ISet remove0 (locals_of_mem node m)
             (AUGER_Monad.ret mBInst m)))
  
  (** val external_value :
      DomBasic.Allocsite.t -> DPos.DPos.t -> DomAbs.Val.t **)
  
  let external_value a pos =
    match DomBasic.proc_of_allocsite a with
    | Some p ->
      ((((DomBasic.PowExtProcPos.coq_ISet.set_singleton (p, pos)),
        (DomBasic.PowLoc.coq_ISet.set_singleton
          (DomBasic.loc_of_allocsite a))), (ArrayBlk.extern a)),
        DomBasic.PowProc.coq_ILat.bot)
    | None ->
      ((((DomBasic.PowExtProcPos.coq_ISet.set_singleton (str_argv, pos)),
        (DomBasic.PowLoc.coq_ISet.set_singleton
          (DomBasic.loc_of_allocsite a))), (ArrayBlk.extern a)),
        DomBasic.PowProc.coq_ILat.bot)
  
  (** val is_undef : DomMem.G.t -> exp -> pid_t option **)
  
  let is_undef g = function
  | Lval (lv, pos) ->
    let Coq_lval_intro (lh, o, pos0) = lv in
    (match lh with
     | VarLhost (f, is_global) ->
       (match o with
        | NoOffset ->
          if InterCfg.is_undef f g.DomMem.G.icfg then Some f else None
        | _ -> None)
     | MemLhost e0 -> None)
  | _ -> None
  
  (** val run_realloc :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> lval -> DomAbs.Val.t list -> DomMem.Mem.t -> 'a1 **)
  
  let run_realloc mB mBInst p node =
    let pid = get_pid node in
    (fun g ret_lv vs m ->
    match vs with
    | [] -> AUGER_Monad.ret mBInst m
    | ptr_v :: l ->
      (match l with
       | [] -> AUGER_Monad.ret mBInst m
       | size_v :: l0 ->
         let allocsite = eval_alloc' node in
         bind mBInst (eval_lv mB mBInst pid ret_lv m) (fun lv ->
           bind mBInst (DomMem.mem_update mB mBInst p g lv allocsite m)
             (fun m0 ->
             bind mBInst
               (DomMem.mem_lookup mB mBInst (deref_of_val ptr_v) m0)
               (fun orig_v ->
               DomMem.mem_update mB mBInst p g
                 (DomAbs.pow_loc_of_val allocsite) orig_v m0)))))
  
  (** val run_strlen :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> lval -> DomMem.Mem.t -> 'a1 **)
  
  let run_strlen mB mBInst p node =
    let pid = get_pid node in
    (fun g ret_lv m ->
    bind mBInst (eval_lv mB mBInst pid ret_lv m) (fun lv ->
      DomMem.mem_update mB mBInst p g lv DomAbs.Val.coq_ILat.bot m))
  
  (** val run_scanf :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> DomMem.G.t ->
      DomAbs.Val.t list -> DomMem.Mem.t -> pid_t -> DPos.DPos.t -> 'a1 **)
  
  let run_scanf mB mBInst p g vs m fname pos =
    match vs with
    | [] -> AUGER_Monad.ret mBInst m
    | t0 :: vs' ->
      list_fold (fun k m_acc ->
        bind mBInst m_acc (fun acc ->
          DomMem.mem_update mB mBInst p g k
            (DomAbs.val_of_pow_proc_pos
              (DomBasic.PowExtProcPos.coq_ISet.set_singleton (str_scanf,
                pos))) acc)) (map (fun v -> DomAbs.pow_loc_of_val v) vs')
        (AUGER_Monad.ret mBInst m)
  
  (** val run_printf1 : 'a1 coq_Monad -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf1 mBInst m =
    AUGER_Monad.ret mBInst m
  
  (** val val_joins : DomAbs.Val.t list -> DomAbs.Val.t **)
  
  let val_joins vals =
    list_fold (fun e acc -> DomAbs.Val.coq_ILat.join acc e) vals
      DomAbs.Val.coq_ILat.bot
  
  (** val va_src_val_joins :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.Mem.t -> DomAbs.Val.t list ->
      'a1 **)
  
  let va_src_val_joins mB mBInst m srcs =
    let va_src_val_join = fun m0 v m_acc ->
      bind mBInst (DomMem.mem_lookup mB mBInst (deref_of_val v) m0)
        (fun str_v ->
        bind mBInst m_acc (fun acc ->
          AUGER_Monad.ret mBInst
            (DomAbs.Val.coq_ILat.join (DomAbs.Val.coq_ILat.join acc v) str_v)))
    in
    list_fold (va_src_val_join m) srcs
      (AUGER_Monad.ret mBInst DomAbs.Val.coq_ILat.bot)
  
  (** val va_arg_joins :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> InterNode.t -> DomMem.Mem.t -> exp
      list -> 'a1 **)
  
  let va_arg_joins mB mBInst node =
    let pid = get_pid node in
    (fun m args0 ->
    let va_arg_join = fun m0 e m_acc ->
      bind mBInst m_acc (fun acc ->
        let deref_e = Lval ((Coq_lval_intro ((MemLhost e), NoOffset,
          DPos.DPos.unknown_pos)), DPos.DPos.unknown_pos)
        in
        bind mBInst (eval mB mBInst pid deref_e m0) (fun v1 ->
          bind mBInst (eval mB mBInst pid e m0) (fun v2 ->
            AUGER_Monad.ret mBInst
              (DomAbs.Val.coq_ILat.join (DomAbs.Val.coq_ILat.join acc v1) v2))))
    in
    list_fold (va_arg_join m) args0
      (AUGER_Monad.ret mBInst DomAbs.Val.coq_ILat.bot))
  
  (** val run_printf_pointer :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> exp -> exp list -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf_pointer mB mBInst p node =
    let pid = get_pid node in
    (fun g e args0 m ->
    bind mBInst (eval mB mBInst pid e m) (fun lv ->
      bind mBInst (va_arg_joins mB mBInst node m args0) (fun v ->
        DomMem.mem_update mB mBInst p g (deref_of_val lv)
          (DomAbs.val_of_pow_proc_pos (DomAbs.pow_proc_pos_of_val v)) m)))
  
  (** val run_printf_ppointer :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> exp -> exp list -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf_ppointer mB mBInst p node =
    let pid = get_pid node in
    (fun g e args0 m ->
    bind mBInst (eval mB mBInst pid e m) (fun lv ->
      bind mBInst (va_arg_joins mB mBInst node m args0) (fun v ->
        let alloc_v = eval_alloc' node in
        bind mBInst
          (DomMem.mem_update mB mBInst p g (deref_of_val lv) alloc_v m)
          (fun m0 ->
          DomMem.mem_update mB mBInst p g (DomAbs.pow_loc_of_val alloc_v)
            (DomAbs.val_of_pow_proc_pos (DomAbs.pow_proc_pos_of_val v)) m0))))
  
  (** val run_printf2 :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> pid_t -> exp list -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf2 mB mBInst p node g f args0 m =
    match args0 with
    | [] -> AUGER_Monad.ret mBInst m
    | hd :: l ->
      (match l with
       | [] -> AUGER_Monad.ret mBInst m
       | e :: tl ->
         if is_ppointer_printf f
         then run_printf_ppointer mB mBInst p node g hd tl m
         else if is_no_assign_printf f
              then AUGER_Monad.ret mBInst m
              else run_printf_pointer mB mBInst p node g hd tl m)
  
  (** val run_printf3 :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> exp list -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf3 mB mBInst p node g args0 m =
    match args0 with
    | [] -> AUGER_Monad.ret mBInst m
    | hd :: l ->
      (match l with
       | [] -> AUGER_Monad.ret mBInst m
       | e :: l0 ->
         (match l0 with
          | [] -> AUGER_Monad.ret mBInst m
          | e0 :: tl -> run_printf_pointer mB mBInst p node g hd tl m))
  
  (** val run_printf :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> pid_t -> exp list -> DomMem.Mem.t -> 'a1 **)
  
  let run_printf mB mBInst p node g f args0 m =
    if is_printf1 f
    then run_printf1 mBInst m
    else if is_printf2 f
         then run_printf2 mB mBInst p node g f args0 m
         else if is_printf3 f
              then run_printf3 mB mBInst p node g args0 m
              else AUGER_Monad.ret mBInst m
  
  (** val set_ext_allocsite :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> lval -> DomBasic.Allocsite.t -> DomMem.Mem.t ->
      DPos.DPos.t -> 'a1 **)
  
  let set_ext_allocsite mB mBInst p node =
    let pid = get_pid node in
    (fun g ret_lv allocsite m pos ->
    let ext_v = external_value allocsite pos in
    bind mBInst (eval_lv mB mBInst pid ret_lv m) (fun lv ->
      bind mBInst (DomMem.mem_update mB mBInst p g lv ext_v m) (fun m0 ->
        DomMem.mem_update mB mBInst p g
          (DomBasic.PowLoc.coq_ISet.set_singleton
            (DomBasic.loc_of_allocsite allocsite)) ext_v m0)))
  
  (** val find_string_api_name : string_t -> t option **)
  
  let find_string_api_name f =
    find f (ext_api ())
  
  (** val process_dsts :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> DPos.DPos.t -> string_t ->
      ((DomAbs.Val.t * alloc_t) * src_t) list -> DomAbs.Val.t -> DomMem.Mem.t
      -> 'a1 **)
  
  let process_dsts mB mBInst p node g pos fname dsts src_val m =
    let taint_val =
      external_value (DomBasic.allocsite_of_ext (Some fname)) pos
    in
    let process_dst = fun dst m_m ->
      bind mBInst m_m (fun m0 ->
        let (y, y0) = dst in
        let (val0, y1) = y in
        (match y1 with
         | NoAlloc ->
           (match y0 with
            | InSrc ->
              DomMem.mem_update mB mBInst p g (deref_of_val val0) src_val m0
            | ExSrc ->
              DomMem.mem_update mB mBInst p g (deref_of_val val0) taint_val
                m0)
         | DoAlloc ->
           (match y0 with
            | InSrc ->
              let alloc_v = eval_alloc' node in
              bind mBInst
                (DomMem.mem_update mB mBInst p g (deref_of_val val0) alloc_v
                  m0) (fun m1 ->
                DomMem.mem_update mB mBInst p g
                  (DomAbs.pow_loc_of_val alloc_v)
                  (DomAbs.val_of_pow_proc_pos
                    (DomAbs.pow_proc_pos_of_val src_val)) m1)
            | ExSrc ->
              let alloc_v = eval_alloc' node in
              bind mBInst
                (DomMem.mem_update mB mBInst p g (deref_of_val val0) alloc_v
                  m0) (fun m1 ->
                DomMem.mem_update mB mBInst p g
                  (DomAbs.pow_loc_of_val alloc_v)
                  (DomAbs.val_of_pow_proc_pos
                    (DomAbs.pow_proc_pos_of_val taint_val)) m1))))
    in
    list_fold process_dst dsts (AUGER_Monad.ret mBInst m)
  
  (** val process_ret :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> DPos.DPos.t -> string_t -> t -> lval option ->
      DomAbs.Val.t -> ((DomAbs.Val.t * alloc_t) * src_t) list -> DomMem.Mem.t
      -> 'a1 **)
  
  let process_ret mB mBInst p node =
    let pid = get_pid node in
    (fun g pos fname sfun ret_opt src_val dsts m ->
    match ret_opt with
    | Some ret_l ->
      bind mBInst (eval_lv mB mBInst pid ret_l m) (fun lv ->
        match sfun.ret with
        | CleanRet ->
          DomMem.mem_update mB mBInst p g lv DomAbs.Val.coq_ILat.bot m
        | TaintRet ->
          let taint_val =
            external_value (DomBasic.allocsite_of_ext (Some fname)) pos
          in
          DomMem.mem_update mB mBInst p g lv taint_val m
        | SrcRet alloc ->
          (match alloc with
           | NoAlloc -> DomMem.mem_update mB mBInst p g lv src_val m
           | DoAlloc ->
             let alloc_v = eval_alloc' node in
             bind mBInst (DomMem.mem_update mB mBInst p g lv alloc_v m)
               (fun m0 ->
               DomMem.mem_update mB mBInst p g
                 (DomAbs.pow_loc_of_val alloc_v)
                 (DomAbs.val_of_pow_proc_pos
                   (DomAbs.pow_proc_pos_of_val src_val)) m0))
        | DstRet ->
          let get_dst_val = fun dst ->
            let (y, y0) = dst in let (val0, y1) = y in val0
          in
          let dst_val = val_joins (map get_dst_val dsts) in
          DomMem.mem_update mB mBInst p g lv dst_val m)
    | None -> AUGER_Monad.ret mBInst m)
  
  (** val run_api :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> DomMem.Mem.t -> lval option -> DomAbs.Val.t list ->
      DPos.DPos.t -> string_t -> t -> 'a1 **)
  
  let run_api mB mBInst p node g m ret_opt vs pos fname sfun =
    let args0 = sfun.args in
    let (dsts, srcs) = get_dstsrc_list vs args0 [] [] in
    bind mBInst (va_src_val_joins mB mBInst m srcs) (fun src_val ->
      bind mBInst (process_dsts mB mBInst p node g pos fname dsts src_val m)
        (fun m0 ->
        bind mBInst
          (process_ret mB mBInst p node g pos fname sfun ret_opt src_val dsts
            m0) (fun m1 -> AUGER_Monad.ret mBInst m1)))
  
  (** val run_api_with_name :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> DomMem.Mem.t -> lval option -> DomAbs.Val.t list ->
      DPos.DPos.t -> string_t -> 'a1 **)
  
  let run_api_with_name mB mBInst p node g m ret_opt vs pos fname =
    match find_string_api_name fname with
    | Some sfun -> run_api mB mBInst p node g m ret_opt vs pos fname sfun
    | None ->
      (match ret_opt with
       | Some ret_lv ->
         set_ext_allocsite mB mBInst p node g ret_lv
           (DomBasic.allocsite_of_ext (Some fname)) m pos
       | None -> AUGER_Monad.ret mBInst m)
  
  (** val run_undef_funcs :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t ->
      DomMem.G.t -> lval option -> pid_t -> DomAbs.Val.t list -> DomMem.Mem.t
      -> DPos.DPos.t -> 'a1 **)
  
  let run_undef_funcs mB mBInst p node g ret_opt ext vs m pos =
    run_api_with_name mB mBInst p node g m ret_opt vs pos ext
  
  (** val run :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> DomMem.param -> InterNode.t -> cmd ->
      (DomMem.Mem.t * DomMem.G.t) -> 'a1 **)

  let run mB mBInst p =
    let phase = DomMem.param_phase p in
    let locs = DomMem.param_locs p in
    (fun node ->
    let pid = get_pid node in
    (fun cmd0 m_g ->
    let (m, g) = m_g in
    (match cmd0 with
     | Cset (l, e, pos) ->
       bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
         bind mBInst (eval mB mBInst pid e m) (fun v ->
           bind mBInst (DomMem.mem_update mB mBInst p g lv v m) (fun m0 ->
             AUGER_Monad.ret mBInst (m0, g))))
     | Cexternal (l, pos) ->
       bind mBInst
         (set_ext_allocsite mB mBInst p node g l
           (DomBasic.allocsite_of_ext None) m pos) (fun m0 ->
         AUGER_Monad.ret mBInst (m0, g))
     | Calloc (l, a, pos) ->
       bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
         bind mBInst
           (DomMem.mem_update mB mBInst p g lv (eval_alloc node a) m)
           (fun m0 -> AUGER_Monad.ret mBInst (m0, g)))
     | Csalloc (l, s, pos) ->
       let allocsite = DomBasic.allocsite_of_node node in
       let pow_loc =
         DomBasic.PowLoc.coq_ISet.set_singleton
           (DomBasic.loc_of_allocsite allocsite)
       in
       bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
         bind mBInst
           (bind mBInst
             (DomMem.mem_update mB mBInst p g pow_loc (eval_string s) m)
             (fun m0 ->
             DomMem.mem_update mB mBInst p g lv
               (eval_string_loc s allocsite pow_loc) m0)) (fun m0 ->
           AUGER_Monad.ret mBInst (m0, g)))
     | Cfalloc (l, name, pos) ->
       bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
         bind mBInst
           (DomMem.mem_update mB mBInst p g lv
             (DomAbs.val_of_pow_proc
               (DomBasic.PowProc.coq_ISet.set_singleton name)) m) (fun m0 ->
           AUGER_Monad.ret mBInst (m0, g)))
     | Ccall (ret_opt, f, args0, pos) ->
       (match is_printf f with
        | Some fname ->
          bind mBInst (run_printf mB mBInst p node g fname args0 m)
            (fun m0 -> AUGER_Monad.ret mBInst (m0, g))
        | None ->
          bind mBInst (eval_list mB mBInst pid args0 m) (fun vs ->
            match is_undef g f with
            | Some fname ->
            (* fully unsound *)
(*            if (not (StringFun.M.mem fname (ext_api ()))) && !Options.opt_unsound then 
              AUGER_Monad.ret mBInst (m,g)
            (* selectively unsound *)
            else *)if BatSet.mem ((Pos.Pos_as_OT.string_of_pos pos)^":"^fname) !Options.opt_unsound_lib then 
              AUGER_Monad.ret mBInst (m,g)
            else
              bind mBInst
                (run_undef_funcs mB mBInst p node g ret_opt fname vs m pos)
                (fun m0 -> AUGER_Monad.ret mBInst (m0, g))
            | None ->
              bind mBInst (eval mB mBInst pid f m) (fun f_v ->
                let fs = DomAbs.pow_proc_of_val f_v in
                let add_lvars_of_proc = fun f0 acc ->
                  match get_args g.DomMem.G.icfg f0 with
                  | Some args1 -> (map (fun x -> (f0, x)) args1) :: acc
                  | None -> acc
                in
                let lvars_list =
                  set_fold DomBasic.PowProc.coq_ISet add_lvars_of_proc fs []
                in
                bind mBInst (bind_lvars_list mB mBInst g p lvars_list vs m)
                  (fun m0 ->
                  bind mBInst (update_dump mB mBInst node fs ret_opt m0 g)
                    (fun g0 -> AUGER_Monad.ret mBInst (m0, g0))))))
     | Creturn (ret_opt, pos) ->
       bind mBInst
         (match ret_opt with
          | Some e ->
            bind mBInst (eval mB mBInst pid e m) (fun v ->
              DomMem.mem_update mB mBInst ((Weak, phase), locs) g
                (DomBasic.Dump.coq_IMap.map_find pid g.DomMem.G.dump) v m)
          | None -> AUGER_Monad.ret mBInst m) (fun m0 ->
         bind mBInst (remove_local_variables mB mBInst p node g m0)
           (fun m1 -> AUGER_Monad.ret mBInst (m1, g)))
     | _ -> AUGER_Monad.ret mBInst (m, g))))
  
  (** val run_access :
      DomMem.param -> InterNode.t -> cmd -> (DomMem.Mem.t * DomMem.G.t) ->
      (DomMem.Mem.t * DomMem.G.t) DomMem.coq_AccPair **)
  
  let run_access p node c m_g =
    run (Obj.magic DomMem.coq_AccMem) (Obj.magic DomMem.coq_MAcc) p node c
      m_g
  
  (** val run_only :
      DomMem.param -> InterNode.t -> cmd -> (DomMem.Mem.t * DomMem.G.t) ->
      DomMem.Mem.t * DomMem.G.t **)
  
  let run_only p node c m_g =
    run (Obj.magic DomMem.coq_IdMem) (Obj.magic DomMem.coq_MId) p node c m_g
  
  type query = exp
  
  type status' = Query.status' =
  | Tainted of DomBasic.PowExtProcPos.t
  | Clean
  
  (** val status'_rect :
      (DomBasic.PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1 **)
  
  let status'_rect f f0 = function
  | Tainted x -> f x
  | Clean -> f0
  
  (** val status'_rec :
      (DomBasic.PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1 **)
  
  let status'_rec f f0 = function
  | Tainted x -> f x
  | Clean -> f0
  
  type status = status'
  
  (** val fmt_query : exp -> (query * DPos.DPos.t) list **)
  
  let fmt_query fmt =
    (fmt, (pos_of_exp fmt)) :: []
  
  (** val collect_query : cmd -> (query * DPos.DPos.t) list **)
  
  let collect_query = function
  | Ccall (ret_opt, f0, arg, pos0) ->
    (match f0 with
     | Lval (lv, pos1) ->
       let Coq_lval_intro (lh, o, pos) = lv in
       (match lh with
        | VarLhost (f, is_global) ->
          (match o with
           | NoOffset ->
             if SemAbs.is_printf1 f
             then (match arg with
                   | [] -> []
                   | fmt :: l -> fmt_query fmt)
             else if SemAbs.is_printf2 f
                  then (match arg with
                        | [] -> []
                        | e :: l ->
                          (match l with
                           | [] -> []
                           | fmt :: l0 -> fmt_query fmt))
                  else if SemAbs.is_printf3 f
                       then (match arg with
                             | [] -> []
                             | e :: l ->
                               (match l with
                                | [] -> []
                                | e0 :: l0 ->
                                  (match l0 with
                                   | [] -> []
                                   | fmt :: l1 -> fmt_query fmt)))
                       else []
           | _ -> [])
        | MemLhost e -> [])
     | _ -> [])
  | _ -> []
  
  (** val check_query :
      ('a1, DomBasic.Loc.t, DomMem.Mem.t, DomAbs.Val.t, DomMem.Mem.PowA.t)
      coq_MemBasic -> 'a1 coq_Monad -> pid_t -> DomMem.Mem.t -> query -> 'a1 **)
  
  let check_query mB mBInst p mem0 q =
    let deref_e = Lval ((Coq_lval_intro ((MemLhost q), NoOffset,
      DPos.DPos.unknown_pos)), DPos.DPos.unknown_pos)
    in
    bind mBInst (eval mB mBInst p deref_e mem0) (fun v ->
      AUGER_Monad.ret mBInst
        ((if DomBasic.PowExtProcPos.coq_ILat.DLat.eq_dec
               (DomAbs.pow_proc_pos_of_val v)
               DomBasic.PowExtProcPos.coq_ILat.bot
          then Clean
          else Tainted (DomAbs.pow_proc_pos_of_val v)) :: []))
 end

