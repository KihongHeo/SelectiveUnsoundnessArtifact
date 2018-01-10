open AUGER_Monad
open DLat
open DMap
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomMem
open InterCfg
open InterNode
open List0
open SemEval
open SemMem
open StringFun
open Sumbool
open Syn
open TStr
open UserInputType
open VocabA

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

let ext_api = APIMap.read ()

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
                         (sumbool_or (DStr.DStr.eq_dec s str_obstack_vprintf)
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
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    InterNode.t -> PowProc.t -> lval option -> Mem.t -> G.t -> 'a1 **)

let update_dump mB mBInst node =
  let pid = get_pid node in
  (fun callees ret_opt m g ->
  bind mBInst
    (match ret_opt with
     | Some ret_lv -> eval_lv mB mBInst pid ret_lv m
     | None -> AUGER_Monad.ret mBInst PowLoc.coq_ILat.bot) (fun rets ->
    let add_dump1 = fun callee d -> Dump.coq_IMap.map_weak_add callee rets d
    in
    AUGER_Monad.ret mBInst { G.icfg = g.G.icfg; G.callgraph = g.G.callgraph;
      G.dump = (set_fold PowProc.coq_ISet add_dump1 callees g.G.dump) }))

(** val locals_of_mem : InterNode.t -> Mem.t -> PowLoc.t **)

let locals_of_mem node =
  let pid = get_pid node in
  (fun m ->
  let local_of_mem = fun l ->
    match fst l with
    | Coq_inl y ->
      (match y with
       | Coq_inl y0 -> PowLoc.coq_ISet.set_empty
       | Coq_inr y0 ->
         let (pid', y1) = y0 in
         if DStr.DStr.eq_dec pid pid'
         then PowLoc.coq_ISet.set_singleton l
         else PowLoc.coq_ISet.set_empty)
    | Coq_inr y -> PowLoc.coq_ISet.set_empty
  in
  Mem.foldi (fun k x acc -> PowLoc.coq_ISet.set_union acc (local_of_mem k)) m
    PowLoc.coq_ISet.set_empty)

(** val remove_local_variables :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> Mem.t -> 'a1 **)

let remove_local_variables mB mBInst p =
  let mode = param_mode p in
  (fun node ->
  let pid = get_pid node in
  (fun g m ->
  match mode with
  | Weak -> AUGER_Monad.ret mBInst m
  | Strong ->
    if G.is_rec pid g
    then AUGER_Monad.ret mBInst m
    else let remove = fun x m_a ->
           bind mBInst m_a (fun m0 ->
             mem_update mB mBInst p g (PowLoc.coq_ISet.set_singleton x)
               Val.coq_ILat.bot m0)
         in
         set_fold PowLoc.coq_ISet remove (locals_of_mem node m)
           (AUGER_Monad.ret mBInst m)))

(** val external_value : Allocsite.t -> DPos.DPos.t -> Val.t **)

let external_value a pos =
  match proc_of_allocsite a with
  | Some p ->
    ((((PowExtProcPos.coq_ISet.set_singleton (p, pos)),
      (PowLoc.coq_ISet.set_singleton (loc_of_allocsite a))),
      (ArrayBlk.extern a)), PowProc.coq_ILat.bot)
  | None ->
    ((((PowExtProcPos.coq_ISet.set_singleton (str_argv, pos)),
      (PowLoc.coq_ISet.set_singleton (loc_of_allocsite a))),
      (ArrayBlk.extern a)), PowProc.coq_ILat.bot)

(** val is_undef : G.t -> exp -> pid_t option **)

let is_undef g = function
| Lval (lv, pos) ->
  let Coq_lval_intro (lh, o, pos0) = lv in
  (match lh with
   | VarLhost (f, is_global) ->
     (match o with
      | NoOffset -> if is_undef f g.G.icfg then Some f else None
      | _ -> None)
   | MemLhost e0 -> None)
| _ -> None

(** val run_realloc :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> lval -> Val.t list -> Mem.t -> 'a1 **)

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
         bind mBInst (mem_update mB mBInst p g lv allocsite m) (fun m0 ->
           bind mBInst (mem_lookup mB mBInst (deref_of_val ptr_v) m0)
             (fun orig_v ->
             mem_update mB mBInst p g (pow_loc_of_val allocsite) orig_v m0)))))

(** val run_strlen :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> lval -> Mem.t -> 'a1 **)

let run_strlen mB mBInst p node =
  let pid = get_pid node in
  (fun g ret_lv m ->
  bind mBInst (eval_lv mB mBInst pid ret_lv m) (fun lv ->
    mem_update mB mBInst p g lv Val.coq_ILat.bot m))

(** val run_scanf :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> G.t -> Val.t list -> Mem.t -> pid_t -> DPos.DPos.t -> 'a1 **)

let run_scanf mB mBInst p g vs m fname pos =
  match vs with
  | [] -> AUGER_Monad.ret mBInst m
  | t0 :: vs' ->
    list_fold (fun k m_acc ->
      bind mBInst m_acc (fun acc ->
        mem_update mB mBInst p g k
          (val_of_pow_proc_pos
            (PowExtProcPos.coq_ISet.set_singleton (str_scanf, pos))) acc))
      (map (fun v -> pow_loc_of_val v) vs') (AUGER_Monad.ret mBInst m)

(** val run_printf1 : 'a1 coq_Monad -> Mem.t -> 'a1 **)

let run_printf1 mBInst m =
  AUGER_Monad.ret mBInst m

(** val val_joins : Val.t list -> Val.t **)

let val_joins vals =
  list_fold (fun e acc -> Val.coq_ILat.join acc e) vals Val.coq_ILat.bot

(** val va_src_val_joins :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    Mem.t -> Val.t list -> 'a1 **)

let va_src_val_joins mB mBInst m srcs =
  let va_src_val_join = fun m0 v m_acc ->
    bind mBInst (mem_lookup mB mBInst (deref_of_val v) m0) (fun str_v ->
      bind mBInst m_acc (fun acc ->
        AUGER_Monad.ret mBInst
          (Val.coq_ILat.join (Val.coq_ILat.join acc v) str_v)))
  in
  list_fold (va_src_val_join m) srcs
    (AUGER_Monad.ret mBInst Val.coq_ILat.bot)

(** val va_arg_joins :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    InterNode.t -> Mem.t -> exp list -> 'a1 **)

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
            (Val.coq_ILat.join (Val.coq_ILat.join acc v1) v2))))
  in
  list_fold (va_arg_join m) args0 (AUGER_Monad.ret mBInst Val.coq_ILat.bot))

(** val run_printf_pointer :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> exp -> exp list -> Mem.t -> 'a1 **)

let run_printf_pointer mB mBInst p node =
  let pid = get_pid node in
  (fun g e args0 m ->
  bind mBInst (eval mB mBInst pid e m) (fun lv ->
    bind mBInst (va_arg_joins mB mBInst node m args0) (fun v ->
      mem_update mB mBInst p g (deref_of_val lv)
        (val_of_pow_proc_pos (pow_proc_pos_of_val v)) m)))

(** val run_printf_ppointer :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> exp -> exp list -> Mem.t -> 'a1 **)

let run_printf_ppointer mB mBInst p node =
  let pid = get_pid node in
  (fun g e args0 m ->
  bind mBInst (eval mB mBInst pid e m) (fun lv ->
    bind mBInst (va_arg_joins mB mBInst node m args0) (fun v ->
      let alloc_v = eval_alloc' node in
      bind mBInst (mem_update mB mBInst p g (deref_of_val lv) alloc_v m)
        (fun m0 ->
        mem_update mB mBInst p g (pow_loc_of_val alloc_v)
          (val_of_pow_proc_pos (pow_proc_pos_of_val v)) m0))))

(** val run_printf2 :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> pid_t -> exp list -> Mem.t -> 'a1 **)

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
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> exp list -> Mem.t -> 'a1 **)

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
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> pid_t -> exp list -> Mem.t -> 'a1 **)

let run_printf mB mBInst p node g f args0 m =
  if is_printf1 f
  then run_printf1 mBInst m
  else if is_printf2 f
       then run_printf2 mB mBInst p node g f args0 m
       else if is_printf3 f
            then run_printf3 mB mBInst p node g args0 m
            else AUGER_Monad.ret mBInst m

(** val set_ext_allocsite :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> lval -> Allocsite.t -> Mem.t ->
    DPos.DPos.t -> 'a1 **)

let set_ext_allocsite mB mBInst p node =
  let pid = get_pid node in
  (fun g ret_lv allocsite m pos ->
  let ext_v = external_value allocsite pos in
  bind mBInst (eval_lv mB mBInst pid ret_lv m) (fun lv ->
    bind mBInst (mem_update mB mBInst p g lv ext_v m) (fun m0 ->
      mem_update mB mBInst p g
        (PowLoc.coq_ISet.set_singleton (loc_of_allocsite allocsite)) ext_v m0)))

(** val find_string_api_name : string_t -> t option **)

let find_string_api_name f =
  find f ext_api

(** val process_dsts :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> DPos.DPos.t -> string_t ->
    ((Val.t * alloc_t) * src_t) list -> Val.t -> Mem.t -> 'a1 **)

let process_dsts mB mBInst p node g pos fname dsts src_val m =
  let taint_val = external_value (allocsite_of_ext (Some fname)) pos in
  let process_dst = fun dst m_m ->
    bind mBInst m_m (fun m0 ->
      let (y, y0) = dst in
      let (val0, y1) = y in
      (match y1 with
       | NoAlloc ->
         (match y0 with
          | InSrc -> mem_update mB mBInst p g (deref_of_val val0) src_val m0
          | ExSrc ->
            mem_update mB mBInst p g (deref_of_val val0) taint_val m0)
       | DoAlloc ->
         (match y0 with
          | InSrc ->
            let alloc_v = eval_alloc' node in
            bind mBInst
              (mem_update mB mBInst p g (deref_of_val val0) alloc_v m0)
              (fun m1 ->
              mem_update mB mBInst p g (pow_loc_of_val alloc_v)
                (val_of_pow_proc_pos (pow_proc_pos_of_val src_val)) m1)
          | ExSrc ->
            let alloc_v = eval_alloc' node in
            bind mBInst
              (mem_update mB mBInst p g (deref_of_val val0) alloc_v m0)
              (fun m1 ->
              mem_update mB mBInst p g (pow_loc_of_val alloc_v)
                (val_of_pow_proc_pos (pow_proc_pos_of_val taint_val)) m1))))
  in
  list_fold process_dst dsts (AUGER_Monad.ret mBInst m)

(** val process_ret :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> DPos.DPos.t -> string_t -> t -> lval
    option -> Val.t -> ((Val.t * alloc_t) * src_t) list -> Mem.t -> 'a1 **)

let process_ret mB mBInst p node =
  let pid = get_pid node in
  (fun g pos fname sfun ret_opt src_val dsts m ->
  match ret_opt with
  | Some ret_l ->
    bind mBInst (eval_lv mB mBInst pid ret_l m) (fun lv ->
      match sfun.ret with
      | CleanRet -> mem_update mB mBInst p g lv Val.coq_ILat.bot m
      | TaintRet ->
        let taint_val = external_value (allocsite_of_ext (Some fname)) pos in
        mem_update mB mBInst p g lv taint_val m
      | SrcRet alloc ->
        (match alloc with
         | NoAlloc -> mem_update mB mBInst p g lv src_val m
         | DoAlloc ->
           let alloc_v = eval_alloc' node in
           bind mBInst (mem_update mB mBInst p g lv alloc_v m) (fun m0 ->
             mem_update mB mBInst p g (pow_loc_of_val alloc_v)
               (val_of_pow_proc_pos (pow_proc_pos_of_val src_val)) m0))
      | DstRet ->
        let get_dst_val = fun dst ->
          let (y, y0) = dst in let (val0, y1) = y in val0
        in
        let dst_val = val_joins (map get_dst_val dsts) in
        mem_update mB mBInst p g lv dst_val m)
  | None -> AUGER_Monad.ret mBInst m)

(** val run_api :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> Mem.t -> lval option -> Val.t list ->
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
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> Mem.t -> lval option -> Val.t list ->
    DPos.DPos.t -> string_t -> 'a1 **)

let run_api_with_name mB mBInst p node g m ret_opt vs pos fname =
  match find_string_api_name fname with
  | Some sfun -> run_api mB mBInst p node g m ret_opt vs pos fname sfun
  | None ->
    (match ret_opt with
     | Some ret_lv ->
       set_ext_allocsite mB mBInst p node g ret_lv
         (allocsite_of_ext (Some fname)) m pos
     | None -> AUGER_Monad.ret mBInst m)

(** val run_undef_funcs :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> G.t -> lval option -> pid_t -> Val.t list ->
    Mem.t -> DPos.DPos.t -> 'a1 **)

let run_undef_funcs mB mBInst p node g ret_opt ext vs m pos =
  run_api_with_name mB mBInst p node g m ret_opt vs pos ext

(** val run :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    param -> InterNode.t -> cmd -> (Mem.t * G.t) -> 'a1 **)

let run mB mBInst p =
  let phase = param_phase p in
  let locs = param_locs p in
  (fun node ->
  let pid = get_pid node in
  (fun cmd0 m_g ->
  let (m, g) = m_g in
  (match cmd0 with
   | Cset (l, e, pos) ->
     bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
       bind mBInst (eval mB mBInst pid e m) (fun v ->
         bind mBInst (mem_update mB mBInst p g lv v m) (fun m0 ->
           AUGER_Monad.ret mBInst (m0, g))))
   | Cexternal (l, pos) ->
     bind mBInst
       (set_ext_allocsite mB mBInst p node g l (allocsite_of_ext None) m pos)
       (fun m0 -> AUGER_Monad.ret mBInst (m0, g))
   | Calloc (l, a, pos) ->
     bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
       bind mBInst (mem_update mB mBInst p g lv (eval_alloc node a) m)
         (fun m0 -> AUGER_Monad.ret mBInst (m0, g)))
   | Csalloc (l, s, pos) ->
     let allocsite = allocsite_of_node node in
     let pow_loc = PowLoc.coq_ISet.set_singleton (loc_of_allocsite allocsite)
     in
     bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
       bind mBInst
         (bind mBInst (mem_update mB mBInst p g pow_loc (eval_string s) m)
           (fun m0 ->
           mem_update mB mBInst p g lv (eval_string_loc s allocsite pow_loc)
             m0)) (fun m0 -> AUGER_Monad.ret mBInst (m0, g)))
   | Cfalloc (l, name, pos) ->
     bind mBInst (eval_lv mB mBInst pid l m) (fun lv ->
       bind mBInst
         (mem_update mB mBInst p g lv
           (val_of_pow_proc (PowProc.coq_ISet.set_singleton name)) m)
         (fun m0 -> AUGER_Monad.ret mBInst (m0, g)))
   | Ccall (ret_opt, f, args0, pos) ->
     (match is_printf f with
      | Some fname ->
        bind mBInst (run_printf mB mBInst p node g fname args0 m) (fun m0 ->
          AUGER_Monad.ret mBInst (m0, g))
      | None ->
        bind mBInst (eval_list mB mBInst pid args0 m) (fun vs ->
          match is_undef g f with
          | Some fname ->
            bind mBInst
              (run_undef_funcs mB mBInst p node g ret_opt fname vs m pos)
              (fun m0 -> AUGER_Monad.ret mBInst (m0, g))
          | None ->
            bind mBInst (eval mB mBInst pid f m) (fun f_v ->
              let fs = pow_proc_of_val f_v in
              let add_lvars_of_proc = fun f0 acc ->
                match get_args g.G.icfg f0 with
                | Some args1 -> (map (fun x -> (f0, x)) args1) :: acc
                | None -> acc
              in
              let lvars_list =
                set_fold PowProc.coq_ISet add_lvars_of_proc fs []
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
            mem_update mB mBInst ((Weak, phase), locs) g
              (Dump.coq_IMap.map_find pid g.G.dump) v m)
        | None -> AUGER_Monad.ret mBInst m) (fun m0 ->
       bind mBInst (remove_local_variables mB mBInst p node g m0) (fun m1 ->
         AUGER_Monad.ret mBInst (m1, g)))
   | _ -> AUGER_Monad.ret mBInst (m, g))))

(** val run_access :
    param -> InterNode.t -> cmd -> (Mem.t * G.t) -> (Mem.t * G.t) coq_AccPair **)

let run_access p node c m_g =
  run (Obj.magic coq_AccMem) (Obj.magic coq_MAcc) p node c m_g

(** val run_only :
    param -> InterNode.t -> cmd -> (Mem.t * G.t) -> Mem.t * G.t **)

let run_only p node c m_g =
  run (Obj.magic coq_IdMem) (Obj.magic coq_MId) p node c m_g

