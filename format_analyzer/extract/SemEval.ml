open AUGER_Monad
open BinInt
open DItv
open DLat
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomMem
open InterNode
open String0
open Syn
open UserInputType

(** val eval_const : constant -> Val.t **)

let eval_const cst =
  Val.coq_ILat.bot

(** val eval_uop : unop -> Val.t -> Val.t **)

let eval_uop u v =
  v

(** val is_array_loc : Loc.t -> bool **)

let is_array_loc x =
  match fst x with
  | Coq_inl s -> false
  | Coq_inr s -> true

(** val array_loc_of_val : Val.t -> Val.t **)

let array_loc_of_val v =
  val_of_pow_loc (PowLoc.coq_ISet.set_filter is_array_loc (pow_loc_of_val v))

(** val eval_bop : binop -> Val.t -> Val.t -> Val.t **)

let eval_bop b v1 v2 =
  Val.coq_ILat.join v1 v2

(** val eval_string : char list -> Val.t **)

let eval_string s =
  Val.coq_ILat.bot

(** val eval_string_loc : char list -> Allocsite.t -> PowLoc.t -> Val.t **)

let eval_string_loc s a lvs =
  let i = Z.of_nat (Pervasives.succ (length s)) in
  val_of_pow_loc
    (PowLoc.coq_ILat.join lvs
      (pow_loc_of_val
        (val_of_array
          (ArrayBlk.make a (Itv.gen_itv (Itv.Int 0) (Itv.Int 0))
            (Itv.gen_itv (Itv.Int i) (Itv.Int i))
            (Itv.gen_itv (Itv.Int 1) (Itv.Int 1))))))

(** val deref_of_val : Val.t -> PowLoc.t **)

let deref_of_val v =
  PowLoc.coq_ILat.join (pow_loc_of_val v)
    (ArrayBlk.pow_loc_of_array (array_of_val v))

(** val resolve_offset :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    Val.t -> offset -> Mem.t -> 'a1 **)

let rec resolve_offset mB mBInst v os m =
  match os with
  | NoOffset -> ret mBInst (deref_of_val v)
  | FOffset (f, os') ->
    resolve_offset mB mBInst
      (val_of_pow_loc
        (PowLoc.coq_ILat.join (pow_loc_append_field (pow_loc_of_val v) f)
          (ArrayBlk.pow_loc_of_struct_w_field (array_of_val v) f))) os' m
  | IOffset (e, os') ->
    bind mBInst (mem_lookup mB mBInst (deref_of_val v) m) (fun v' ->
      resolve_offset mB mBInst v' os' m)

(** val eval :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    pid_t -> exp -> Mem.t -> 'a1 **)

let eval mB mBInst pid =
  let rec eval0 e m =
    match e with
    | Const (c, pos) -> ret mBInst (eval_const c)
    | Lval (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun lv -> mem_lookup mB mBInst lv m)
    | UnOp (u, e0, pos) ->
      bind mBInst (eval0 e0 m) (fun v -> ret mBInst (eval_uop u v))
    | BinOp (b, e1, e2, pos) ->
      bind mBInst (eval0 e1 m) (fun v1 ->
        bind mBInst (eval0 e2 m) (fun v2 -> ret mBInst (eval_bop b v1 v2)))
    | Question (e1, e2, e3, pos) ->
      bind mBInst (eval0 e2 m) (fun v2 ->
        bind mBInst (eval0 e3 m) (fun v3 ->
          ret mBInst (Val.coq_ILat.join v2 v3)))
    | CastE (i, e0, pos) ->
      (match i with
       | Some new_stride ->
         bind mBInst (eval0 e0 m) (fun v ->
           ret mBInst
             (modify_array v
               (ArrayBlk.cast_array_int new_stride (array_of_val v))))
       | None -> eval0 e0 m)
    | AddrOf (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun v -> ret mBInst (val_of_pow_loc v))
    | StartOf (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun lv -> mem_lookup mB mBInst lv m)
    | _ -> ret mBInst Val.coq_ILat.bot
  and eval_lv0 lv m =
    let Coq_lval_intro (lhost', offset0, pos) = lv in
    bind mBInst
      (match lhost' with
       | VarLhost (vi, is_global) ->
         let x = if is_global then var_of_gvar vi else var_of_lvar (pid, vi)
         in
         ret mBInst
           (val_of_pow_loc (PowLoc.coq_ISet.set_singleton (loc_of_var x)))
       | MemLhost e -> eval0 e m) (fun v ->
      resolve_offset mB mBInst v offset0 m)
  in eval0

(** val eval_lv :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    pid_t -> lval -> Mem.t -> 'a1 **)

let eval_lv mB mBInst pid =
  let rec eval0 e m =
    match e with
    | Const (c, pos) -> ret mBInst (eval_const c)
    | Lval (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun lv -> mem_lookup mB mBInst lv m)
    | UnOp (u, e0, pos) ->
      bind mBInst (eval0 e0 m) (fun v -> ret mBInst (eval_uop u v))
    | BinOp (b, e1, e2, pos) ->
      bind mBInst (eval0 e1 m) (fun v1 ->
        bind mBInst (eval0 e2 m) (fun v2 -> ret mBInst (eval_bop b v1 v2)))
    | Question (e1, e2, e3, pos) ->
      bind mBInst (eval0 e2 m) (fun v2 ->
        bind mBInst (eval0 e3 m) (fun v3 ->
          ret mBInst (Val.coq_ILat.join v2 v3)))
    | CastE (i, e0, pos) ->
      (match i with
       | Some new_stride ->
         bind mBInst (eval0 e0 m) (fun v ->
           ret mBInst
             (modify_array v
               (ArrayBlk.cast_array_int new_stride (array_of_val v))))
       | None -> eval0 e0 m)
    | AddrOf (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun v -> ret mBInst (val_of_pow_loc v))
    | StartOf (l, pos) ->
      bind mBInst (eval_lv0 l m) (fun lv -> mem_lookup mB mBInst lv m)
    | _ -> ret mBInst Val.coq_ILat.bot
  and eval_lv0 lv m =
    let Coq_lval_intro (lhost', offset0, pos) = lv in
    bind mBInst
      (match lhost' with
       | VarLhost (vi, is_global) ->
         let x = if is_global then var_of_gvar vi else var_of_lvar (pid, vi)
         in
         ret mBInst
           (val_of_pow_loc (PowLoc.coq_ISet.set_singleton (loc_of_var x)))
       | MemLhost e -> eval0 e m) (fun v ->
      resolve_offset mB mBInst v offset0 m)
  in eval_lv0

(** val eval_list :
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    pid_t -> exp list -> Mem.t -> 'a1 **)

let rec eval_list mB mBInst pid es m =
  match es with
  | [] -> ret mBInst []
  | e :: tl ->
    bind mBInst (eval mB mBInst pid e m) (fun v ->
      bind mBInst (eval_list mB mBInst pid tl m) (fun tl' ->
        ret mBInst (v :: tl')))

(** val eval_alloc' : t -> Val.t **)

let eval_alloc' node =
  let allocsite = allocsite_of_node node in
  let pow_loc = PowLoc.coq_ISet.set_singleton (loc_of_allocsite allocsite) in
  Val.coq_ILat.join (val_of_pow_loc pow_loc)
    (val_of_array
      (ArrayBlk.make allocsite (Itv.gen_itv (Itv.Int 0) (Itv.Int 0)) Itv.top
        (Itv.gen_itv (Itv.Int 1) (Itv.Int 1))))

(** val eval_alloc : t -> alloc -> Val.t **)

let eval_alloc node a =
  eval_alloc' node

