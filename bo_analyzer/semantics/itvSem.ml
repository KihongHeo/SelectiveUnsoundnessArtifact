(** Semantics of interval analysis *)
open Vocab
open Cil
open AbsSem
open BasicDom
open ItvDom
open Global
open ArrayBlk
open BatTuple

module Dom = ItvDom.Mem
module Access = Access.Make(Dom)

(* ************************* *
 * Accessibility information *
 * ************************* *)
let access = ref Access.empty

let access_mode = ref false
let init_access : unit -> unit 
= fun () -> 
  access_mode := true; access := Access.empty; ()

let return_access : unit -> Access.t
= fun () -> access_mode := false; !access

(* ********************** *
 * Abstract memory access *
 * ********************** *)
let can_strong_update : update_mode -> Global.t -> PowLoc.t -> bool
= fun mode global lvs ->
  if !Options.opt_unsound_update_all then true
  else 
    match mode,lvs with
    | Weak, _ -> false
    | Strong, lvs' ->
      if PowLoc.cardinal lvs' = 1 then
        let lv = PowLoc.choose lvs' in
        Loc.is_gvar lv || 
        (Loc.is_lvar lv && not (Global.is_rec (Loc.get_proc lv) global))
      else false

let lookup : PowLoc.t -> Mem.t -> Val.t
= fun locs mem ->
  (if !access_mode && not (Mem.eq mem Mem.bot) then 
    access := Access.add_set Access.use locs !access);
  Mem.lookup locs mem

let update : update_mode -> Global.t -> PowLoc.t -> Val.t -> Mem.t -> Mem.t  
= fun mode global locs v mem -> 
  (if !access_mode then 
     if can_strong_update mode global locs then 
       access := Access.add_set Access.def locs !access
     else 
       access := Access.add_set Access.all locs !access);
  (* unsoundness *)
  let locs = PowLoc.filter (fun k ->
      (*not (UnsoundGlobal.unsound_target k v)
      &&*) not (BatSet.mem (Loc.to_string k) !Options.opt_unsound_global)) locs in
  if can_strong_update mode global locs then Mem.strong_update locs v mem
  else Mem.weak_update locs v mem 

(* ********************************** *
 * Semantic functions for expressions *
 * ********************************** *)

let eval_const : Cil.constant -> Val.t = fun cst ->
  match cst with
  | Cil.CInt64 (i64, _, _) ->
    let itv = try Itv.of_int (Cil.i64_to_int i64) with _ -> Itv.top in
    Val.of_itv itv
  | Cil.CStr s -> invalid_arg ("evalOp.ml: eval_const string "^s)
  | Cil.CWStr s -> prerr_endline "evalOp.ml: eval_const wide string"; Val.of_itv Itv.top (* XXX *)
  | Cil.CChr c -> Val.of_itv (Itv.of_int (int_of_char c))
  (* Float numbers are modified to itvs.  If you want to make a
     precise and sound analysis for float numbers, you have to
     develop a domain for them. *)
  | Cil.CReal (f, _, _) ->
    Val.of_itv (Itv.of_ints (int_of_float (floor f)) (int_of_float (ceil f)))
  (* BatEnum is not evaluated correctly in our analysis. *)
  | Cil.CEnum _ -> Val.of_itv Itv.top

let eval_uop : Cil.unop -> Val.t -> Val.t = fun u v ->
  if Val.eq v Val.bot then Val.bot else
    let itv = Val.itv_of_val v in
    let itv' =
      match u with
      | Cil.Neg -> Itv.minus (Itv.of_int 0) itv
      | Cil.BNot -> Itv.b_not itv
      | Cil.LNot -> Itv.l_not itv in
    Val.of_itv itv'

let eval_bop : Cil.binop -> Val.t -> Val.t -> Val.t = fun b v1 v2 ->
  match b with
  | Cil.PlusA -> Val.of_itv (Itv.plus (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.PlusPI
  | Cil.IndexPI ->
    let ablk = Val.array_of_val v1 in
    let offset = Val.itv_of_val v2 in
    let ablk = ArrayBlk.plus_offset ablk offset in
    Val.join (Val.of_pow_loc (Val.pow_loc_of_val v1)) (Val.of_array ablk)
  | Cil.MinusA -> Val.of_itv (Itv.minus (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.MinusPI ->
    let ablk = Val.array_of_val v1 in
    let offset = Val.itv_of_val v2 in
    let ablk = ArrayBlk.minus_offset ablk offset in
    Val.join (Val.of_pow_loc (Val.pow_loc_of_val v1)) (Val.of_array ablk)
  | Cil.MinusPP -> 
    let offset1 = Val.array_of_val v1 |> ArrayBlk.offsetof in
    let offset2 = Val.array_of_val v2 |> ArrayBlk.offsetof in
    Itv.minus offset1 offset2 |> Val.of_itv
  | Cil.Mult -> Val.of_itv (Itv.times (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Div -> Val.of_itv (Itv.divide (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Mod -> Val.of_itv (Itv.modulo (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Shiftlt -> Val.of_itv (Itv.l_shift (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Shiftrt -> Val.of_itv (Itv.r_shift (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Lt -> Val.of_itv (Itv.lt_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Gt -> Val.of_itv (Itv.gt_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Le -> Val.of_itv (Itv.le_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Ge -> Val.of_itv (Itv.ge_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Eq -> Val.of_itv (Itv.eq_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.Ne -> Val.of_itv (Itv.ne_itv (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.BAnd -> Val.of_itv (Itv.b_and (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.BXor -> Val.of_itv (Itv.b_xor (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.BOr -> Val.of_itv (Itv.b_or (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.LAnd -> Val.of_itv (Itv.l_and (Val.itv_of_val v1) (Val.itv_of_val v2))
  | Cil.LOr -> Val.of_itv (Itv.l_or (Val.itv_of_val v1) (Val.itv_of_val v2))

let rec resolve_offset : Proc.t -> PowLoc.t -> ArrayBlk.t -> Cil.offset -> Mem.t -> PowLoc.t
= fun pid ploc arr os mem ->
  match os with
  | Cil.NoOffset -> ploc
  | Cil.Field (f, os') ->
    let ploc = ArrayBlk.append_field arr f in
    let arr = lookup ploc mem |> Val.array_of_val in
    resolve_offset pid ploc arr os' mem
  | Cil.Index (e, os') ->
    let _ = eval pid e mem in (* NOTE: to sync with access function *)
    let ploc = ArrayBlk.pow_loc_of_array arr in
    let arr = lookup ploc mem |> Val.array_of_val in 
    resolve_offset pid ploc arr os' mem
(* TODO: Syntax of lval in Cil is different to our design, but the
   essense is the same.  We can simply modify our design
   documentation. *)
and eval_lv : Proc.t -> Cil.lval -> Mem.t -> PowLoc.t
= fun pid lv  mem ->
  let (ploc,arr) =
    match fst lv with
    | Cil.Var vi ->
      let powloc = var_of_varinfo vi pid |> PowLoc.singleton in
      let arr = lookup powloc mem |> Val.array_of_val in (* XXX : is it necessary? *)
      (powloc, arr)
    | Cil.Mem e -> 
      let v = eval pid e mem in
      let ploc = PowLoc.join (Val.pow_loc_of_val v) (Val.array_of_val v |> ArrayBlk.pow_loc_of_array) in
      let arr = v |> Val.array_of_val in
      (ploc,arr)
  in
  PowLoc.remove Loc.null (resolve_offset pid ploc arr (snd lv) mem)

and var_of_varinfo vi pid  =
  if vi.Cil.vglob then Loc.of_gvar vi.Cil.vname 
  else Loc.of_lvar (pid, vi.Cil.vname)

and eval : Proc.t -> Cil.exp -> Mem.t -> Val.t
= fun pid e mem ->
  match e with
  | Cil.Const c -> eval_const c 
  | Cil.Lval l -> lookup (eval_lv pid l mem) mem 
  | Cil.SizeOf t -> Val.of_itv (try CilHelper.byteSizeOf t |> Itv.of_int with _ -> Itv.top)
  | Cil.SizeOfE e -> Val.of_itv (try CilHelper.byteSizeOf (Cil.typeOf e) |> Itv.of_int with _ -> Itv.top)
  | Cil.SizeOfStr s -> Val.of_itv (Itv.of_int (String.length s + 1))
  | Cil.AlignOf t -> Val.of_itv (Itv.of_int (Cil.alignOf_int t))
  (* TODO: type information is required for precise semantics of AlignOfE.  *)
  | Cil.AlignOfE _ -> Val.of_itv Itv.top
  | Cil.UnOp (u, e, _) -> eval_uop u (eval pid e mem)
  | Cil.BinOp (b, e1, e2, _) ->
    eval_bop b (eval pid e1 mem) (eval pid e2 mem)
  | Cil.Question (e1, e2, e3, _) -> 
    let i1 = Val.itv_of_val (eval pid e1 mem) in
    if Itv.is_bot i1 then
      Val.bot
    else if Itv.eq (Itv.of_int 0) i1 then
      eval pid e3 mem
    else if not (Itv.le (Itv.of_int 0) i1) then
      eval pid e2 mem
    else 
      Val.join (eval pid e2 mem) (eval pid e3 mem)
  | Cil.CastE (t, e) -> 
    let v = eval pid e mem in 
    (try Val.cast (Cil.typeOf e) t v with _ -> v)
  | Cil.AddrOf l -> Val.of_pow_loc (eval_lv pid l mem)
  | Cil.AddrOfLabel _ ->
    invalid_arg "itvSem.ml:eval AddrOfLabel mem. \
                 Analysis does not support label values."

  | Cil.StartOf l -> lookup (eval_lv pid l mem) mem


let eval_list : Proc.t -> Cil.exp list -> Mem.t -> Val.t list
= fun pid exps mem ->
  List.map (fun e -> eval pid e mem) exps

let eval_alloc : Node.t -> int -> Cil.exp -> bool -> Mem.t -> Val.t
= fun node position e is_static mem ->
  let pid = Node.get_pid node in 
  let allocsite = Allocsite.allocsite_of_node node position in
  let o = Itv.of_int 0 in
  let sz = Val.itv_of_val (eval pid e mem) in
  (* NOTE: stride is always one when allocating memory. *)
  let st = Itv.of_int 1 in
  let np = Itv.nat in
  let pow_loc = if is_static then PowLoc.bot else PowLoc.singleton Loc.null in
  let array = ArrayBlk.make allocsite o sz st np in
  Val.join (Val.of_pow_loc pow_loc) (Val.of_array array)

let eval_salloc : Node.t -> int -> string -> Mem.t -> Val.t
= fun node position s mem ->
  let allocsite = Allocsite.allocsite_of_string node position in
  let o = Itv.of_int 0 in
  let sz = Itv.of_int (String.length s + 1) in
  let st = Itv.of_int 1 in
  let np = Itv.of_int (String.length s) in
  let array = ArrayBlk.make allocsite o sz st np in
  Val.of_array array


let eval_string : string -> Val.t = fun s ->
  Val.of_itv Itv.nat

(* ****************************** *
 * Semantic functions for pruning *
 * ****************************** *)
let rev_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Gt
  | Gt -> Lt
  | Le -> Ge
  | Ge -> Le
  | Eq -> Eq
  | Ne -> Ne
  | _ -> invalid_arg "prune.ml: rev_binop"

let not_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | LAnd -> LOr
  | LOr -> LAnd
  | _ -> invalid_arg "prune.ml: rev_binop"

let rec make_cond_simple : exp -> exp option = fun cond ->
  match cond with
  | BinOp (op, CastE (_, e1), e2, t) 
  | BinOp (op, e1, CastE (_, e2), t) -> 
    let newe = BinOp (op, e1, e2, t) in
    make_cond_simple newe
  | BinOp (op, Lval _, _, _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some cond
  | BinOp (op, e, Lval x, t)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some (BinOp (rev_binop op, Lval x, e, t))
  | BinOp (op, BinOp (PlusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (MinusA, e, Lval y, t2), t))
  | BinOp (op, BinOp (MinusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (PlusA, e, Lval y, t2), t))
  | UnOp (LNot, BinOp (op, e1, e2, t2), _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    make_cond_simple (BinOp (not_binop op, e1, e2, t2))
  | UnOp (LNot, BinOp (op, e1, e2, t2), t1)
    when op = LAnd || op = LOr ->
    let not_e1 = UnOp (LNot, e1, t1) in
    let not_e2 = UnOp (LNot, e2, t1) in
    (match make_cond_simple not_e1, make_cond_simple not_e2 with
     | Some e1', Some e2' -> Some (BinOp (not_binop op, e1', e2', t2))
     | _, _ -> None)
  | UnOp (LNot, UnOp (LNot, e, _), _) -> make_cond_simple e
  | UnOp (LNot, Lval _, _) -> Some cond
  | Lval _ -> Some cond
  | _ -> None 

let rec prune_simple : update_mode -> Global.t -> Proc.t -> exp -> Mem.t
  -> Mem.t
= fun mode global pid cond mem ->
  match cond with
  | BinOp (op, Lval x, e, t)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    let x_lv = eval_lv pid x mem in
    if PowLoc.cardinal x_lv = 1 then 
      let x_v = lookup x_lv mem in
      let e_v = eval pid e mem |> Val.itv_of_val in
      let x_itv = Val.itv_of_val x_v in
      let x_ploc = Val.pow_loc_of_val x_v in
      let x_itv = Itv.prune op x_itv e_v in
      let x_ploc = PowLoc.prune op x_ploc e in
      let x_pruned = Val.make (x_itv, x_ploc, Val.array_of_val x_v, Val.pow_proc_of_val x_v) in
      update mode global x_lv x_pruned mem
    else mem
  | BinOp (op, e1, e2, _) when op = LAnd || op = LOr ->
    let mem1 = prune_simple mode global pid e1 mem in
    let mem2 = prune_simple mode global pid e2 mem in
    if op = LAnd then Mem.meet mem1 mem2 else Mem.join mem1 mem2
  | UnOp (LNot, Lval x, _) ->
    let x_lv = eval_lv pid x mem in
    if PowLoc.cardinal x_lv = 1 then 
      let x_v = lookup x_lv mem in
      let x_itv = Val.itv_of_val x_v in
      let e_v = Itv.zero in
      let x_itv = Itv.meet x_itv e_v in
      let x_pruned = Val.modify_itv x_itv x_v in
      update mode global x_lv x_pruned mem
    else mem
  | Lval x ->
    let x_lv = eval_lv pid x mem in
    if PowLoc.cardinal x_lv = 1 then 
      let x_v = lookup x_lv mem in
      let x_itv = Val.itv_of_val x_v in
      let pos_x = Itv.meet x_itv Itv.pos in
      let neg_x = Itv.meet x_itv Itv.neg in
      let x_itv = Itv.join pos_x neg_x in
      let x_pruned = Val.modify_itv x_itv x_v in
      update mode global x_lv x_pruned mem
    else mem
  | _ -> mem

let prune : update_mode -> Global.t -> Proc.t -> exp -> Mem.t -> Mem.t
= fun mode global pid cond mem ->
  match make_cond_simple cond with
  | None -> mem
  | Some cond_simple -> prune_simple mode global pid cond_simple mem

(* ******************************* *
 * Semantic functions for commands *
 * ******************************* *)
let mem_alloc_libs = ["__ctype_b_loc"; "initscr"; "newwin"; "localtime"; "__errno_location"; "opendir"; "readdir"]

let sparrow_print pid exps mem loc =
  if !Options.opt_verbose < 1 then ()
  else 
    let vs = eval_list pid exps mem in
    let vs_str = string_of_list Val.to_string vs in
    let exps_str = string_of_list CilHelper.s_exp exps in
    prerr_endline
      ("sparrow_print (" ^ exps_str ^ " @ " ^ CilHelper.s_location loc ^ ") : "
       ^ vs_str)

let sparrow_dump mem loc =
  if !Options.opt_verbose < 1 then ()
  else 
    prerr_endline
      ("sparrow_dump (" ^ CilHelper.s_location loc ^ ") : \n"
       ^ Mem.to_string mem)

let model_alloc_one mode pid lvo f (mem, global) =
  match lvo with
    None -> (mem, global)
  | Some lv -> 
    let allocsite = Allocsite.allocsite_of_ext (Some f.vname) in
    let arr_val = ItvDom.Val.of_array (ArrayBlk.make allocsite Itv.zero Itv.one Itv.one Itv.nat) in
    let ext_loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
    let mem = update mode global (eval_lv pid lv mem) arr_val mem in
    let mem = update mode global ext_loc Val.input_value mem in
    (mem,global)

let model_realloc ?(position=0) mode node (lvo, exps) (mem, global) =
  let pid = Node.get_pid node in
  match lvo with
  | Some lv ->
    begin
      match exps with
      | _::size::_ -> 
        (update mode global (eval_lv pid lv mem) (eval_alloc node position size false mem) mem, global)
      | _ -> raise (Failure "Error: arguments of realloc are not given")
    end
  | _ -> (mem,global)

let model_calloc ?(position=0) mode node (lvo, exps) (mem, global) =
  let pid = Node.get_pid node in
  match lvo with
  | Some lv ->
    begin
      match exps with
      | n::size::_ -> 
        let new_size = Cil.BinOp (Cil.Mult, n, size, Cil.uintType) in
        (update mode global (eval_lv pid lv mem) (eval_alloc node position new_size false mem) mem, global)
      | _ -> raise (Failure "Error: arguments of realloc are not given")
    end
  | _ -> (mem,global)

let model_scanf mode pid exps (mem, global) =
  match exps with 
    _::t -> 
      List.fold_left (fun (mem, global) e -> 
          match e with 
            Cil.AddrOf lv -> 
              let mem = update mode global (eval_lv pid lv mem) Val.input_value mem in
              (mem, global)
          | _ -> (mem,global)) (mem,global) t
  | _ -> (mem, global)
 
let model_strdup ?(position=0) mode node (lvo, exps) (mem, global) =
  let pid = Node.get_pid node in
  match (lvo, exps) with
  | (Some lv, str::_) ->
    let allocsite = Allocsite.allocsite_of_node node position in
    let str_val = eval pid str mem |> ItvDom.Val.array_of_val in
    let size = ArrayBlk.sizeof str_val in
    let null_pos = ArrayBlk.nullof str_val in
    let allocsites = ArrayBlk.pow_loc_of_array str_val in
    let offset = Itv.zero in
    let arr_val = ArrayBlk.make allocsite Itv.zero (Itv.minus size offset) Itv.one null_pos in 
    let loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
    let v = Val.join (Val.of_array arr_val) (Val.of_pow_loc loc) in
    let mem = update mode global (eval_lv pid lv mem) v mem in
    let mem = update mode global loc (lookup allocsites mem) mem in
    (mem,global)
  | _ -> (mem,global)

let model_input mode pid lvo (mem, global) =
  match lvo with
    Some lv -> 
      let allocsite = Allocsite.allocsite_of_ext None in
      let ext_v = Val.external_value allocsite in
      let ext_loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
      let mem = update mode global (eval_lv pid lv mem) ext_v mem in
      let mem = update mode global ext_loc Val.input_value mem in
        (mem,global)
  | _ -> (mem,global)

let model_assign mode pid (lvo,exps) (mem, global) =
  match (lvo,exps) with
    (Some lv, e::_) -> 
      (update mode global (eval_lv pid lv mem) (eval pid e mem) mem, global)
  | (_, _) -> (mem,global)


let model_strlen mode pid (lvo, exps) (mem, global) =
  match (lvo, exps) with
  | (Some lv, str::_) ->
    let str_val = eval pid str mem in
    let null_pos = ArrayBlk.nullof (ItvDom.Val.array_of_val str_val) in
    let v = Val.of_itv (Itv.meet Itv.nat null_pos) in
    (update mode global (eval_lv pid lv mem) v mem, global)
  | _ -> (mem,global)

let rec model_fgets mode pid (lvo, exps) (mem, global) =
  match (lvo, exps) with
  | (_, Lval buf::size::_) | (_, StartOf buf::size::_) ->
    let size_itv = eval pid size mem |> ItvDom.Val.itv_of_val in
    let buf_lv = eval_lv pid buf mem in
    let buf_arr = lookup buf_lv mem |> ItvDom.Val.array_of_val in
    let allocsites = ArrayBlk.pow_loc_of_array buf_arr in
    let buf_val = ArrayBlk.set_null_pos buf_arr (Itv.join Itv.zero size_itv) |> ItvDom.Val.of_array in
    mem 
    |> update mode global buf_lv buf_val
    |> update mode global allocsites Val.input_value
    |> (fun mem -> (mem, global))
  | (_, CastE (_, buf)::size::e) -> model_fgets mode pid (lvo, buf::size::e) (mem, global)
  | _ -> (mem,global)

let rec model_sprintf mode pid (lvo, exps) (mem, global) =
  match exps with
  | Lval buf::str::[] | StartOf buf::str::[] ->  (* format string *)
    let str_val = eval pid str mem |> ItvDom.Val.array_of_val in
    let (arrays, null_pos) = (ArrayBlk.pow_loc_of_array str_val, ArrayBlk.nullof str_val) in
    let buf_lv = eval_lv pid buf mem in
    let buf_arr = lookup buf_lv mem |> ItvDom.Val.array_of_val in
    let allocsites = ArrayBlk.pow_loc_of_array buf_arr in
    let buf_val = ArrayBlk.set_null_pos buf_arr null_pos |> ItvDom.Val.of_array in
    mem 
    |> update mode global buf_lv buf_val
    |> update mode global allocsites (lookup arrays mem)
    |> (match lvo with Some lv -> update mode global (eval_lv pid lv mem) (Val.of_itv null_pos) | _ -> id)
    |> (fun mem -> (mem, global))
  | CastE (_, buf)::str::[] 
  | buf::CastE (_, str)::[] -> model_sprintf mode pid (lvo, buf::str::[]) (mem, global)
  | _ -> 
    begin
      match lvo with
        Some lv -> (update mode global (eval_lv pid lv mem) (Val.of_itv Itv.nat) mem, global)
      | _ -> (mem,global)
    end

(* argc, argv *)
let sparrow_arg mode pid exps (mem,global) = 
  match exps with
    (Cil.Lval argc)::(Cil.Lval argv)::_ ->
      let argv_a = Allocsite.allocsite_of_ext (Some "argv") in 
      let argv_v = Val.make (Itv.bot,PowLoc.bot,ArrayBlk.make argv_a Itv.zero Itv.pos Itv.one Itv.nat,PowProc.bot) in
      let arg_a = Allocsite.allocsite_of_ext (Some "arg") in 
      let arg_v = Val.make (Itv.bot,PowLoc.bot,ArrayBlk.make arg_a Itv.zero Itv.pos Itv.one Itv.nat,PowProc.bot) in
      (update mode global (eval_lv pid argc mem) (Val.of_itv Itv.pos) mem
      |> update mode global (eval_lv pid argv mem) argv_v 
      |> update mode global (PowLoc.singleton (Loc.of_allocsite argv_a)) arg_v
      |> update mode global (PowLoc.singleton (Loc.of_allocsite arg_a)) (Val.of_itv Itv.top), global)
  | _ -> (mem,global)

(* optind, optarg *)
let sparrow_opt mode pid exps (mem,global) = 
  match exps with
    (Cil.Lval optind)::(Cil.Lval optarg)::_ ->
      let arg_a = Allocsite.allocsite_of_ext (Some "arg") in 
      let arg_v = Val.make (Itv.bot,PowLoc.bot,ArrayBlk.make arg_a Itv.zero Itv.pos Itv.one Itv.nat,PowProc.bot) in
      (update mode global (eval_lv pid optind mem) (Val.of_itv Itv.nat) mem
      |> update mode global (eval_lv pid optarg mem) arg_v 
      |> update mode global (PowLoc.singleton (Loc.of_allocsite arg_a)) (Val.of_itv Itv.top), global)
  | _ -> (mem,global)

let model_unknown mode node pid lvo f exps (mem, global) = 
  match lvo with 
    None -> (mem, global)
  | Some lv when Cil.isArithmeticType (Cil.unrollTypeDeep (Cil.typeOfLval lv)) -> 
      let ext_v = if CilHelper.is_unsigned (Cil.unrollTypeDeep (Cil.typeOfLval lv)) then 
                    Val.of_itv Itv.nat 
                  else Val.of_itv Itv.top 
      in
      let mem = update mode global (eval_lv pid lv mem) ext_v mem in
      (mem,global)
  | Some lv -> 
      let allocsite = Allocsite.allocsite_of_ext (Some f.vname) in
      let ext_v = ArrayBlk.extern allocsite |> ArrayBlk.cast_array (Cil.typeOfLval lv) |> Val.of_array in
      let ext_loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
      let mem = update mode global (eval_lv pid lv mem) ext_v mem in
      let mem = update mode global ext_loc Val.input_value mem in
      (mem,global)

let model_memcpy mode pid (lvo, exps) (mem, global) = 
  match (lvo, exps) with
    Some lv, dst::src::_ ->
      let (src_v, dst_v) = tuple mem |> Tuple2.map (eval pid src) (eval pid dst) in
      let (src_l, dst_l) = (src_v, dst_v) |> Tuple2.mapn Val.array_of_val
                           |> Tuple2.mapn ArrayBlk.pow_loc_of_array
      in
      mem 
      |> update mode global dst_l (lookup src_l mem)
      |> update mode global (eval_lv pid lv mem) dst_v
      |> (fun mem -> (mem, global))
  | _, _ -> (mem, global)

let model_getpwent ?(position=0) mode node pid lvo f (mem,global) = 
  match lvo, f.vtype with 
    Some lv, Cil.TFun ((Cil.TPtr ((Cil.TComp (comp, _) as elem_t), _) as ptr_t), _, _, _) -> 
      let struct_loc = eval_lv pid lv mem in
      let struct_v = eval_alloc node position (Cil.SizeOf elem_t) false mem |> Val.cast ptr_t (Cil.typeOfLval lv) in
      let field_loc = ArrayBlk.append_field (Val.array_of_val struct_v) (List.find (fun f -> f.fname ="pw_name") comp.cfields) in
      let allocsite = Allocsite.allocsite_of_ext (Some "getpwent.pw_name") in
      let ext_v = ArrayBlk.input allocsite |> Val.of_array in
      let ext_loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
      let mem = update mode global struct_loc struct_v mem 
              |> update mode global field_loc ext_v 
              |> update mode global ext_loc Val.input_value in
      (mem, global)
  | _ -> (mem, global) 

let rec model_strcpy mode node pid es (mem, global) =
  match es with
    (CastE (_, e))::t -> model_strcpy mode node pid (e::t) (mem,global)
  | dst::(CastE(_, e))::[] -> model_strcpy mode node pid (dst::e::[]) (mem,global)
  | (Lval dst)::(Lval src)::[] | (StartOf dst)::(StartOf src)::[]
  | (Lval dst)::(StartOf src)::[] | (StartOf dst)::(Lval src)::[] ->
      let src_arr = Val.array_of_val (lookup (eval_lv pid src mem) mem) in
      let dst_arr = Val.array_of_val (lookup (eval_lv pid dst mem) mem) in
      let np = ArrayBlk.nullof src_arr in
      let newv = Val.of_array (ArrayBlk.set_null_pos dst_arr np) in
      let mem = mem |> update mode global (eval_lv pid dst mem) newv in 
      (mem, global)
  | _ -> (mem, global)

let rec model_strncpy mode node pid es (mem, global) =
  match es with
    CastE (_, e)::t -> model_strncpy mode node pid (e::t) (mem, global)
  | (Lval dst)::_::size::_
  | (StartOf dst)::_::size::_ ->
      let arr = Val.array_of_val (lookup (eval_lv pid dst mem) mem) in
      let sz = Val.itv_of_val (eval pid size mem) in
      let np = Itv.join Itv.zero (Itv.minus sz Itv.one)in
      let newv = Val.of_array (ArrayBlk.set_null_pos arr np) in
      let mem = mem |> update mode global (eval_lv pid dst mem) newv in
      (mem, global)
  | _ -> (mem,global)


let rec model_strcat mode node pid es (mem, global) =
  match es with 
    (CastE (_, e))::t -> model_strcat mode node pid (e::t) (mem,global)
  | dst::(CastE(_, e))::[] -> model_strcat mode node pid (dst::e::[]) (mem,global)
  | (Lval dst)::(Lval src)::[] | (StartOf dst)::(StartOf src)::[]
  | (Lval dst)::(StartOf src)::[] | (StartOf dst)::(Lval src)::[] ->
      let src_arr = Val.array_of_val (lookup (eval_lv pid src mem) mem) in
      let dst_arr = Val.array_of_val (lookup (eval_lv pid dst mem) mem) in
      let np = ArrayBlk.nullof src_arr in
      let newv = Val.of_array (ArrayBlk.plus_null_pos dst_arr np) in
      let mem = mem |> update mode global (eval_lv pid dst mem) newv in 
      (mem, global)
  | _ -> (mem, global)

let rec model_strchr mode node pid (lvo, exps) (mem, global) =
  match lvo, exps with 
    Some _, (CastE (_, e))::t -> model_strchr mode node pid (lvo, e::t) (mem,global)
  | Some lv, (Lval str)::_ | Some lv, (StartOf str)::_ ->
      let str_arr = Val.array_of_val (lookup (eval_lv pid str mem) mem) in
      let np = ArrayBlk.nullof str_arr in
      let newv = Val.of_array (ArrayBlk.plus_offset str_arr np) in
      let mem = mem |> update mode global (eval_lv pid lv mem) newv in 
      (mem, global)
  | _, _ -> (mem, global)

let handle_undefined_functions ?position mode node pid (lvo,f,exps) (mem,global) loc = 
  match f.vname with
  | "sparrow_arg" -> sparrow_arg mode pid exps (mem,global)
  | "sparrow_opt" -> sparrow_opt mode pid exps (mem,global)
  | "sparrow_print" -> sparrow_print pid exps mem loc; (mem,global)
  | "sparrow_dump" -> sparrow_dump mem loc; (mem,global)
  | "sparrow_assume" -> (prune mode global pid (List.hd exps) mem, global)
  | "strlen" -> model_strlen mode pid (lvo, exps) (mem, global)
  | "realloc" -> model_realloc ?position mode node (lvo, exps) (mem, global)
  | "calloc" -> model_calloc ?position mode node (lvo, exps) (mem, global)
  | "fgets" -> model_fgets mode pid (lvo, exps) (mem, global)
  | "sprintf" -> model_sprintf mode pid (lvo, exps) (mem, global)
  | "scanf" -> model_scanf mode pid exps (mem, global)
  | "getenv" -> model_input mode pid lvo (mem, global)
  | "strdup" -> model_strdup ?position mode node (lvo, exps) (mem, global)
  | "gettext" -> model_assign mode pid (lvo, exps) (mem, global)
  | "memcpy" -> model_memcpy mode pid (lvo, exps) (mem, global)
  | "getpwent" -> model_getpwent ?position mode node pid lvo f (mem,global)
  | "strcpy" -> model_strcpy mode node pid exps (mem, global)
  | "strncpy" -> model_strncpy mode node pid exps (mem, global)
  | "strcat" -> model_strcat mode node pid exps (mem, global)
  | "strchr" | "strrchr" -> model_strchr mode node pid (lvo, exps) (mem, global)
  | s when List.mem s mem_alloc_libs -> model_alloc_one mode pid lvo f (mem, global)
  | _ -> model_unknown mode node pid lvo f exps (mem, global)

let bind_lvar : update_mode -> Global.t -> Loc.t -> Val.t -> Mem.t -> Mem.t
= fun mode global lvar v mem ->
  let l = PowLoc.singleton lvar in
  update mode global l v mem

let bind_arg_ids : update_mode -> Global.t -> Val.t list -> Loc.t list -> Mem.t -> Mem.t
= fun mode global vs arg_ids mem ->
  list_fold2 (bind_lvar mode global) arg_ids vs mem

(* Binds a list of values to a set of argument lists.  If |args_set|
    > 1, the argument binding does weak update. *)
let bind_arg_lvars_set : update_mode -> Global.t -> (Loc.t list) BatSet.t -> Val.t list -> Mem.t -> Mem.t
= fun mode global arg_ids_set vs mem ->
  let is_same_length l = List.length l = List.length vs in
  let arg_ids_set = BatSet.filter is_same_length arg_ids_set in
  let mode = if BatSet.cardinal arg_ids_set > 1 then AbsSem.Weak else mode in
  BatSet.fold (bind_arg_ids mode global vs) arg_ids_set mem

let rec run_cmd ?(position=0) mode node cmd (mem,global) = 
  let pid = Node.get_pid node in
  match cmd with 
  | IntraCfg.Cmd.Cinstr _ -> invalid_arg "absSem.ml: run Cinstr" 
  | IntraCfg.Cmd.Cif _ -> invalid_arg "absSem.ml: run Cif" 
  | IntraCfg.Cmd.CLoop _ -> invalid_arg "absSem.ml: run CLoop" 
  | IntraCfg.Cmd.Cseq l -> List.fold_left (fun ((mem,global),pos) c -> 
      (run_cmd ~position:pos mode node c (mem,global), pos+1)) ((mem,global),0) l |> fst
  | IntraCfg.Cmd.Cset (l, e, loc) ->
      (update mode global (eval_lv pid l mem) (eval pid e mem) mem, global)
  | IntraCfg.Cmd.Cexternal (l, _) -> 
    (match Cil.typeOfLval l with
       Cil.TInt (_, _) | Cil.TFloat (_, _) ->
         let ext_v = Val.of_itv Itv.top in
         let mem = update mode global (eval_lv pid l mem) ext_v mem in
         (mem,global)
     | _ -> 
       let allocsite = Allocsite.allocsite_of_ext None in
       let ext_v = Val.external_value allocsite in
       let ext_loc = PowLoc.singleton (Loc.of_allocsite allocsite) in
       let mem = update mode global (eval_lv pid l mem) ext_v mem in
       let mem = update mode global ext_loc ext_v mem in
        (mem,global))
  | IntraCfg.Cmd.Calloc (l, a, is_static, loc) ->
    (update mode global (eval_lv pid l mem) (eval_alloc node position a is_static mem) mem, global)
  | IntraCfg.Cmd.Csalloc (l, s, loc) ->
    let str_loc = 
      Allocsite.allocsite_of_string node position 
      |> Loc.of_allocsite |> PowLoc.singleton
    in
    mem 
    |> update mode global (eval_lv pid l mem) (eval_salloc node position s mem)
    |> update mode global str_loc (eval_string s)
    |> (fun mem -> (mem, global))
  | IntraCfg.Cmd.Cfalloc (l, fd, _) -> 
    let clos = Val.of_pow_proc (PowProc.singleton fd.svar.vname) in
    (update mode global (eval_lv pid l mem) clos mem, global)
  | IntraCfg.Cmd.Cassume (e, _) -> 
    let _ = eval pid e mem in (* for inspection *)
    (prune mode global pid e mem, global)
  | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f, Cil.NoOffset), arg_exps, loc)
    when Global.is_undef f.vname global -> (* undefined library functions *)
    if BatSet.mem ((CilHelper.s_location loc)^":"^f.vname) !Options.opt_unsound_lib then (mem,global)
    else
      let _ = eval_list pid arg_exps mem in (* for inspection *) 
      handle_undefined_functions ~position mode node pid (lvo,f,arg_exps) (mem,global) loc
  | IntraCfg.Cmd.Ccall (lvo, f, arg_exps, _) -> (* user functions *)
    let fs = Val.pow_proc_of_val (eval pid f mem) in
    if PowProc.eq fs PowProc.bot 
    then (mem, global)
    else
      let arg_lvars_of_proc f acc =
        let args = InterCfg.argsof global.icfg f in
        let lvars = List.map (fun x -> Loc.of_lvar (f,x)) args in
        BatSet.add lvars acc in
      let arg_lvars_set = PowProc.fold arg_lvars_of_proc fs BatSet.empty in
      let arg_vals = eval_list pid arg_exps mem in
      let dump =
         match lvo with
         | None -> global.dump
         | Some lv ->
           PowProc.fold (fun f d ->
              Dump.weak_add f (eval_lv pid lv mem) d
           ) fs global.dump in
      let mem = bind_arg_lvars_set mode global arg_lvars_set arg_vals mem in
      (mem, {global with dump = dump})
  | IntraCfg.Cmd.Creturn (ret_opt, _) ->
      (match ret_opt with
      | None -> mem
      | Some e ->
        Dump.find pid global.dump
        |> (fun ret_locs -> update Weak global ret_locs (eval pid e mem) mem))
      |> remove_local_variables mode global pid
      |> (fun mem -> (mem, global))
  | IntraCfg.Cmd.Casm _ -> (mem, global)    (* Not supported *)
  | IntraCfg.Cmd.Cskip -> (mem, global)

and remove_local_variables : update_mode -> Global.t -> Proc.t -> Mem.t -> Mem.t
= fun mode global pid mem ->
  match mode with
  | Weak -> mem
  | Strong ->
    if Global.is_rec pid global then mem 
    else
      let locals = Mem.filter (fun x _ -> Loc.is_local_of pid x) mem |> Mem.keys in
      update mode global locals Val.bot mem 

(* Default update option is weak update. *)
let rec run ?(mode = Weak) ?(locset = BatSet.empty) ?(ptrinfo = Table.bot)
    : Node.t -> Mem.t * Global.t -> Mem.t * Global.t
= fun node (mem, global) ->
  run_cmd mode node (InterCfg.cmdof global.icfg node) (mem,global)

let accessof ?(mode = Weak) ?(locset = BatSet.empty) ?(ptrinfo = Table.bot): Global.t -> Node.t -> Mem.t -> Access.t 
= fun global node mem -> 
  init_access ();
  let _ = run ~mode:mode node (mem,global) in
  return_access ()

(* ***************************** *
 * Flow-insensitive pre-analysis *
 * ***************************** *)
let onestep_transfer : Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes (mem,global) ->
  list_fold (fun node (mem,global) ->
    run node (mem,global)
  ) nodes (mem,global) 

let rec fixpt : Node.t list -> int -> Mem.t * Global.t -> Mem.t * Global.t
=fun nodes k (mem,global) ->
  my_prerr_string ("\riteration : " ^ string_of_int k);
  flush stderr;
  let (mem',global') = onestep_transfer nodes (mem,global) in
  let mem' = Mem.widen mem mem' in
    if Mem.le mem' mem && Dump.le global'.dump global.dump
    then (my_prerr_newline (); (mem',global'))
    else fixpt nodes (k+1) (mem',global')

let callees_of : InterCfg.t -> InterCfg.Node.t -> Mem.t -> PowProc.t
= fun icfg node mem ->
  let pid = InterCfg.Node.get_pid node in
  let c = InterCfg.cmdof icfg node in
  match c with
  | IntraCfg.Cmd.Ccall (_, e, _, _) ->
    Val.pow_proc_of_val (eval pid e mem)
  | _ -> PowProc.bot

let draw_call_edges : InterCfg.Node.t list -> Mem.t -> Global.t -> Global.t
= fun nodes mem global ->
  let icfg = Global.get_icfg global in
  Global.update_icfg 
    (List.fold_left (fun icfg node ->
       if InterCfg.is_callnode node icfg then
         let callees = callees_of icfg node mem in
         PowProc.fold (InterCfg.add_call_edge node) callees icfg
       else icfg) icfg nodes) global

let draw_callgraph : Node.t list -> Mem.t -> Global.t -> Global.t
=fun nodes mem global ->
  let icfg = Global.get_icfg global in
  let callgraph = Global.get_callgraph global in
  let callgraph = List.fold_left (fun callgraph node ->
      let callees = callees_of icfg node mem in
      PowProc.fold (fun callee callgraph ->
        CallGraph.add_edge (InterCfg.Node.get_pid node) callee callgraph) callees callgraph)
    callgraph nodes
    |> CallGraph.init_trans_calls
  in
  Global.update_callgraph callgraph global

let do_preanalysis ?(locset=BatSet.empty) ?(ptrinfo=Table.empty) : Global.t -> Dom.t * Global.t
= fun global -> 
  let nodes = InterCfg.nodesof (get_icfg global) in
  let (mem, global) = fixpt nodes 1 (Mem.bot,global) in
  let _ = my_prerr_endline ("mem size : " ^ i2s(Mem.fold (fun _ c -> c+1) mem 0)) in
  (mem,
   global 
  |> draw_call_edges nodes mem
  |> draw_callgraph nodes mem
  |> Global.remove_unreachable_functions
  |> Global.update_mem mem)
