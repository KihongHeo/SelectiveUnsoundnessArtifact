open Cil
open Vocab
open AbsSem
open ItvDom
open Global
open IntraCfg
open InterCfg
open BasicDom
open OctDom
open BatTuple
open Apron

module Dom = OctDom.Mem
module Access = Access.Make(Dom)

(* ************************* *
 * Accessibility information *
 * ************************* *)
let access = ref Access.empty

let access_mode = ref false
let init_access : unit -> unit 
= fun () -> access_mode := true; access := Access.empty; ()

let return_access : unit -> Access.t
= fun () -> access_mode := false; !access

(* ********************** *
 * Abstract memory access *
 * ********************** *)
let can_strong_update : update_mode -> Global.t -> (OctLoc.t * Pack.t * Octagon.t) list -> bool
= fun mode global oct_list -> 
  match mode, oct_list with 
  | Weak, _ -> false
  | Strong, (octlv, pack, oct)::[] ->
    (match octlv with 
      OctLoc.Loc lv -> Loc.is_gvar lv || (Loc.is_lvar lv && not (Global.is_rec (Loc.get_proc lv) global))
    | _ -> true)
  | _ -> false

let lookup : Pack.t -> Mem.t -> Octagon.t
= fun pack mem ->
  (if !access_mode && not (Mem.eq mem Mem.bot) then 
    access := Access.add Access.use pack !access);
  Mem.find pack mem

let update : update_mode -> Global.t -> (OctLoc.t * Pack.t * Octagon.t) list -> Mem.t -> Mem.t 
= fun mode global oct_list mem -> 
  (if !access_mode then
    if can_strong_update mode global oct_list then 
      List.iter (fun (_, pack, _) -> access := Access.add Access.def pack !access) oct_list
    else 
      List.iter (fun (_, pack, _) -> access := Access.add Access.all pack !access) oct_list);
  if oct_list = [] then mem  
  else if can_strong_update mode global oct_list then 
    List.fold_left (fun mem (_, pack, oct) -> Mem.add pack oct mem) mem oct_list
  else 
    List.fold_left (fun mem (_, pack, oct) -> Mem.weak_add pack oct mem) mem oct_list

(* ********************* *
 * Octagonal exptression *
 * ********************* *)
let itv_to_interval i =
  let f = function 
    | Itv.Integer.MInf -> Scalar.of_infty (-1)
    | Itv.Integer.PInf -> Scalar.of_infty 1
    | Itv.Integer.Int x -> Scalar.of_int x
  in
  if i = Itv.bot then Interval.bottom
  else Interval.of_scalar (f (Itv.lower_integer i)) (f (Itv.upper_integer i))

let octloc_to_texpr : OctLoc.t -> Texpr1.expr
= fun l -> Texpr1.Var (Apron.Var.of_string (OctLoc.to_string l))

let const_to_texpr : Cil.constant -> Texpr1.expr = function  
  | Cil.CInt64 (i, _, _) -> Texpr1.Cst (Coeff.s_of_int (Int64.to_int i))
  | Cil.CStr s -> invalid_arg ("octSem.ml: const_to_texpr string "^s)
  | Cil.CWStr s -> invalid_arg "octSem.ml: const_to_texpr wide string"
  | Cil.CChr c -> Texpr1.Cst (Coeff.s_of_int (int_of_char c))
  | Cil.CReal (f, _, _) -> Texpr1.Cst (Coeff.s_of_int (int_of_float (ceil f)))
  (* BatEnum is not evaluated correctly in our analysis. *)
  | Cil.CEnum _ -> Texpr1.Cst (Coeff.Interval Interval.top)
 
let lval_to_texpr : Proc.t -> PackConf.t -> Pack.t -> Cil.lval -> ItvDom.Mem.t -> Mem.t -> Texpr1.expr list
= fun pid packconf pack lv premem mem ->
  let lvset = ItvSem.eval_lv pid lv premem in
  let oct = lookup pack mem in
  PowLoc.fold (fun x el ->
      let x = OctLoc.of_loc x in 
      let rv_pack = PackConf.get_pack packconf x in
      let rv_oct = lookup rv_pack mem in
      if Pack.mem x pack && not (Octagon.is_bot oct) then 
        (Texpr1.Var (Apron.Var.of_string (OctLoc.to_string x)))::el
      else if not (Octagon.is_bot rv_oct) then
        (Texpr1.Cst (Coeff.Interval (Octagon.itv_of_var x rv_oct |> itv_to_interval)))::el
      else el) lvset []

(* XXX : Cil.bitsSizeOf often fails: just return top for the moment
 * Adhoc solution: To avoid this failure, translate original C sources 
 * into "CIL" (using -il option) and analyze the CIL program. *)
let sizeof_to_texpr : Cil.typ -> Texpr1.expr 
=fun typ ->  
  try Texpr1.Cst (Coeff.s_of_int ((Cil.bitsSizeOf typ) / 8))
  with _ -> 
    prerr_endline ("warn: Cil.bitsSizeOf (" ^ CilHelper.s_type typ ^ ")");
    Texpr1.Cst (Coeff.Interval Interval.top)

let rec exp_to_texpr : Proc.t -> PackConf.t -> Pack.t -> Cil.exp -> ItvDom.Mem.t -> Mem.t -> Texpr1.expr list
= fun pid packconf pack e ptrmem mem ->
  match e with 
    Cil.Const c -> [const_to_texpr c]
  | Cil.Lval l -> lval_to_texpr pid packconf pack l ptrmem mem
  | Cil.SizeOf t -> [sizeof_to_texpr t]
  | Cil.SizeOfE e -> [sizeof_to_texpr (Cil.typeOf e)]
  | Cil.SizeOfStr s -> [Texpr1.Cst (Coeff.s_of_int (String.length s + 1))]
  | Cil.AlignOfE _ -> [Texpr1.Cst (Coeff.Interval Interval.top)]
  | Cil.CastE (_, e) -> exp_to_texpr pid packconf pack e ptrmem mem
  | Cil.UnOp (uop, e, _) -> uop_to_texpr pid packconf pack uop e ptrmem mem
  | Cil.BinOp (bop, e1, e2, _) -> binop_to_texpr pid packconf pack bop e1 e2 ptrmem mem
  | _ -> []

and uop_to_texpr pid packconf pack uop e ptrmem mem = 
  let e_list = exp_to_texpr pid packconf pack e ptrmem mem in
  match uop with 
    Cil.Neg -> List.map (fun x -> Texpr1.Unop (Texpr1.Neg, x, Texpr1.Int, Texpr1.Near)) e_list
  | Cil.BNot | Cil.LNot -> List.map (fun _ -> Texpr1.Cst (Coeff.Interval Interval.top)) e_list

and binop_to_texpr pid packconf pack bop e1 e2 ptrmem mem = 
  let e1_list = exp_to_texpr pid packconf pack e1 ptrmem mem in
  let e2_list = exp_to_texpr pid packconf pack e2 ptrmem mem in
  List.fold_left (fun l x ->
    List.fold_left (fun l y ->
      match bop with 
        Cil.PlusA | Cil.MinusA | Cil.Mult | Cil.Div -> 
          (Texpr1.Binop (bop_to_texpr bop, x, y, Texpr1.Int, Texpr1.Near))::l
      | Cil.BAnd | Cil.BXor | Cil.BOr | Cil.Shiftlt | Cil.Shiftrt -> 
          (Texpr1.Cst (Coeff.Interval Interval.top))::l
      | _ -> l) l e2_list) [] e1_list

and bop_to_texpr = function 
  | Cil.PlusA -> Texpr1.Add
  | Cil.MinusA -> Texpr1.Sub
  | Cil.Mult -> Texpr1.Mul
  | Cil.Div -> Texpr1.Div
  | _ -> invalid_arg "octSem.ml: bio_to_texpr"

let normalize : Cil.binop -> OctLoc.t -> Texpr1.expr -> (Texpr1.expr * Tcons1.typ) 
= fun bop lv texpr -> 
  let lv = OctLoc.to_var lv in
  match bop with 
    Cil.Lt ->
      (Texpr1.Binop 
        (Texpr1.Sub,
           Texpr1.Binop (Texpr1.Sub, texpr, Texpr1.Var lv, Texpr1.Int, Texpr1.Near),
           Texpr1.Cst (Coeff.s_of_int 1),
           Texpr1.Int,
           Texpr1.Near), Tcons1.SUPEQ)
  | Cil.Le ->
      (Texpr1.Binop (Texpr1.Sub, texpr, Texpr1.Var lv, Texpr1.Int, Texpr1.Near), Tcons1.SUPEQ)
  | Cil.Gt -> 
      (Texpr1.Binop 
         (Texpr1.Sub,
            Texpr1.Binop (Texpr1.Sub, Texpr1.Var lv, texpr, Texpr1.Int, Texpr1.Near),
            Texpr1.Cst (Coeff.s_of_int 1),
            Texpr1.Int,
            Texpr1.Near), Tcons1.SUPEQ)
  | Cil.Ge ->
      (Texpr1.Binop (Texpr1.Sub, Texpr1.Var lv, texpr, Texpr1.Int, Texpr1.Near), Tcons1.SUPEQ)
  | Cil.Eq -> 
      (Texpr1.Binop (Texpr1.Sub, Texpr1.Var lv, texpr, Texpr1.Int, Texpr1.Near), Tcons1.EQ)
  | Cil.Ne -> 
      (Texpr1.Binop (Texpr1.Sub, Texpr1.Var lv, texpr, Texpr1.Int, Texpr1.Near), Tcons1.DISEQ)
  | _ -> raise (Failure "normalize")

let cond_to_texpr : Proc.t -> PackConf.t -> Pack.t -> OctLoc.t -> Cil.binop -> Cil.exp 
  -> ItvDom.Mem.t -> Mem.t -> (Texpr1.expr * Tcons1.typ) list
= fun pid packconf pack lv bop e ptrmem mem -> 
  exp_to_texpr pid packconf pack e ptrmem mem
  |> List.map (normalize bop lv)

(* natural number : [0, +oo] *)
let nat_texpr = Texpr1.Cst (Coeff.Interval (Interval.of_scalar (Scalar.of_int 0) (Scalar.of_infty 1)))
let strlen_texpr_set : Proc.t -> PackConf.t -> Pack.t -> Cil.exp -> ItvDom.Mem.t -> Mem.t -> Texpr1.expr list
= fun pid packconf pack exp ptrmem mem ->
  let set = ItvSem.eval pid exp ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
  if PowOctLoc.for_all (fun x ->
      let rv_pack = PackConf.get_pack packconf x in
      let rv_oct = lookup rv_pack mem in
      not (Octagon.is_bot rv_oct)) set
  then [nat_texpr]
  else []

let strlen_texpr_prune : Proc.t -> PackConf.t -> Pack.t -> OctLoc.t -> Cil.exp -> ItvDom.Mem.t 
  -> Mem.t -> (Texpr1.expr * Tcons1.typ) list
= fun pid packconf pack lv exp ptrmem mem ->
  let set = ItvSem.eval pid exp ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
  PowOctLoc.fold (fun x el -> 
      let rv_pack = PackConf.get_pack packconf x in
      let rv_oct = lookup rv_pack mem in
      if Pack.mem x pack && not (Octagon.is_bot rv_oct) then (octloc_to_texpr x)::el
      else el) set []
  |> List.map (normalize Cil.Lt lv)

(* ************************** *
 * Abstract semantic function *
 * ************************** *)
let rec simplify_exp = function
    Cil.CastE (_, e) -> simplify_exp e
  | Cil.BinOp (Cil.Mult, Cil.SizeOfE _, e1, _)
  | Cil.BinOp (Cil.Mult, e1, Cil.SizeOfE _, _)
  | Cil.BinOp (Cil.Mult, Cil.SizeOf _, e1, _)
  | Cil.BinOp (Cil.Mult, e1, Cil.SizeOf _, _) -> simplify_exp e1
  | Cil.BinOp (b, e1, e2, t) -> Cil.BinOp(b, simplify_exp e1, simplify_exp e2, t)
  | Cil.UnOp (u, e, t) -> Cil.UnOp (u, simplify_exp e, t)
  | e -> e 

let set : update_mode -> Global.t -> ItvDom.Mem.t -> PackConf.t -> Proc.t -> PowOctLoc.t 
  -> Cil.exp -> Dom.t -> Dom.t
= fun mode global ptrmem packconf pid lv_set e mem ->
    let simple_e = simplify_exp e in
    if try Cil.isIntegralType (Cil.typeOf simple_e) with _ -> false then
    begin
      PowOctLoc.fold (fun lv l ->
        let pack = PackConf.get_pack packconf lv in
        let old_oct = lookup pack mem in
        let texpr_list = exp_to_texpr pid packconf pack simple_e ptrmem mem in
        List.fold_left (fun l texpr ->
          (lv, pack, Octagon.set lv texpr old_oct)::l) l texpr_list) lv_set [] 
      |> (fun l -> update mode global l mem)
    end
    else mem

let forget : update_mode -> Global.t -> PackConf.t -> Proc.t -> PowOctLoc.t -> Dom.t -> Dom.t
= fun mode global packconf pid lv_set mem ->
  PowOctLoc.fold (fun lv l ->
    let pack = PackConf.get_pack packconf lv in
    let old_oct = lookup pack mem in
    let new_oct = Octagon.forget lv old_oct in
    (lv, pack, new_oct)::l) lv_set []
  |> (fun l -> update mode global l mem) 

let alloc : update_mode -> Global.t -> ItvDom.Mem.t -> PackConf.t -> Proc.t -> PowOctLoc.t
  -> PowOctLoc.t -> Cil.exp -> Dom.t -> Dom.t 
= fun mode global ptrmem packconf pid lv ptrs e mem ->
  set mode global ptrmem packconf pid ptrs e mem 

let rec prune : update_mode -> Global.t -> ItvDom.Mem.t -> PackConf.t -> Proc.t -> Cil.exp -> Dom.t -> Dom.t
= fun mode global ptrmem packconf pid exp mem ->
  match exp |> simplify_exp |> ItvSem.make_cond_simple with
    None -> mem
  | Some (Cil.BinOp (bop, Cil.Lval lval, e, _)) 
    when bop = Lt || bop = Gt || bop = Le || bop = Ge || bop = Eq || bop = Ne ->
      let lv_set = ItvSem.eval_lv pid lval ptrmem |> PowOctLoc.of_locs in 
      PowOctLoc.fold (fun lv l -> 
        let pack = PackConf.get_pack packconf lv in
        let old_oct = lookup pack mem in
        let texpr_list = cond_to_texpr pid packconf pack lv bop e ptrmem mem in
        List.fold_left (fun l (texpr, typ) ->
          (lv,pack, Octagon.prune lv texpr typ old_oct)::l) l texpr_list) lv_set []
      |> (fun l -> update mode global l mem)
  | Some (Cil.UnOp (LNot, Lval x, t)) -> 
      prune mode global ptrmem packconf pid (Cil.BinOp (Eq, Lval x, Cil.zero, t)) mem
  | Some (Lval x) -> 
      prune mode global ptrmem packconf pid (Cil.BinOp (Gt, Lval x, Cil.zero, Cil.intType)) mem
  | _ -> mem

let sparrow_print : ItvDom.Mem.t -> PackConf.t -> Proc.t -> Cil.exp list -> Dom.t -> Cil.location -> unit
= fun ptrmem packconf pid exps mem loc ->  
  match exps with 
    Lval lv::_ ->
      ItvSem.eval_lv pid lv ptrmem
      |> PowOctLoc.of_locs
      |> PowOctLoc.iter (fun lv  ->
          let pack = PackConf.get_pack packconf lv in
          let oct = lookup pack mem in
          prerr_endline ("sparrow_print (" ^ CilHelper.s_location loc ^ ") : "^ Octagon.to_string oct))
  | _ -> ()

let model_strlen mode packconf node pid lvo exps ptrmem (mem, global) =
  match lvo, exps with 
  | (Some lv, (str::_)) ->  
    let lv_set = ItvSem.eval_lv pid lv ptrmem |> PowOctLoc.of_locs in 
    let mem = 
      PowOctLoc.fold (fun lv l -> 
        let pack = PackConf.get_pack packconf lv in
        let old_oct = lookup pack mem in
        let texpr_list = strlen_texpr_set pid packconf pack str ptrmem mem in
        List.fold_left (fun l texpr ->
          (lv, pack, Octagon.set lv texpr old_oct)::l) l texpr_list) lv_set []
      |> (fun l -> update mode global l mem)
    in
    let l = PowOctLoc.fold (fun lv l -> 
        let pack = PackConf.get_pack packconf lv in
        let old_oct = lookup pack mem in
        let texpr_list = strlen_texpr_prune pid packconf pack lv str ptrmem mem in
        List.fold_left (fun l (texpr, typ) ->
          (lv, pack, Octagon.prune lv texpr typ old_oct)::l) l texpr_list) lv_set []
    in 
    update mode global l mem
  | _ -> mem

let sparrow_arg mode packconf pid exps ptrmem (mem,global) = 
  match exps with
    (Cil.Lval argc)::(Cil.Lval argv)::_ ->
      let lv = ItvSem.eval_lv pid argv ptrmem |> PowOctLoc.of_locs in
      let argc_lv = ItvSem.eval_lv pid argc ptrmem |> PowOctLoc.of_locs in
      let argv_a = Allocsite.allocsite_of_ext (Some "argv") |> OctLoc.of_size |> PowOctLoc.singleton in
      let arg_a = Allocsite.allocsite_of_ext (Some "arg") |> OctLoc.of_size |> PowOctLoc.singleton in 
      mem
      |> forget mode global packconf pid argc_lv
      |> prune mode global ptrmem packconf pid (Cil.BinOp (Cil.Ge, Cil.Lval argc, Cil.one, Cil.intType))
      |> alloc mode global ptrmem packconf pid lv argv_a (Cil.Lval argc)
      |> forget mode global packconf pid arg_a 
  | _ -> mem

let model_realloc mode packconf node pid position lvo exps ptrmem (mem, global) =
  match lvo, exps with
    (Some l, _::size::_) ->
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_alloc node position size false ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem packconf pid lv ptrs size mem
  | _ -> mem

let model_calloc mode packconf node pid position lvo exps ptrmem (mem, global) = 
  match lvo, exps with
    (Some l, size::_) ->
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_alloc node position size false ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem packconf pid lv ptrs size mem
  | _ -> mem

let strdup_texpr : Proc.t -> PackConf.t -> Pack.t -> Cil.exp -> ItvDom.Mem.t -> Mem.t -> Texpr1.expr list
= fun pid packconf pack exp ptrmem mem ->
  let set = ItvSem.eval pid exp ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
  PowOctLoc.fold (fun x l ->       
      let rv_pack = PackConf.get_pack packconf x in
      let rv_oct = lookup rv_pack mem in
      if Pack.mem x pack && not (Octagon.is_bot rv_oct) then (octloc_to_texpr x)::l
      else if not (Octagon.is_bot rv_oct) then nat_texpr::l
      else l 
  ) set []
        
let model_strdup mode packconf node pid position lvo exps ptrmem (mem,global) = 
  match (lvo, exps) with
    (Some lv, str::_) ->
      let lv = Allocsite.allocsite_of_node node position |> OctLoc.of_size in
      let pack = PackConf.get_pack packconf lv in
      let old_oct = lookup pack mem in
      let texpr_list = strdup_texpr pid packconf pack str ptrmem mem in
      List.fold_left (fun l texpr ->
        (lv, pack, Octagon.set lv texpr old_oct)::l) [] texpr_list
      |> (fun l -> update mode global l mem)
  | (_, _) -> mem

let model_input mode packconf pid lvo ptrmem (mem,global) =
  match lvo with 
    Some lv -> 
      let size = Allocsite.allocsite_of_ext None |> OctLoc.of_size |> PowOctLoc.singleton in
      forget mode global packconf pid size mem
  | _ -> mem

let model_unknown mode packconf node pid lvo f exps ptrmem (mem, global) = 
  match lvo with 
    None -> mem
  | Some lv when Cil.isArithmeticType (Cil.unrollTypeDeep (Cil.typeOfLval lv)) ->
    let lv = ItvSem.eval_lv pid lv ptrmem in
    let oct_lv = PowOctLoc.of_locs lv in 
    forget mode global packconf pid oct_lv mem
  | Some lv -> 
    let size = Allocsite.allocsite_of_ext (Some f.vname) |> OctLoc.of_size |> PowOctLoc.singleton  in
    let loc  = Allocsite.allocsite_of_ext (Some f.vname) |> Loc.of_allocsite |> OctLoc.of_loc |> PowOctLoc.singleton  in
    mem
    |> forget mode global packconf pid size 
    |> forget mode global packconf pid loc

let handle_undefined_functions mode packconf node pid position (lvo,f,exps) ptrmem (mem,global) loc = 
  match f.vname with
  | "sparrow_print" -> sparrow_print ptrmem packconf pid exps mem loc; mem
  | "sparrow_arg" -> sparrow_arg mode packconf pid exps ptrmem (mem,global)
  | "strlen" -> model_strlen mode packconf node pid lvo exps ptrmem (mem, global)
  | "realloc" -> model_realloc mode packconf node pid position lvo exps ptrmem (mem, global)
  | "calloc" -> model_calloc mode packconf node pid position lvo exps ptrmem (mem, global)
  | "getenv" -> model_input mode packconf pid lvo ptrmem (mem,global)
  | "strdup" -> model_strdup mode packconf node pid position lvo exps ptrmem (mem, global)
  | _ -> model_unknown mode packconf node pid lvo f exps ptrmem (mem, global)

let binding : update_mode -> Global.t -> ItvDom.Mem.t -> PackConf.t -> Proc.t -> (Loc.t list) BatSet.t -> Cil.exp list -> Dom.t -> Dom.t
= fun mode global ptrmem packconf pid paramset args mem ->
  let rec adjust params args new_params new_args =
    match (params, args) with
      (h1::t1, h2::t2) -> adjust t1 t2 (h1::new_params) (h2::new_args)
    | ([], _) | (_, []) -> (new_params, new_args)
  in
  let mode = if BatSet.cardinal paramset > 1 then Weak else mode in
  BatSet.fold (fun params mem ->
      let (params, args) = adjust params args [] [] in
      List.fold_left2 (fun mem param arg -> 
        let param = PowLoc.singleton param |> PowOctLoc.of_locs in
        set mode global ptrmem packconf pid param arg mem) mem params args
  ) paramset mem

let rec run_cmd ?(position=0) mode packconf node cmd ptrmem (mem,global) =
  let pid = Node.get_pid node in
  match cmd with
    IntraCfg.Cmd.Cset (l, e, _) ->
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in 
      set mode global ptrmem packconf pid lv e mem
  | IntraCfg.Cmd.Cexternal (l, _) ->
      let lv = ItvSem.eval_lv pid l ptrmem in
      let oct_lv = PowOctLoc.of_locs lv in
      let (arr_val, arr_size) = 
        ItvDom.Mem.lookup lv ptrmem
        |> ItvDom.Val.array_of_val 
        |> tuple
        |> Tuple2.map ArrayBlk.pow_loc_of_array ArrayBlk.allocsites_of_array
        |> Tuple2.map PowOctLoc.of_locs PowOctLoc.of_sizes 
      in
      mem
      |> forget mode global packconf pid oct_lv
      |> forget mode global packconf pid arr_val
      |> forget mode global packconf pid arr_size
  | IntraCfg.Cmd.Calloc (l, e, is_static, _) ->  
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in
      let ptrs = ItvSem.eval_alloc node position e is_static ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      alloc mode global ptrmem packconf pid lv ptrs e mem
  | IntraCfg.Cmd.Csalloc (l,s,_) -> 
      let lv = ItvSem.eval_lv pid l ptrmem |> PowOctLoc.of_locs in 
      let ptrs = ItvSem.eval_salloc node position s ptrmem |> ItvDom.Val.allocsites_of_val |> PowOctLoc.of_sizes in
      let e = Cil.integer (String.length s + 1) in
      alloc mode global ptrmem packconf pid lv ptrs e mem
  | IntraCfg.Cmd.Cseq l -> 
      List.fold_left (fun (mem,pos) c -> 
        (run_cmd ~position:pos mode packconf node c ptrmem (mem,global), pos+1)) (mem,0) l |> fst
  | IntraCfg.Cmd.Cassume (e, _) -> prune mode global ptrmem packconf pid e mem
  | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f, Cil.NoOffset), arg_exps, loc)
    when Global.is_undef f.vname global -> (* undefined library functions *)
      handle_undefined_functions mode packconf node pid position (lvo,f,arg_exps) ptrmem (mem,global) loc
  | IntraCfg.Cmd.Ccall (lvo, f, arg_exps, _) ->
      let fs = ItvDom.Val.pow_proc_of_val (ItvSem.eval pid f ptrmem) in
      if PowProc.eq fs PowProc.bot then mem
      else
        let arg_lvars_of_proc f acc =
          let args = InterCfg.argsof global.icfg f in
          let lvars = List.map (fun x -> Loc.of_lvar (f,x)) args in
          BatSet.add lvars acc in
        let arg_lvars_set = PowProc.fold arg_lvars_of_proc fs BatSet.empty in
        binding mode global ptrmem packconf pid arg_lvars_set arg_exps mem
  | IntraCfg.Cmd.Creturn (Some e, _) -> 
      let ret_locs = Dump.find (InterCfg.Node.get_pid node) global.dump |> PowOctLoc.of_locs in
      set mode global ptrmem packconf pid ret_locs e mem
  | _ -> mem

let run ?(mode = Weak) ?(locset = BatSet.empty) ?(ptrinfo = ItvDom.Table.bot) : Node.t -> Dom.t * Global.t -> Dom.t * Global.t
= fun node (mem, global) ->
  let ptrmem = ItvDom.Table.find node ptrinfo in
  (if !Options.opt_oct_debug then 
  begin
    prerr_endline "CMD";
    prerr_endline (Node.to_string node);
    prerr_endline (IntraCfg.Cmd.to_string (InterCfg.cmdof global.icfg node));
    prerr_endline "== input ==";
    prerr_endline (Mem.to_string mem)
  end);
  let mem = run_cmd mode locset node (InterCfg.cmdof global.icfg node) ptrmem (mem,global) in
  (if !Options.opt_oct_debug then 
   begin
    prerr_endline "== output ==";
    prerr_endline (Mem.to_string mem)
   end);
  (mem, global)

let accessof ?(mode = Weak) ?(locset = BatSet.empty) ?(ptrinfo = ItvDom.Table.bot) : Global.t -> Node.t -> Dom.t -> Access.t
= fun global node mem ->
  let (itv_use, itv_def) = 
    ItvDom.Table.find node ptrinfo 
    |> ItvSem.accessof ~mode:mode global node
    |> tuple 
    |> Tuple2.map ItvSem.Access.useof ItvSem.Access.defof
  in
  init_access ();
  ignore(run ~mode:mode ~locset:locset ~ptrinfo:ptrinfo node (mem,global));
  return_access ()
  |> BatSet.fold (fun l access ->
        let loc_pack = OctLoc.of_loc l |> PackConf.get_pack locset in
        match l with 
          BasicDom.Loc.Allocsite a -> 
            let size_pack = OctLoc.of_size a |> PackConf.get_pack locset in
            access |> Access.add Access.use loc_pack |> Access.add Access.use size_pack
        | _ -> access |> Access.add Access.use loc_pack) itv_use
  |> BatSet.fold (fun l access ->
        let loc_pack = OctLoc.of_loc l |> PackConf.get_pack locset in
        match l with 
          BasicDom.Loc.Allocsite a -> 
            let size_pack = OctLoc.of_size a |> PackConf.get_pack locset in
            access |> Access.add Access.all loc_pack |> Access.add Access.all size_pack 
        | _ -> access |> Access.add Access.all loc_pack) itv_def

let do_preanalysis ?(locset=BatSet.empty) ?(ptrinfo=ItvDom.Table.empty) : Global.t -> Mem.t * Global.t 
= fun global -> (Dom.top locset, global)

(* ********************** *
 * Buffer overrun checker *
 * ********************** *)
let check_bo: Proc.t -> OctDom.PackConf.t -> Allocsite.t -> Itv.t -> Cil.exp 
  -> ItvDom.Mem.t -> Dom.t -> Itv.t
= fun pid packconf a offset idx ptrmem mem ->
  let size = OctLoc.of_size a in
  let pack = PackConf.get_pack packconf size in
  let oct = Mem.find pack mem in
  (if !Options.opt_oct_debug then 
  begin
    prerr_endline (Pack.to_string pack);
    prerr_endline (Octagon.to_string oct)
  end);
  exp_to_texpr pid packconf pack idx ptrmem mem
  |> List.fold_left (fun diff idx ->
      let size_texpr = Texpr1.Var (OctLoc.to_var size) in
      let diff_texpr= Texpr1.Binop (Texpr1.Sub, size_texpr, idx, Texpr1.Int, Texpr1.Near) in
      Itv.join diff (Octagon.itv_of_expr diff_texpr oct)) Itv.bot
  |> (fun d -> Itv.minus d offset)
