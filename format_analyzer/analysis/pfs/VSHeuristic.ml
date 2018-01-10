open Graph
open VocabB
open Datatypes
open Syn
open UserInputType
open UserInput.Input

(* TODO: SemEval is user input dependent. *)

let accMem = Obj.magic coq_AccMem
let mAcc = Obj.magic coq_MAcc
let idMem = Obj.magic coq_IdMem
let mId = Obj.magic coq_MId

module C = Cil
module F = Frontc
module E = Errormsg

type locset = PowLoc.t

type feature = {
  gvars : locset; (* global variable: done. *)
  lvars : locset; (* local variable: done. *)
  lvars_in_G : locset; (* local variables of _G_ : done *)
  fields : locset; (* structure fields : done *)
  allocsites : locset; (* allocsites : done *)
  ext_allocsites : locset; (* external allocsites : done *)
  single_defs : locset; (* defined at single-site: done.*)
  assign_const : locset; (* e.g. x = (c1 + c2): done. *)
  assign_sizeof : locset; (* e.g., x = sizeof(...): done *)
  prune_simple : locset; (* make_prune_simple worked: done *)
  prune_by_const : locset; (* e.g., x < c: done *)
  prune_by_var : locset; (* e.g., x < y: done *)
  prune_by_not : locset; (* e.g., !x: done *)
  pass_to_alloc : locset; (* e.g., malloc(x): done *)
  pass_to_alloc2 : locset; (* e.g., y = x; malloc(y): done *)
  pass_to_alloc_clos : locset; (* e.g., y = x; malloc(y): done *)
  pass_to_realloc : locset; (* e.g., realloc(x): done *)
  pass_to_realloc2 : locset; (* e.g., y = x; realloc(y): done *)
  pass_to_realloc_clos : locset; (* e.g., y = x; realloc(y): done *)
  pass_to_buf : locset; (* e.g., buf = x; done *)
  return_from_alloc : locset; (* x := malloc(...): done *)
  return_from_alloc2 : locset; (* y := malloc(...); x = y: done *)
  return_from_alloc_clos : locset; (* y := malloc(...); x = y: done *)
  return_from_realloc : locset; (* x := malloc(...): done *)
  return_from_realloc2 : locset; (* y := malloc(...); x = y: done *)
  return_from_realloc_clos : locset; (* y := malloc(...); x = y: done *)
  inc_itself_by_one : locset; (* e.g., x = x + 1: done *)
  incptr_itself_by_one : locset; (* e.g., x = x + 1 (x is a pointer): done *)
  inc_itself_by_const : locset; (* e.g., x = x + c (where c > 1): done *)
  incptr_itself_by_const : locset; (* e.g., x = x + c (x is a pointer) (where c > 1): done *)
  inc : locset; (* e.g., x = y + 1 : done *)
  dec : locset; (* e.g., x = y - 1 : done *)
  dec_itself : locset; (* e.g., x = x - y : done *)
  mul_itself_by_const : locset; (* e.g., x = x * 2 : done *)
  used_as_array_index : locset;  (* e.g., arr[x]: done *)
  used_as_array_buf : locset; (* e.g., x[i] : done *)
  mod_in_rec_fun : locset; (* modified inside recursive functions : done *)
  return_from_ext_fun : locset; (* e.g., x = ext_function() : done *)
  mod_inside_loops : locset; (* while (1) { ... x:= ... } : done *)
  used_inside_loops : locset (* while (1) { ... :=x ... } : done *)
}

let empty_feature = {
  gvars = PowLoc.empty;
  lvars = PowLoc.empty;
  fields = PowLoc.empty;
  allocsites = PowLoc.empty;
  ext_allocsites = PowLoc.empty;
  single_defs = PowLoc.empty;
  assign_const = PowLoc.empty;
  assign_sizeof = PowLoc.empty;
  prune_simple = PowLoc.empty;
  prune_by_const = PowLoc.empty;
  prune_by_var = PowLoc.empty;
  prune_by_not = PowLoc.empty;
  pass_to_alloc = PowLoc.empty;
  pass_to_alloc2 = PowLoc.empty;
  pass_to_alloc_clos = PowLoc.empty;
  pass_to_realloc = PowLoc.empty;
  pass_to_realloc2 = PowLoc.empty;
  pass_to_realloc_clos = PowLoc.empty;
  pass_to_buf = PowLoc.empty;
  return_from_alloc = PowLoc.empty;
  return_from_alloc2 = PowLoc.empty;
  return_from_alloc_clos = PowLoc.empty;
  return_from_realloc = PowLoc.empty;
  return_from_realloc2 = PowLoc.empty;
  return_from_realloc_clos = PowLoc.empty;
  inc_itself_by_one = PowLoc.empty;
  incptr_itself_by_one = PowLoc.empty;
  inc_itself_by_const = PowLoc.empty;
  incptr_itself_by_const = PowLoc.empty;
  mul_itself_by_const = PowLoc.empty;
  used_as_array_index = PowLoc.empty;
  mod_in_rec_fun = PowLoc.empty;
  lvars_in_G = PowLoc.empty;
  dec_itself = PowLoc.empty;
  dec = PowLoc.empty;
  inc = PowLoc.empty;
  used_as_array_buf = PowLoc.empty;
  return_from_ext_fun = PowLoc.empty;
  mod_inside_loops = PowLoc.empty;
  used_inside_loops = PowLoc.empty
}

(*
let prerr_feature feature =
  let l2s locs = string_of_set Loc.to_string locs in
  prerr_endline "== features for variable ranking ==";
  prerr_endline ("gvars : " ^ l2s feature.gvars);
  prerr_endline ("lvars : " ^ l2s feature.lvars);
  prerr_endline ("fields : " ^ l2s feature.fields);
  prerr_endline ("allocsites : " ^ l2s feature.allocsites);
  prerr_endline ("ext_allocsites : " ^ l2s feature.ext_allocsites);
  prerr_endline ("single_def : " ^ l2s feature.single_defs);
  prerr_endline ("assigned_const : " ^ l2s feature.assign_const);
  prerr_endline ("assigned_sizeof : " ^ l2s feature.assign_sizeof);
  prerr_endline ("prune_simple : " ^ l2s feature.prune_simple);
  prerr_endline ("prune_const : " ^ l2s feature.prune_by_const);
  prerr_endline ("prune_var : " ^ l2s feature.prune_by_var);
  prerr_endline ("prune_not : " ^ l2s feature.prune_by_not);
  prerr_endline ("pass_to_alloc : " ^ l2s feature.pass_to_alloc);
  prerr_endline ("pass_to_alloc2 : " ^ l2s feature.pass_to_alloc2);
  prerr_endline ("pass_to_alloc_clos : " ^ l2s feature.pass_to_alloc_clos);
  prerr_endline ("pass_to_realloc : " ^ l2s feature.pass_to_realloc);
  prerr_endline ("pass_to_realloc2 : " ^ l2s feature.pass_to_realloc2);
  prerr_endline ("pass_to_realloc_clos : " ^ l2s feature.pass_to_realloc_clos);
  prerr_endline ("pass_to_buf : " ^ l2s feature.pass_to_buf);
  prerr_endline ("return_from_alloc : " ^ l2s feature.return_from_alloc);
  prerr_endline ("return_from_alloc2 : " ^ l2s feature.return_from_alloc2);
  prerr_endline ("return_from_alloc_clos : " ^ l2s feature.return_from_alloc_clos);
  prerr_endline ("return_from_realloc : " ^ l2s feature.return_from_realloc);
  prerr_endline ("return_from_realloc2 : " ^ l2s feature.return_from_realloc2);
  prerr_endline ("return_from_realloc_clos : " ^ l2s feature.return_from_realloc_clos);
  prerr_endline ("inc_itself_by_one : " ^ l2s feature.inc_itself_by_one);
  prerr_endline ("incptr_itself_by_one : " ^ l2s feature.incptr_itself_by_one);
  prerr_endline ("inc_itself_by_const : " ^ l2s feature.inc_itself_by_const);
  prerr_endline ("incptr_itself_by_const : " ^ l2s feature.incptr_itself_by_const);
  prerr_endline ("mul_itself_by_const : " ^ l2s feature.mul_itself_by_const);
  prerr_endline ("dec_itself : " ^ l2s feature.dec_itself);
  prerr_endline ("inc : " ^ l2s feature.inc);
  prerr_endline ("dec : " ^ l2s feature.dec);
  prerr_endline ("used_as_array_index : " ^ l2s feature.used_as_array_index);
  prerr_endline ("used_as_array_buf : " ^ l2s feature.used_as_array_buf);
  prerr_endline ("mod_in_rec_fun : " ^ l2s feature.mod_in_rec_fun);
  prerr_endline ("lvars_in_G : " ^ l2s feature.lvars_in_G);
  prerr_endline ("returned_from_ext_fun : " ^ l2s feature.return_from_ext_fun);
  prerr_endline ("mod_inside_loops : " ^ l2s feature.mod_inside_loops);
  prerr_endline ("used_inside_loops : " ^ l2s feature.used_inside_loops)
*)

(* simplify expressions:
   1. remove casts
   2. remove coefficients
   e.g.,  (int)(sizeof(int) * (int)x) -> x )
*)
let rec simplify_exp e = e |> remove_casts |> remove_coeffs

and remove_casts e =
  match e with
  | CastE (typ, e1, _) -> remove_casts e1
  | BinOp (bop, e1, e2, pos) ->
    BinOp (bop, remove_casts e1, remove_casts e2, pos)
  | UnOp (uop, e1, pos) -> UnOp (uop, remove_casts e1, pos)
  | _ -> e

and remove_coeffs e =
  match e with
  | BinOp (_, e1, e2, _) when is_const e1 -> remove_coeffs e2
  | BinOp (_, e1, e2, _) when is_const e2 -> remove_coeffs e1
  | UnOp (_, e1, _) -> remove_coeffs e1
  | _ -> e

and is_const e =
  match e with
  | Const _ -> true
  | SizeOf _ -> true
  | SizeOfE _ -> true
  | SizeOfStr _ -> true
  | UnOp (_, e, _) -> is_const e
  | BinOp (_, e1, e2, _) -> is_const e1 && is_const e2
  | _ -> false

and is_sizeof e =
  match e with
  | SizeOf _
  | SizeOfE _
  | SizeOfStr _ -> true
  | _ -> false

and is_var e =
  match e with
  | Lval (Coq_lval_intro (VarLhost _, NoOffset, _), _) -> true
  | _ -> false

let inc_itself_by_one (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (PlusA,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           Const (CInt64 (Some i), _),
           _)
    when x = y && i = 1 -> true
  | _ -> false

let incptr_itself_by_one (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (PlusPI,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           Const (CInt64 (Some i), _),
           _)
    when x = y && i = 1 -> true
  | _ -> false

let inc_itself_by_const (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (PlusA,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           Const (CInt64 (Some i), _),
           _)
    when x = y && i > 1 -> true
  | _ -> false

let incptr_itself_by_const (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (PlusPI,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           Const (CInt64 (Some i), _),
           _)
    when x = y && i > 1 -> true
  | _ -> false

let mul_itself_by_const (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (Mult,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           Const (CInt64 (Some i), _),
           _)
    when x = y && i > 1 -> true
  | _ -> false

let dec_itself (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _),
    BinOp (MinusA,
           Lval (Coq_lval_intro (VarLhost (y, _), NoOffset, _), _),
           _, _)
    when x = y -> true
  | _ -> false

let is_inc (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _), BinOp (PlusA, _, _, _) -> true
  | _ -> false

let is_dec (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _), BinOp (MinusA, _, _, _) -> true
  | _ -> false

let is_mul (lv, e) =
  match lv, e with
  | Coq_lval_intro (VarLhost (x, _), NoOffset, _), BinOp (Mult, _, _, _) -> true
  | _ -> false

let is_proc_G loc =
  match loc with
  | (Coq_inl (Coq_inr (p, _)), _) -> p = "_G_"
  | _ -> false

let add_assign_const loc feat =
  { feat with assign_const = PowLoc.add loc feat.assign_const }

let add_assign_sizeof loc feat =
  { feat with assign_sizeof = PowLoc.add loc feat.assign_sizeof }

let add_prune_by_const loc feat =
  { feat with prune_by_const = PowLoc.add loc feat.prune_by_const }

let add_prune_by_var loc feat =
  { feat with prune_by_var = PowLoc.add loc feat.prune_by_var }

let add_prune_by_not loc feat =
  { feat with prune_by_not = PowLoc.add loc feat.prune_by_not }

let add_prune_simple loc feat =
  { feat with prune_simple = PowLoc.add loc feat.prune_simple }

let add_pass_to_alloc loc feat =
  { feat with pass_to_alloc = PowLoc.add loc feat.pass_to_alloc }

let add_return_from_alloc loc feat =
  { feat with return_from_alloc = PowLoc.add loc feat.return_from_alloc }

let add_pass_to_alloc2 loc feat =
  { feat with pass_to_alloc2 = PowLoc.add loc feat.pass_to_alloc2 }

let add_return_from_alloc2 loc feat =
  { feat with return_from_alloc2 = PowLoc.add loc feat.return_from_alloc2 }

let add_pass_to_realloc loc feat =
  { feat with pass_to_realloc = PowLoc.add loc feat.pass_to_realloc }

let add_return_from_realloc loc feat =
  { feat with return_from_realloc = PowLoc.add loc feat.return_from_realloc }

let add_pass_to_realloc2 loc feat =
  { feat with pass_to_realloc2 = PowLoc.add loc feat.pass_to_realloc2 }

let add_return_from_realloc2 loc feat =
  { feat with return_from_realloc2 = PowLoc.add loc feat.return_from_realloc2 }

let add_return_from_ext_fun loc feat =
  { feat with return_from_ext_fun = PowLoc.add loc feat.return_from_ext_fun }

let add_inc_itself_by_one loc feat =
  { feat with inc_itself_by_one = PowLoc.add loc feat.inc_itself_by_one }

let add_incptr_itself_by_one loc feat =
  { feat with incptr_itself_by_one = PowLoc.add loc feat.incptr_itself_by_one }

let add_inc_itself_by_const loc feat =
  { feat with inc_itself_by_const = PowLoc.add loc feat.inc_itself_by_const }

let add_incptr_itself_by_const loc feat =
  { feat
    with incptr_itself_by_const = PowLoc.add loc feat.incptr_itself_by_const }

let add_mul_itself_by_const loc feat =
  { feat with mul_itself_by_const = PowLoc.add loc feat.mul_itself_by_const }

let add_inc loc feat =
  { feat with inc = PowLoc.add loc feat.inc }

let add_dec_itself loc feat =
  { feat with dec_itself = PowLoc.add loc feat.dec_itself }

let add_dec loc feat =
  { feat with dec = PowLoc.add loc feat.dec }

let add_used_as_array_index loc feat =
  { feat with used_as_array_index = PowLoc.add loc feat.used_as_array_index }

let add_used_as_array_buf loc feat =
  { feat with used_as_array_buf = PowLoc.add loc feat.used_as_array_buf }

let add_mod_in_rec_fun loc feat =
  { feat with mod_in_rec_fun = PowLoc.add loc feat.mod_in_rec_fun }

let add_mod_inside_loops loc feat =
  { feat with mod_inside_loops = PowLoc.add loc feat.mod_inside_loops }

let add_used_inside_loops loc feat =
  { feat with used_inside_loops = PowLoc.add loc feat.used_inside_loops }

let check_op op =
  op = Syn.Lt || op = Syn.Gt || op = Syn.Le || op = Syn.Ge || op = Syn.Eq
  || op = Syn.Ne

let extract_set f (lv, e) m g feature =
  let locs = SemEval.eval_lv idMem mId f lv m in
  try
    feature
    |> (if is_const e then PowLoc.fold add_assign_const locs else id)
    |> (if is_sizeof e then PowLoc.fold add_assign_sizeof locs else id)
    |> (if inc_itself_by_one (lv, e) then
          PowLoc.fold add_inc_itself_by_one locs
        else id)
    |> (if incptr_itself_by_one (lv, e) then
          PowLoc.fold add_incptr_itself_by_one locs
        else id)
    |> (if incptr_itself_by_const (lv, e) then
          PowLoc.fold add_incptr_itself_by_const locs
        else id)
    |> (if inc_itself_by_const (lv, e) then
          PowLoc.fold add_inc_itself_by_const locs
        else id)
    |> (if mul_itself_by_const (lv, e) then
          PowLoc.fold add_mul_itself_by_const locs
        else id)
    |> (if dec_itself (lv, e) then PowLoc.fold add_dec_itself locs else id)
    |> (if is_inc (lv, e) then PowLoc.fold add_inc locs else id)
    |> (if is_dec (lv, e) then PowLoc.fold add_dec locs else id)
    |> (if G.is_rec f g then PowLoc.fold add_mod_in_rec_fun locs else
          id)
  with e -> prerr_endline "extract_set"; raise e

let extract_assume f e m g feature =
  feature
(* TODO: domain dependent one *)
(* |>
  (match SemPrune.make_cond_simple e with
   | None -> id
   | Some cond ->
     let v_apair = SemPrune.prune_simple
         Weak PrePhase f PowLoc.empty g accMem mAcc cond m in
     PowLoc.fold add_prune_simple (Acc.defof (get_acc v_apair))
     >>>
  (match cond with
         | BinOp (op, Lval (x, _), e, _) when check_op op ->
           let locs =
             SemEval.eval_lv idMem mId f x m in
           (if is_const e then PowLoc.fold add_prune_by_const locs else id)
           >>> (if is_var e then PowLoc.fold add_prune_by_var locs else id)
         | UnOp (LNot, Lval (x, _), _) ->
           let locs =
             SemEval.eval_lv idMem mId f x m in
           PowLoc.fold add_prune_by_not locs
         | _ -> id))
*)


let extract_alloc f (lv, e) m feature =
  let locs_lv =
    SemEval.eval_lv idMem mId f lv m in
  let access = get_acc (SemEval.eval accMem mAcc f e m) in
  let locs_e = Acc.useof access in
  feature
  |> PowLoc.fold add_pass_to_alloc locs_e
  |> PowLoc.fold add_return_from_alloc locs_lv

let extract_call_realloc f (lvo, fe, el) m feature =
  match lvo, simplify_exp fe with
  | Some lv, Lval (Coq_lval_intro (VarLhost (f, _), NoOffset, _), _)
    when f = "realloc" ->
    let locs_lv = SemEval.eval_lv idMem mId f lv m in
    let access = get_acc (SemEval.eval_list accMem mAcc f el m) in
    let locs_e = Acc.useof access in
    feature
    |> PowLoc.fold add_pass_to_realloc locs_e
    |> PowLoc.fold add_return_from_realloc locs_lv
  | _ -> feature

let is_undef f g = G.is_undef f g && not (f = "realloc" || f = "strlen")

let extract_call_ext_fun f (lvo, fe, el) m g feature =
  match lvo,fe with
  | Some lv, Lval (Coq_lval_intro (VarLhost (f, _), NoOffset, _), pos)
    when is_undef f g ->
    let access = get_acc (SemEval.eval accMem mAcc f (Lval (lv, pos)) m) in
    PowLoc.fold add_return_from_ext_fun (Acc.useof access) feature
  | _ -> feature

let extract_call f (lvo, fe, el) m g feature =
  feature
  |> extract_call_ext_fun f (lvo, fe, el) m g
  |> extract_call_realloc f (lvo, fe, el) m

let extract_used_index f m cmd feature =
  feature
(*
  let queries = AlarmExp.collect cmd in
    list_fold
      (fun q ->
         let e =
           match q with
           | Cmd.ArrayExp (_, e) -> e
           | Cmd.DerefExp (BinOp (op, _, e2)) -> e2
           | _ -> Const (CStr "") in
      let access = get_acc (SemEval.eval f e m) in
      let locs = Acc.useof access in
      PowLoc.fold add_used_as_array_index locs
    ) queries feature
*)

let extract_used_buf pid mem cmd feature =
  feature
(*
  let queries = AlarmExp.collect cmd in
    list_fold (fun q ->
      let locs =
        Acc.useof
          (SemEval.eval pid
            (match q with
            | Cmd.ArrayExp (lv,_,_) -> Lval lv
            | Cmd.DerefExp (BinOp(op,e1,_,_),_) -> e1
            | Cmd.DerefExp (e,_) -> e
            (* | _ -> Const (CStr "") (\* dummy exp *\) *))
            mem Acc.empty)
      in PowLoc.fold add_used_as_array_buf locs
    ) queries feature
*)

let extract_loops f m cmd node icfg feature =
  feature
(*
  if not (BeInterCfg.is_inside_loop node icfg) then feature
  else
   match cmd with
   | Cset (lv, e) ->
     let defs = Acc.useof (get_acc (SemEval.eval f (Lval lv) m)) in
     let uses = Acc.useof (get_acc (SemEval.eval f e m)) in
     feature
     |> PowLoc.fold add_mod_inside_loops defs
     |> PowLoc.fold add_used_inside_loops uses
   | _ -> feature
*)

let extract1 icfg m g node feature =
  let f = InterNode.get_pid node in
  let cmd = get_some (InterCfg.get_cmd icfg node) in
  try
    feature
    |> (match cmd with
        | Cset (lv, e, _) -> extract_set f (lv, e) m g
        | Cassume (e, _) -> extract_assume f e m g
        | Calloc (lv, e, _) -> extract_alloc f (lv, e) m
        | Ccall (lvo, fe, el, _) -> extract_call f (lvo, fe, el) m g
        | _ -> id)
    |> (extract_used_index f m cmd)
    |> (extract_used_buf f m cmd)
    |> (extract_loops f m cmd node icfg)
  with e -> prerr_endline "extract1"; raise e

let traverse1 pre g =
  let icfg = G.icfg g in
  let m = Pre.get_mem pre in
  let nodes = InterCfg.nodes icfg in
  InterCfg.NodeSet.fold (extract1 icfg m g) nodes empty_feature

(* extract information obtainable after first iteration *)
(* : passed_to_alloc2, returned_from_alloc2 *)
let extract2 icfg m node feature =
  let f = InterNode.get_pid node in
  match InterCfg.get_cmd icfg node with
  | Some (Cset (lv, e, _)) ->
    let locs_lv = SemEval.eval_lv idMem mId f lv m in
    let locs_e  = Acc.useof (get_acc (SemEval.eval accMem mAcc f e m)) in
    let e = simplify_exp e in
    (match lv, e with
     | Coq_lval_intro (VarLhost _, NoOffset, _),
       Lval (Coq_lval_intro (VarLhost _, NoOffset, _), _) -> (* x := y *)
       (match PowLoc.choose locs_lv, PowLoc.choose locs_e with
       | Some l_x, Some l_y ->
         feature
         |> (if PowLoc.mem l_x feature.pass_to_alloc then
               add_pass_to_alloc2 l_y
             else id)
         |> (if PowLoc.mem l_y feature.return_from_alloc then
               add_return_from_alloc2 l_x
             else id)
         |> (if PowLoc.mem l_x feature.pass_to_realloc then
               add_pass_to_realloc2 l_y
             else id)
         |> (if PowLoc.mem l_y feature.return_from_realloc then
               add_return_from_realloc2 l_x
             else id)
       | _, _ -> feature)
     | _ -> feature)
  | _ -> feature

let traverse2 pre g feature =
  let icfg = G.icfg g in
  let m = Pre.get_mem pre in
  let nodes = InterCfg.nodes icfg in
  InterCfg.NodeSet.fold (extract2 icfg m) nodes feature

module N = struct
  type t = Loc.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end
module CopyG = Graph.Persistent.Digraph.ConcreteBidirectional(N)

let build_copy_graph icfg m =
  let iter_node node g =
    match InterCfg.get_cmd icfg node with
    | Some (Cset (lv, e, _)) ->
      (match lv, simplify_exp e with
       | Coq_lval_intro (VarLhost _, NoOffset, _),
         Lval (Coq_lval_intro (VarLhost _, NoOffset, _), _) ->
         let f = InterNode.get_pid node in
         let lhs_set = SemEval.eval_lv idMem mId f lv m in
         let rhs_set =
           Acc.useof (get_acc (SemEval.eval accMem mAcc f e m)) in
         (match PowLoc.choose lhs_set, PowLoc.choose rhs_set with
         | Some lhs, Some rhs -> CopyG.add_edge g rhs lhs
         | _, _ -> g)
       | _ -> g)
    | _ -> g in
  InterCfg.NodeSet.fold iter_node (InterCfg.nodes icfg) CopyG.empty

let list2locs l = list_fold PowLoc.add l PowLoc.empty

let closure pre g feature =
  let icfg = G.icfg g in
  let m = Pre.get_mem pre in
  let copy_graph = build_copy_graph icfg m in
  let pta = feature.pass_to_alloc in
  let rfa = feature.return_from_alloc in
  let ptra = feature.pass_to_realloc in
  let rfra = feature.return_from_realloc in
  let buf = feature.used_as_array_buf in
  let pred g n = try list2locs (CopyG.pred g n) with _ -> PowLoc.empty in
  let succ g n = try list2locs (CopyG.succ g n) with _ -> PowLoc.empty in
  let pred_set g s = PowLoc.fold (fun n -> PowLoc.union (pred g n)) s PowLoc.empty in
  let succ_set g s = PowLoc.fold (fun n -> PowLoc.union (succ g n)) s PowLoc.empty in
  let rec clos_backward set =
    let preds = pred_set copy_graph set in
    if PowLoc.subset preds set then set else
      clos_backward (PowLoc.union set preds) in
  let rec clos_forward set =
    let succs = succ_set copy_graph set in
    if PowLoc.subset succs set then set else
      clos_forward (PowLoc.union set succs) in
  { feature with
    pass_to_alloc_clos = clos_backward pta
  ; pass_to_realloc_clos = clos_backward ptra
  ; return_from_alloc_clos = clos_forward rfa
  ; return_from_realloc_clos = clos_forward rfra
  ; pass_to_buf = clos_backward buf }

let is_loc_field = function
  | (_, fl) when fl <> [] -> true
  | _ -> false

let is_loc_allocsite = function
  | (Coq_inr _, _) -> true
  | _ -> false

let is_loc_ext_alloc = function
  | (Coq_inr (Coq_inr _), _) -> true
  | _ -> false

let is_loc_lvar l =
  match classify_loc l with
  | LVarLoc _ -> true
  | _ -> false

let is_loc_gvar l =
  match classify_loc l with
  | GVarLoc -> true
  | _ -> false

let extract_feature locset pre g =
  let lvars = PowLoc.filter is_loc_lvar locset in
  let lvars_in_G = PowLoc.filter is_proc_G lvars in
  let gvars = PowLoc.filter is_loc_gvar locset in
  let fields = PowLoc.filter is_loc_field locset in
  let allocsites = PowLoc.filter is_loc_allocsite locset in
  let ext_allocsites = PowLoc.filter is_loc_ext_alloc allocsites in
  let single_defs = Pre.get_single_defs (Pre.get_defs_of pre) in
  let feature =
    try traverse1 pre g with
      e -> prerr_endline "traverse1" ; raise e in (* first iteration *)
  let feature = traverse2 pre g feature in (* second iteration *)
  let feature = closure pre g feature in
  { feature with
    gvars = gvars
  ; lvars = lvars
  ; fields = fields
  ; allocsites = allocsites
  ; ext_allocsites = ext_allocsites
  ; single_defs = single_defs
  ; lvars_in_G = lvars_in_G }

let weight_of l f =
  let scale = 10 in
  let  w1 () = scale * !Options.wv.Options.w1 in
  let  w2 () = scale * !Options.wv.Options.w2 in
  let  w3 () = scale * !Options.wv.Options.w3 in
  let  w4 () = scale * !Options.wv.Options.w4 in
  let  w5 () = scale * !Options.wv.Options.w5 in
  let  w6 () = scale * !Options.wv.Options.w6 in
  let  w7 () = scale * !Options.wv.Options.w7 in
  let  w8 () = scale * !Options.wv.Options.w8 in
  let  w9 () = scale * !Options.wv.Options.w9 in
  let  w10 () = scale * !Options.wv.Options.w10 in
  let  w11 () = scale * !Options.wv.Options.w11 in
  let  w12 () = scale * !Options.wv.Options.w12 in
  let  w13 () = scale * !Options.wv.Options.w13 in
  let  w14 () = scale * !Options.wv.Options.w14 in
  let  w15 () = scale * !Options.wv.Options.w15 in
  let  w16 () = scale * !Options.wv.Options.w16 in
  let  w17 () = scale * !Options.wv.Options.w17 in
  let  w18 () = scale * !Options.wv.Options.w18 in
  let  w19 () = scale * !Options.wv.Options.w19 in
  let  w20 () = scale * !Options.wv.Options.w20 in
  let  w21 () = scale * !Options.wv.Options.w21 in
  let  w22 () = scale * !Options.wv.Options.w22 in
  let  w23 () = scale * !Options.wv.Options.w23 in
  let  w24 () = scale * !Options.wv.Options.w24 in
  let  w25 () = scale * !Options.wv.Options.w25 in
  let  w26 () = scale * !Options.wv.Options.w26 in
  let  w27 () = scale * !Options.wv.Options.w27 in
  let  w28 () = scale * !Options.wv.Options.w28 in
  let  w29 () = scale * !Options.wv.Options.w29 in
  let  w30 () = scale * !Options.wv.Options.w30 in
  let  w31 () = scale * !Options.wv.Options.w31 in
  let  w32 () = scale * !Options.wv.Options.w32 in
  let  w33 () = scale * !Options.wv.Options.w33 in
  let  w34 () = scale * !Options.wv.Options.w34 in
  let  w35 () = scale * !Options.wv.Options.w35 in
  let  w36 () = scale * !Options.wv.Options.w36 in
  let  w37 () = scale * !Options.wv.Options.w37 in
  let  w38 () = scale * !Options.wv.Options.w38 in
  let  w39 () = scale * !Options.wv.Options.w39 in
  let  w40 () = scale * !Options.wv.Options.w40 in
  let  w41 () = scale * !Options.wv.Options.w41 in
  let  w42 () = scale * !Options.wv.Options.w42 in
  let  w43 () = scale * !Options.wv.Options.w43 in
  let  w44 () = scale * !Options.wv.Options.w44 in
  let  w45 () = scale * !Options.wv.Options.w45 in
  let  w46 () = scale * !Options.wv.Options.w46 in
  let  w47 () = scale * !Options.wv.Options.w47 in
  let  w48 () = scale * !Options.wv.Options.w48 in
  let  w49 () = scale * !Options.wv.Options.w49 in
  let  w50 () = scale * !Options.wv.Options.w50 in
  let  w51 () = scale * !Options.wv.Options.w51 in
  let  w52 () = scale * !Options.wv.Options.w52 in
  let  w53 () = scale * !Options.wv.Options.w53 in
  let  w54 () = scale * !Options.wv.Options.w54 in
  let  w55 () = scale * !Options.wv.Options.w55 in
  let  w56 () = scale * !Options.wv.Options.w56 in
  let  w57 () = scale * !Options.wv.Options.w57 in
  let  w58 () = scale * !Options.wv.Options.w58 in
  let  w59 () = scale * !Options.wv.Options.w59 in
  let  w60 () = scale * !Options.wv.Options.w60 in
  let mem = PowLoc.mem in
  let pass_to_alloc l f =
    mem l f.pass_to_alloc || mem l f.pass_to_alloc2
    || mem l f.pass_to_realloc || mem l f.pass_to_realloc2
    || mem l f.pass_to_alloc_clos || mem l f.pass_to_realloc_clos in
  let return_from_alloc l f =
    mem l f.return_from_alloc || mem l f.return_from_alloc2
    || mem l f.return_from_realloc || mem l f.return_from_realloc2
    || mem l f.return_from_alloc_clos || mem l f.return_from_realloc_clos in
  0
  (* combination rules *)
  (* temporary variables for loop initialization *)
  |> (if mem l f.assign_const && mem l f.prune_by_const
         && mem l f.mod_inside_loops
      then
        if mem l f.lvars then (+) (w1 ()) else
        if mem l f.lvars_in_G then (+) (w2 ()) else
          (+) (w3 ())
      else (+) (w4 ()))
  |> (if mem l f.prune_by_const && pass_to_alloc l f then
        if mem l f.gvars then (+) (w5 ()) else
        if mem l f.lvars then (+) (w6 ()) else
          (+) (w7 ())
      else (+) (w8 ()))
  |> (if mem l f.mul_itself_by_const && mem l f.assign_const
         && pass_to_alloc l f
      then
        if mem l f.gvars then (+) (w9 ()) else
        if mem l f.lvars then (+) (w10 ()) else
          (+) (w11 ())
      else (+) (w12 ()))
  |> (if (mem l f.inc_itself_by_const || mem l f.inc_itself_by_one)
         && pass_to_alloc l f
      then
        if mem l f.gvars then (+) (w13 ()) else
        if mem l f.lvars then (+) (w14 ()) else
          (+) (w15 ())
      else (+) (w16 ()))
  |> (if pass_to_alloc l f && return_from_alloc l f && mem l f.used_as_array_buf
      then (+) (w17 ()) else (+) (w18 ()))
  |> (if (return_from_alloc l f || pass_to_alloc l f)
         && mem l f.used_as_array_buf
      then (+) (w19 ()) else (+) (w20 ()))
  |> (if return_from_alloc l f then
        if mem l f.pass_to_buf then
          if mem l f.lvars then (+) (w21 ()) else
          if mem l f.gvars then (+) (w22 ()) else id
        else id
      else id)
  |> (if mem l f.lvars && pass_to_alloc l f then (+) (w23 ()) else (+) (w24 ()))
  |> (if mem l f.lvars (* local variable increasing in straightline code *)
         && mem l f.inc_itself_by_const && not (mem l f.mod_inside_loops)
      then (+) (w25 ()) else (+) (w26 ()))
  |> (if mem l f.lvars (* local pointer increasing in straightline code *)
         && mem l f.incptr_itself_by_one && not (mem l f.mod_inside_loops)
      then (+) (w27 ()) else (+) (w28 ()))
  |> (if mem l f.single_defs && mem l f.assign_const
      then (+) (w29 ()) else (+) (w30 ()))
  (* atomic rules *)
  |> (if mem l f.ext_allocsites then (+) (w31 ()) else (+) (w32 ()))
  |> (if mem l f.lvars then (+) (w33 ()) else (+) (w34 ()))
  |> (if mem l f.lvars_in_G then (+) (w35 ()) else (+) (w36 ()))
  |> (if mem l f.assign_const then (+) (w37 ()) else (+) (w38 ()))
  |> (if mem l f.prune_by_const then (+) (w39 ()) else (+) (w40 ()))
  |> (if pass_to_alloc l f then (+) (w41 ()) else (+) (w42 ()))
  |> (if mem l f.inc_itself_by_const || mem l f.incptr_itself_by_one
         || mem l f.incptr_itself_by_const
      then (+) (w43 ()) else (+) (w44 ()))
  |> (if mem l f.dec_itself then (+) (w45()) else (+) (w46()))
  |> (if mem l f.used_as_array_index then (+) (w47()) else (+) (w48()))
  |> (if mem l f.mod_in_rec_fun then (+) (w30()) else (+) (w49()))
  |> (if mem l f.return_from_ext_fun then (+) (w50 ()) else (+) (w51 ()))
  |> (if mem l f.used_inside_loops then (+) (w52 ()) else (+) (w53 ()))
  |> (if mem l f.assign_sizeof then (+) (w54 ()) else (+) (w55 ()))
  |> (if mem l f.fields then (+) (w56 ()) else (+) (w57 ()))
  |> (if mem l f.allocsites then (+) (w58 ()) else (+) (w59 ()))
  |> (if mem l f.prune_by_not then (+) (w60 ()) else id)

let assign_weight locs feature =
  List.map (fun l -> (l, weight_of l feature)) locs

let rank locset pre global =
  let feature =
    try extract_feature locset pre global with
      e -> prerr_endline "extract features"; raise e in
  let locs_weighted = assign_weight (powloc2list locset) feature in
  let sorted = List.sort (fun (_, w) (_, w') -> compare w' w) locs_weighted in
  BatList.map fst sorted
