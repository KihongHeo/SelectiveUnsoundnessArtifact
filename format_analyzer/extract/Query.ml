open AUGER_Monad
open DLat
open DomAbs
open DomBasic
open DomMem
open SemAbs
open SemEval
open Syn
open UserInputType

type query = exp

type status' =
| Tainted of PowExtProcPos.t
| Clean

(** val status'_rect : (PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1 **)

let status'_rect f f0 = function
| Tainted x -> f x
| Clean -> f0

(** val status'_rec : (PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1 **)

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
           if is_printf1 f
           then (match arg with
                 | [] -> []
                 | fmt :: l -> fmt_query fmt)
           else if is_printf2 f
                then (match arg with
                      | [] -> []
                      | e :: l ->
                        (match l with
                         | [] -> []
                         | fmt :: l0 -> fmt_query fmt))
                else if is_printf3 f
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
    ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
    pid_t -> Mem.t -> query -> 'a1 **)

let check_query mB mBInst p mem q =
  let deref_e = Lval ((Coq_lval_intro ((MemLhost q), NoOffset,
    DPos.DPos.unknown_pos)), DPos.DPos.unknown_pos)
  in
  bind mBInst (eval mB mBInst p deref_e mem) (fun v ->
    ret mBInst
      ((if PowExtProcPos.coq_ILat.eq_dec (pow_proc_pos_of_val v)
             PowExtProcPos.coq_ILat.bot
        then Clean
        else Tainted (pow_proc_pos_of_val v)) :: []))

