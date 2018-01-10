open BasicDom
open Vocab 

module Val = 
struct
  include ProdDom.Make4 (Itv) (PowLoc) (ArrayBlk) (PowProc)
  let null = (Itv.bot, PowLoc.null, ArrayBlk.bot,PowProc.bot)
  let is_itv (i,p,a,f) = not (Itv.is_bot i)
  let is_array (i,p,a,f) = not (ArrayBlk.is_empty a)
  let make (i,p,a,proc) = (i,p,a,proc)
  let itv_of_val : t -> Itv.t = fst
  let pow_loc_of_val : t -> PowLoc.t = snd
  let array_of_val : t -> ArrayBlk.t = trd
  let pow_proc_of_val : t -> PowProc.t = frth
  let allocsites_of_val : t -> Allocsite.t BatSet.t
  = fun v -> v |> array_of_val |> ArrayBlk.allocsites_of_array 

  let of_itv : Itv.t -> t = fun x ->
    (x, PowLoc.bot, ArrayBlk.bot, PowProc.bot)
  let of_pow_loc : PowLoc.t -> t = fun x ->
    (Itv.bot, x, ArrayBlk.bot, PowProc.bot)
  let of_array : ArrayBlk.t -> t = fun x ->
    (Itv.bot, PowLoc.bot, x,PowProc.bot)
  let of_pow_proc : PowProc.t -> t = fun x ->
    (Itv.bot, PowLoc.bot, ArrayBlk.bot, x)

  let modify_itv : Itv.t -> t -> t = fun i x ->
    (i, pow_loc_of_val x, array_of_val x, pow_proc_of_val x)
  
  let modify_arr : ArrayBlk.t -> t -> t = fun a x ->
    (itv_of_val x, pow_loc_of_val x, a, pow_proc_of_val x)

  let external_value : Allocsite.t -> t = fun allocsite ->
      let array = ArrayBlk.extern allocsite in
      (Itv.top, PowLoc.bot, array, PowProc.bot)
  
  let input_value : t = (Itv.top, PowLoc.bot, ArrayBlk.bot, PowProc.bot)

  let cast : Cil.typ -> Cil.typ -> t -> t
  = fun from_typ to_typ v -> 
    let (from_typ, to_typ) = BatTuple.Tuple2.mapn Cil.unrollTypeDeep (from_typ, to_typ) in
    if v = (of_itv Itv.zero) && (Cil.isPointerType to_typ) then (* char* x = (char* ) 0 *)
      null
    else if Cil.isIntegralType to_typ then 
      v |> itv_of_val |> Itv.cast from_typ to_typ |> of_itv
    else
      v |> array_of_val |> ArrayBlk.cast_array to_typ |> flip modify_arr v

  let to_string x = 
   "("^(Itv.to_string (fst x))^", "^(PowLoc.to_string (snd x))^", "
    ^(ArrayBlk.to_string (trd x))
    ^", "^(PowProc.to_string (frth x))^")"

end

module Mem = 
struct 
  include MapDom.Make (Loc) (Val)

  let lookup : PowLoc.t -> t -> Val.t = fun locs mem ->
    if eq mem bot then Val.bot 
    else
      let find_join loc acc = Val.join acc (find loc mem) in
      PowLoc.fold find_join locs Val.bot

  let strong_update : PowLoc.t -> Val.t -> t -> t
  = fun locs v mem ->
    PowLoc.fold (fun x -> add x v) locs mem

  let weak_update : PowLoc.t -> Val.t -> t -> t
  = fun locs v mem -> 
    PowLoc.fold (fun x -> weak_add x v) locs mem 
end

module Table = MapDom.Make (Node) (Mem)
