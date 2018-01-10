(** Abstract domains of interval analysis *)
module Val : sig 
  include AbsDom.LAT

  val null : t
  val make : (Itv.t * BasicDom.PowLoc.t * ArrayBlk.t * BasicDom.PowProc.t) -> t
  val itv_of_val : t -> Itv.t
  val pow_loc_of_val : t -> BasicDom.PowLoc.t
  val array_of_val : t -> ArrayBlk.t
  val pow_proc_of_val : t -> BasicDom.PowProc.t
  val allocsites_of_val : t -> BasicDom.Allocsite.t BatSet.t
  val of_itv : Itv.t -> t
  val of_pow_loc : BasicDom.PowLoc.t -> t
  val of_array : ArrayBlk.t -> t
  val of_pow_proc : BasicDom.PowProc.t -> t
  val modify_itv : Itv.t -> t -> t
  val modify_arr : ArrayBlk.t -> t -> t
  val external_value : BasicDom.Allocsite.t -> t
  val input_value : t
  val cast : Cil.typ -> Cil.typ -> t -> t
end

module Mem : sig
  include MapDom.S
  val lookup : BasicDom.PowLoc.t -> t -> Val.t
  val strong_update : BasicDom.PowLoc.t -> Val.t -> t -> t
  val weak_update : BasicDom.PowLoc.t -> Val.t -> t -> t
end with type A.t = BasicDom.Loc.t and type B.t = Val.t

module Table : MapDom.S with type A.t = BasicDom.Node.t and type B.t = Mem.t
