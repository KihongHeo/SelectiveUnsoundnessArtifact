(** Array domain *)
module Struct : PowDom.S
module ArrInfo : 
sig
  type t = {
    offset    : Itv.t;
    size      : Itv.t;
    stride    : Itv.t;
    null_pos  : Itv.t;
    structure : Struct.t;
  }
  val top : t
  val input : t
end

include MapDom.S with type A.t = BasicDom.Allocsite.t and type B.t = ArrInfo.t

val make : BasicDom.Allocsite.t -> Itv.t -> Itv.t -> Itv.t -> Itv.t -> t

val offsetof : t -> Itv.t
val sizeof : t -> Itv.t
val nullof : t -> Itv.t
val extern : BasicDom.Allocsite.t -> t
val input : BasicDom.Allocsite.t -> t
val weak_plus_size : t -> Itv.t -> t
val plus_offset : t -> Itv.t -> t
val minus_offset : t -> Itv.t -> t
val set_null_pos : t -> Itv.t -> t
val plus_null_pos : t -> Itv.t -> t
val cast_array : Cil.typ -> t -> t 
val allocsites_of_array : t -> BasicDom.Allocsite.t BatSet.t 
val pow_loc_of_array : t -> BasicDom.PowLoc.t
val append_field : t -> Cil.fieldinfo -> BasicDom.PowLoc.t 
