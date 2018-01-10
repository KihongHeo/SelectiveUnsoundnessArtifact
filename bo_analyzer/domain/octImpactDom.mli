(** Abstract domains of octagon impact analysis *)

module AbsOct : sig
  include AbsDom.LAT
  val top : t
  val is_defined :  OctDom.OctLoc.t -> t -> bool
  val set_const : OctDom.OctLoc.t -> t -> t
  val weak_set_const : OctDom.OctLoc.t -> t -> t
  val set_variable : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> t
  val weak_set_variable : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> t
  val forget : OctDom.OctLoc.t -> t -> t
  val assume : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> t
  val weak_assume : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> t
  val check : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> bool
end

module Mem : sig
  include MapDom.S
  val init : OctDom.Pack.t -> t
  val lookup : t -> AbsOct.t

end with type A.t = OctDom.Pack.t and type B.t = AbsOct.t

module Table : MapDom.S with type A.t = BasicDom.Node.t and type B.t = Mem.t

module Relation : sig
  type t 
  val empty : t
  val add_edge : OctDom.OctLoc.t -> OctDom.OctLoc.t -> t -> t
  val add_absoct : AbsOct.t -> t -> t
  val get_packconf : t -> OctDom.PackConf.t
end

val pack : OctDom.Pack.t
