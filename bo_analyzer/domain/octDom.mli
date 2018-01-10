(** Abstract domains of octagon analysis *)

module OctLoc : sig
  type t = Loc of BasicDom.Loc.t | Size of BasicDom.Allocsite.t
  val compare : t -> t -> int
  val dummy : t 
  val of_loc : BasicDom.Loc.t -> t
  val of_size : BasicDom.Allocsite.t -> t
  val to_string : t -> string
  val to_var : t -> Apron.Var.t
end

module PowOctLoc : sig
  include PowDom.S
  val empty: t
  val of_locs : BasicDom.PowLoc.t -> t
  val of_sizes : BasicDom.Allocsite.t BatSet.t -> t
end with type elt = OctLoc.t

module Pack = PowOctLoc

module PackConf : sig
  type t = Pack.t BatSet.t
  val empty : t
  val make : ItvDom.Table.t -> t -> t
  val get_pack : t -> OctLoc.t -> Pack.t
  val singleton : Pack.t -> t
  val add : Pack.t -> t -> t
  val fold : (Pack.t -> 'a -> 'a) -> t -> 'a -> 'a
  val print_info : t -> unit
end

module Octagon : sig
 include AbsDom.LAT
 val is_bot : t -> bool
 val itv_of_var : OctLoc.t -> t -> Itv.t
 val itv_of_expr : Apron.Texpr1.expr -> t -> Itv.t

 val set : OctLoc.t -> Apron.Texpr1.expr -> t -> t
 val forget : OctLoc.t -> t -> t
 val prune : OctLoc.t -> Apron.Texpr1.expr -> Apron.Tcons1.typ -> t -> t
end
module Mem : sig
  include MapDom.S
  val top: PackConf.t -> t
end with type A.t = Pack.t and type B.t = Octagon.t

module Table : MapDom.S with type A.t = BasicDom.Node.t and type B.t = Mem.t
