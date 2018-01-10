(** Accessibility information *)
module type ACCESS = 
sig
  type t
  module Loc : AbsDom.SET
  module PowLoc : AbsDom.SET
  type mode
  val empty : t
  val def : mode
  val use : mode
  val all : mode
  val add : mode -> Loc.t -> t -> t
  val singleton : mode -> Loc.t -> t
  val mem : Loc.t -> t -> bool
  val remove : Loc.t -> t -> t
  val remove_set : Loc.t BatSet.t -> t -> t
  val add_set : mode -> Loc.t BatSet.t -> t -> t
  val from_set : mode -> Loc.t BatSet.t -> t
  val add_list : mode -> Loc.t list -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> Loc.t BatSet.t
  val restrict : Loc.t BatSet.t -> t -> t
  val filter_out : Loc.t BatSet.t -> t -> t
  val accessof : t -> Loc.t BatSet.t
  val useof : t -> Loc.t BatSet.t
  val defof : t -> Loc.t BatSet.t
  val cardinal : t -> int
  val to_string_use : t -> string
  val to_string_def : t -> string
  val to_string : t -> string
  val print : t -> unit
  val print_use : t -> unit
  val print_def : t -> unit
end

module Make(Dom: MapDom.S) : ACCESS with type Loc.t = Dom.A.t
