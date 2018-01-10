open DLat
open DPow
open Datatypes
open Sumbool
open VocabA

module Access : 
 functor (PowLoc:POW) ->
 sig 
  type t = PowLoc.t * PowLoc.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : ('a1 * 'a2) -> 'a1
  
  val snd : ('a1 * 'a2) -> 'a2
  
  type access_mode =
  | DEF
  | USE
  | ALL
  
  val access_mode_rect : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1
  
  val access_mode_rec : 'a1 -> 'a1 -> 'a1 -> access_mode -> 'a1
  
  val defof : t -> PowLoc.t
  
  val useof : t -> PowLoc.t
  
  val accessof : t -> PowLoc.t
  
  val empty : t
  
  val add : access_mode -> PowLoc.elt -> t -> t
  
  val add_set : access_mode -> PowLoc.t -> t -> t
  
  val singleton : access_mode -> PowLoc.elt -> t
  
  val from_set : access_mode -> PowLoc.t -> t
  
  val mem : PowLoc.elt -> t -> bool
  
  val remove : PowLoc.elt -> t -> t
  
  val remove_set : PowLoc.t -> t -> t
  
  val add_list : access_mode -> PowLoc.elt list -> t -> t
  
  val union : t -> t -> t
  
  val restrict : PowLoc.t -> t -> t
  
  val filter_out : PowLoc.t -> t -> t
 end

