open AUGER_Monad
open BinInt
open DItv
open DLat
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomMem
open InterNode
open String0
open Syn
open UserInputType

val eval_const : constant -> Val.t

val eval_uop : unop -> Val.t -> Val.t

val is_array_loc : Loc.t -> bool

val array_loc_of_val : Val.t -> Val.t

val eval_bop : binop -> Val.t -> Val.t -> Val.t

val eval_string : char list -> Val.t

val eval_string_loc : char list -> Allocsite.t -> PowLoc.t -> Val.t

val deref_of_val : Val.t -> PowLoc.t

val resolve_offset :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  Val.t -> offset -> Mem.t -> 'a1

val eval :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  pid_t -> exp -> Mem.t -> 'a1

val eval_lv :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  pid_t -> lval -> Mem.t -> 'a1

val eval_list :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  pid_t -> exp list -> Mem.t -> 'a1

val eval_alloc' : t -> Val.t

val eval_alloc : t -> alloc -> Val.t

