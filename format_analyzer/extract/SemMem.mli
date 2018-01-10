open AUGER_Monad
open Compare_dec
open DPow
open Datatypes
open DomAbs
open DomBasic
open DomMem
open List0
open Peano_dec
open UserInputType
open VocabA

val bind_lvar :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad -> G.t
  -> param -> LVar.t -> Val.t -> 'a1 -> 'a1

val bind_lvars :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad -> G.t
  -> param -> Val.t list -> LVar.t list -> 'a1 -> 'a1

val bind_lvars_list :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad -> G.t
  -> param -> LVar.t list list -> Val.t list -> Mem.t -> 'a1

