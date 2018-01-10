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

val status'_rect : (PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1

val status'_rec : (PowExtProcPos.t -> 'a1) -> 'a1 -> status' -> 'a1

type status = status'

val fmt_query : exp -> (query * DPos.DPos.t) list

val collect_query : cmd -> (query * DPos.DPos.t) list

val check_query :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  pid_t -> Mem.t -> query -> 'a1

