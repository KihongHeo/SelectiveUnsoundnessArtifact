open AUGER_Monad
open DLat
open DMap
open DPow
open Datatypes
open DomAbs
open DomArrayBlk
open DomBasic
open DomMem
open InterCfg
open InterNode
open List0
open SemEval
open SemMem
open StringFun
open Sumbool
open Syn
open TStr
open UserInputType
open VocabA

val str_realloc : string_t

val str_strlen : string_t

val str_scanf : string_t

val str_printf : string_t

val str_vprintf : string_t

val str_fprintf : string_t

val str_sprintf : string_t

val str_vfprintf : string_t

val str_vsprintf : string_t

val str_vasprintf : string_t

val str___asprintf : string_t

val str_asprintf : string_t

val str_vdprintf : string_t

val str_dprintf : string_t

val str_obstack_printf : string_t

val str_obstack_vprintf : string_t

val str_easprintf : string_t

val str_evasprintf : string_t

val str_snprintf : string_t

val str_vsnprintf : string_t

val str_argv : string_t

val ext_api : t M.t

val is_printf1 : DStr.DStr.t -> bool

val is_printf2 : DStr.DStr.t -> bool

val is_printf3 : DStr.DStr.t -> bool

val is_ppointer_printf : DStr.DStr.t -> bool

val is_no_assign_printf : DStr.DStr.t -> bool

val is_printf : exp -> pid_t option

val update_dump :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  InterNode.t -> PowProc.t -> lval option -> Mem.t -> G.t -> 'a1

val locals_of_mem : InterNode.t -> Mem.t -> PowLoc.t

val remove_local_variables :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> Mem.t -> 'a1

val external_value : Allocsite.t -> DPos.DPos.t -> Val.t

val is_undef : G.t -> exp -> pid_t option

val run_realloc :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> lval -> Val.t list -> Mem.t -> 'a1

val run_strlen :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> lval -> Mem.t -> 'a1

val run_scanf :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> G.t -> Val.t list -> Mem.t -> pid_t -> DPos.DPos.t -> 'a1

val run_printf1 : 'a1 coq_Monad -> Mem.t -> 'a1

val val_joins : Val.t list -> Val.t

val va_src_val_joins :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  Mem.t -> Val.t list -> 'a1

val va_arg_joins :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  InterNode.t -> Mem.t -> exp list -> 'a1

val run_printf_pointer :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> exp -> exp list -> Mem.t -> 'a1

val run_printf_ppointer :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> exp -> exp list -> Mem.t -> 'a1

val run_printf2 :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> pid_t -> exp list -> Mem.t -> 'a1

val run_printf3 :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> exp list -> Mem.t -> 'a1

val run_printf :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> pid_t -> exp list -> Mem.t -> 'a1

val set_ext_allocsite :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> lval -> Allocsite.t -> Mem.t -> DPos.DPos.t
  -> 'a1

val find_string_api_name : string_t -> t option

val process_dsts :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> DPos.DPos.t -> string_t ->
  ((Val.t * alloc_t) * src_t) list -> Val.t -> Mem.t -> 'a1

val process_ret :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> DPos.DPos.t -> string_t -> t -> lval option
  -> Val.t -> ((Val.t * alloc_t) * src_t) list -> Mem.t -> 'a1

val run_api :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> Mem.t -> lval option -> Val.t list ->
  DPos.DPos.t -> string_t -> t -> 'a1

val run_api_with_name :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> Mem.t -> lval option -> Val.t list ->
  DPos.DPos.t -> string_t -> 'a1

val run_undef_funcs :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> G.t -> lval option -> pid_t -> Val.t list -> Mem.t
  -> DPos.DPos.t -> 'a1

val run :
  ('a1, Loc.t, Mem.t, Val.t, Mem.PowA.t) coq_MemBasic -> 'a1 coq_Monad ->
  param -> InterNode.t -> cmd -> (Mem.t * G.t) -> 'a1

val run_access :
  param -> InterNode.t -> cmd -> (Mem.t * G.t) -> (Mem.t * G.t) coq_AccPair

val run_only : param -> InterNode.t -> cmd -> (Mem.t * G.t) -> Mem.t * G.t

