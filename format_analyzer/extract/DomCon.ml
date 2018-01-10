open BinInt
open DFMapAVL
open DList
open DNat
open DProd
open DSum
open DUnit
open Datatypes
open InterNode
open IntraNode
open Syn
open TStr

type __ = Obj.t

module Step = Nat

module CallId = Step

module Proc = DStr.DStr

module GVar = DStr.DStr

module LVar = ProdKey3(CallId)(Proc)(DStr.DStr)

module Var = SumKey2(GVar)(LVar)

module ExtAllocsite = SumKey2(Unit)(Proc)

module Allocsite = SumKey2(InterNode)(ExtAllocsite)

module OSS = ProdKey3(Nat)(Nat)(Nat)

module Region = ProdKey3(Step)(Allocsite)(OSS)

module VarRegion = SumKey2(Var)(Region)

module Field = DStr.DStr

module Fields = ListKey(Field)

module Loc = ProdKey2(VarRegion)(Fields)

type val_t = ((Z.t, Loc.t) sum, Proc.t) sum

module M = FMapAVL'.Make(Loc)

type mem_t = val_t M.t

type retLoc_t = ((InterNode.t * Loc.t option) * CallId.t) * Proc.t

type dump_t = retLoc_t list

type mem_pos =
| Inputof
| Outputof

(** val mem_pos_rect : 'a1 -> 'a1 -> mem_pos -> 'a1 **)

let mem_pos_rect f f0 = function
| Inputof -> f
| Outputof -> f0

(** val mem_pos_rec : 'a1 -> 'a1 -> mem_pos -> 'a1 **)

let mem_pos_rec f f0 = function
| Inputof -> f
| Outputof -> f0

type state_t =
  ((((mem_pos * InterNode.t) * CallId.t) * Step.t) * mem_t) * dump_t

(** val val_of_z : int -> val_t **)

let val_of_z z =
  Coq_inl (Coq_inl z)

(** val val_of_loc : Loc.t -> val_t **)

let val_of_loc l =
  Coq_inl (Coq_inr l)

(** val val_of_proc : Proc.t -> val_t **)

let val_of_proc p =
  Coq_inr p

(** val loc_of_gvar : vid_t -> vid_t list -> Loc.t **)

let loc_of_gvar x fs =
  ((Coq_inl (Coq_inl x)), fs)

(** val loc_of_lvar : CallId.t -> Proc.t -> vid_t -> vid_t list -> Loc.t **)

let loc_of_lvar cid p x fs =
  ((Coq_inl (Coq_inr ((cid, p), x))), fs)

(** val loc_of_alloc :
    Step.t -> Allocsite.t -> OSS.t -> vid_t list -> Loc.t **)

let loc_of_alloc step alloc oss fs =
  ((Coq_inr ((step, alloc), oss)), fs)

