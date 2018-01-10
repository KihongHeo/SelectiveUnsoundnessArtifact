open Datatypes
open DomAbs
open FMapAVL
open List0
open TStr

type __ = Obj.t

type arg_t =
| FixArg
| VaArg

(** val arg_t_rect : 'a1 -> 'a1 -> arg_t -> 'a1 **)

let arg_t_rect f f0 = function
| FixArg -> f
| VaArg -> f0

(** val arg_t_rec : 'a1 -> 'a1 -> arg_t -> 'a1 **)

let arg_t_rec f f0 = function
| FixArg -> f
| VaArg -> f0

type src_t =
| InSrc
| ExSrc

(** val src_t_rect : 'a1 -> 'a1 -> src_t -> 'a1 **)

let src_t_rect f f0 = function
| InSrc -> f
| ExSrc -> f0

(** val src_t_rec : 'a1 -> 'a1 -> src_t -> 'a1 **)

let src_t_rec f f0 = function
| InSrc -> f
| ExSrc -> f0

type alloc_t =
| NoAlloc
| DoAlloc

(** val alloc_t_rect : 'a1 -> 'a1 -> alloc_t -> 'a1 **)

let alloc_t_rect f f0 = function
| NoAlloc -> f
| DoAlloc -> f0

(** val alloc_t_rec : 'a1 -> 'a1 -> alloc_t -> 'a1 **)

let alloc_t_rec f f0 = function
| NoAlloc -> f
| DoAlloc -> f0

type argument_t =
| Src of arg_t
| Dst of arg_t * alloc_t * src_t
| SkipArg

(** val argument_t_rect :
    (arg_t -> 'a1) -> (arg_t -> alloc_t -> src_t -> 'a1) -> 'a1 -> argument_t
    -> 'a1 **)

let argument_t_rect f f0 f1 = function
| Src x -> f x
| Dst (x, x0, x1) -> f0 x x0 x1
| SkipArg -> f1

(** val argument_t_rec :
    (arg_t -> 'a1) -> (arg_t -> alloc_t -> src_t -> 'a1) -> 'a1 -> argument_t
    -> 'a1 **)

let argument_t_rec f f0 f1 = function
| Src x -> f x
| Dst (x, x0, x1) -> f0 x x0 x1
| SkipArg -> f1

type ret_t =
| CleanRet
| TaintRet
| SrcRet of alloc_t
| DstRet

(** val ret_t_rect :
    'a1 -> 'a1 -> (alloc_t -> 'a1) -> 'a1 -> ret_t -> 'a1 **)

let ret_t_rect f f0 f1 f2 = function
| CleanRet -> f
| TaintRet -> f0
| SrcRet x -> f1 x
| DstRet -> f2

(** val ret_t_rec : 'a1 -> 'a1 -> (alloc_t -> 'a1) -> 'a1 -> ret_t -> 'a1 **)

let ret_t_rec f f0 f1 f2 = function
| CleanRet -> f
| TaintRet -> f0
| SrcRet x -> f1 x
| DstRet -> f2

type t = { args : argument_t list; ret : ret_t }

(** val t_rect : (argument_t list -> ret_t -> 'a1) -> t -> 'a1 **)

let t_rect f t0 =
  let { args = x; ret = x0 } = t0 in f x x0

(** val t_rec : (argument_t list -> ret_t -> 'a1) -> t -> 'a1 **)

let t_rec f t0 =
  let { args = x; ret = x0 } = t0 in f x x0

(** val args : t -> argument_t list **)

let args x = x.args

(** val ret : t -> ret_t **)

let ret x = x.ret

(** val src : argument_t **)

let src =
  Src FixArg

(** val src_va : argument_t **)

let src_va =
  Src VaArg

(** val src_ret : ret_t **)

let src_ret =
  SrcRet NoAlloc

(** val src_ret_alloc : ret_t **)

let src_ret_alloc =
  SrcRet DoAlloc

(** val dst : argument_t **)

let dst =
  Dst (FixArg, NoAlloc, InSrc)

(** val dst_va : argument_t **)

let dst_va =
  Dst (VaArg, NoAlloc, InSrc)

(** val dst_ext : argument_t **)

let dst_ext =
  Dst (FixArg, NoAlloc, ExSrc)

(** val dst_va_ext : argument_t **)

let dst_va_ext =
  Dst (VaArg, NoAlloc, ExSrc)

(** val dst_alloc : argument_t **)

let dst_alloc =
  Dst (FixArg, DoAlloc, InSrc)

(** val dst_ret : ret_t **)

let dst_ret =
  DstRet

(** val skip : argument_t **)

let skip =
  SkipArg

(** val clean_ret : ret_t **)

let clean_ret =
  CleanRet

(** val taint_ret : ret_t **)

let taint_ret =
  TaintRet

(** val get_dstsrc_list :
    Val.t list -> argument_t list -> ((Val.t * alloc_t) * src_t) list ->
    Val.t list -> ((Val.t * alloc_t) * src_t) list * Val.t list **)

let rec get_dstsrc_list vs args0 dsts srcs =
  match vs with
  | [] ->
    (match args0 with
     | [] -> (dsts, srcs)
     | a :: argtl ->
       (match a with
        | Src va_arg ->
          (match va_arg with
           | FixArg -> (dsts, srcs)
           | VaArg -> get_dstsrc_list [] argtl dsts (app vs srcs))
        | Dst (va_arg, alloc, src0) ->
          (match va_arg with
           | FixArg -> (dsts, srcs)
           | VaArg ->
             let dsts' = map (fun v -> ((v, alloc), src0)) vs in
             get_dstsrc_list [] argtl (app dsts' dsts) srcs)
        | SkipArg -> (dsts, srcs)))
  | v :: vtl ->
    (match args0 with
     | [] -> (dsts, srcs)
     | a :: argtl ->
       (match a with
        | Src va_arg ->
          (match va_arg with
           | FixArg -> get_dstsrc_list vtl argtl dsts (v :: srcs)
           | VaArg -> get_dstsrc_list [] argtl dsts (app vs srcs))
        | Dst (va_arg, alloc, src0) ->
          (match va_arg with
           | FixArg ->
             get_dstsrc_list vtl argtl (((v, alloc), src0) :: dsts) srcs
           | VaArg ->
             let dsts' = map (fun v0 -> ((v0, alloc), src0)) vs in
             get_dstsrc_list [] argtl (app dsts' dsts) srcs)
        | SkipArg -> get_dstsrc_list vtl argtl dsts srcs))

module M = Make(DStr.DStr)

(** val empty : 'a1 M.t **)

let empty =
  M.empty

(** val add : M.key -> 'a1 -> 'a1 M.t -> 'a1 M.t **)

let add x e m =
  M.add x e m

(** val find : M.key -> 'a1 M.t -> 'a1 option **)

let find x m =
  M.find x m

