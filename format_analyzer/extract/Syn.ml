open Pos
open TStr

type pid_t = string_t

type vid_t = string_t

type constant =
| CInt64 of int option
| CChr of int
| CReal of int * int
| CEnum

(** val constant_rect :
    (int option -> 'a1) -> (int -> 'a1) -> (int -> int -> 'a1) -> 'a1 ->
    constant -> 'a1 **)

let constant_rect f f0 f1 f2 = function
| CInt64 x -> f x
| CChr x -> f0 x
| CReal (x, x0) -> f1 x x0
| CEnum -> f2

(** val constant_rec :
    (int option -> 'a1) -> (int -> 'a1) -> (int -> int -> 'a1) -> 'a1 ->
    constant -> 'a1 **)

let constant_rec f f0 f1 f2 = function
| CInt64 x -> f x
| CChr x -> f0 x
| CReal (x, x0) -> f1 x x0
| CEnum -> f2

type unop =
| Neg
| BNot
| LNot

(** val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

let unop_rect f f0 f1 = function
| Neg -> f
| BNot -> f0
| LNot -> f1

(** val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

let unop_rec f f0 f1 = function
| Neg -> f
| BNot -> f0
| LNot -> f1

type binop =
| PlusA
| PlusPI
| IndexPI
| MinusA
| MinusPI
| MinusPP
| Mult
| Div
| Mod
| Shiftlt
| Shiftrt
| Lt
| Gt
| Le
| Ge
| Eq
| Ne
| BAnd
| BXor
| BOr
| LAnd
| LOr

(** val binop_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> binop -> 'a1 **)

let binop_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 = function
| PlusA -> f
| PlusPI -> f0
| IndexPI -> f1
| MinusA -> f2
| MinusPI -> f3
| MinusPP -> f4
| Mult -> f5
| Div -> f6
| Mod -> f7
| Shiftlt -> f8
| Shiftrt -> f9
| Lt -> f10
| Gt -> f11
| Le -> f12
| Ge -> f13
| Eq -> f14
| Ne -> f15
| BAnd -> f16
| BXor -> f17
| BOr -> f18
| LAnd -> f19
| LOr -> f20

(** val binop_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> binop -> 'a1 **)

let binop_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 = function
| PlusA -> f
| PlusPI -> f0
| IndexPI -> f1
| MinusA -> f2
| MinusPI -> f3
| MinusPP -> f4
| Mult -> f5
| Div -> f6
| Mod -> f7
| Shiftlt -> f8
| Shiftrt -> f9
| Lt -> f10
| Gt -> f11
| Le -> f12
| Ge -> f13
| Eq -> f14
| Ne -> f15
| BAnd -> f16
| BXor -> f17
| BOr -> f18
| LAnd -> f19
| LOr -> f20

(** val binop_dec : binop -> binop -> bool **)

let binop_dec x y =
  match x with
  | PlusA ->
    (match y with
     | PlusA -> true
     | _ -> false)
  | PlusPI ->
    (match y with
     | PlusPI -> true
     | _ -> false)
  | IndexPI ->
    (match y with
     | IndexPI -> true
     | _ -> false)
  | MinusA ->
    (match y with
     | MinusA -> true
     | _ -> false)
  | MinusPI ->
    (match y with
     | MinusPI -> true
     | _ -> false)
  | MinusPP ->
    (match y with
     | MinusPP -> true
     | _ -> false)
  | Mult ->
    (match y with
     | Mult -> true
     | _ -> false)
  | Div ->
    (match y with
     | Div -> true
     | _ -> false)
  | Mod ->
    (match y with
     | Mod -> true
     | _ -> false)
  | Shiftlt ->
    (match y with
     | Shiftlt -> true
     | _ -> false)
  | Shiftrt ->
    (match y with
     | Shiftrt -> true
     | _ -> false)
  | Lt ->
    (match y with
     | Lt -> true
     | _ -> false)
  | Gt ->
    (match y with
     | Gt -> true
     | _ -> false)
  | Le ->
    (match y with
     | Le -> true
     | _ -> false)
  | Ge ->
    (match y with
     | Ge -> true
     | _ -> false)
  | Eq ->
    (match y with
     | Eq -> true
     | _ -> false)
  | Ne ->
    (match y with
     | Ne -> true
     | _ -> false)
  | BAnd ->
    (match y with
     | BAnd -> true
     | _ -> false)
  | BXor ->
    (match y with
     | BXor -> true
     | _ -> false)
  | BOr ->
    (match y with
     | BOr -> true
     | _ -> false)
  | LAnd ->
    (match y with
     | LAnd -> true
     | _ -> false)
  | LOr ->
    (match y with
     | LOr -> true
     | _ -> false)

type exp =
| Const of constant * pos_t
| Lval of lval * pos_t
| SizeOf of int option * pos_t
| SizeOfE of int option * pos_t
| SizeOfStr of char list * pos_t
| AlignOf of int * pos_t
| AlignOfE of exp * pos_t
| UnOp of unop * exp * pos_t
| BinOp of binop * exp * exp * pos_t
| Question of exp * exp * exp * pos_t
| CastE of int option * exp * pos_t
| AddrOf of lval * pos_t
| StartOf of lval * pos_t
and lval =
| Coq_lval_intro of lhost * offset * pos_t
and lhost =
| VarLhost of vid_t * bool
| MemLhost of exp
and offset =
| NoOffset
| FOffset of vid_t * offset
| IOffset of exp * offset

(** val exp_rect :
    (constant -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (int option ->
    pos_t -> 'a1) -> (int option -> pos_t -> 'a1) -> (char list -> pos_t ->
    'a1) -> (int -> pos_t -> 'a1) -> (exp -> 'a1 -> pos_t -> 'a1) -> (unop ->
    exp -> 'a1 -> pos_t -> 'a1) -> (binop -> exp -> 'a1 -> exp -> 'a1 ->
    pos_t -> 'a1) -> (exp -> 'a1 -> exp -> 'a1 -> exp -> 'a1 -> pos_t -> 'a1)
    -> (int option -> exp -> 'a1 -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1)
    -> (lval -> pos_t -> 'a1) -> exp -> 'a1 **)

let rec exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 = function
| Const (c, pos) -> f c pos
| Lval (lv, pos) -> f0 lv pos
| SizeOf (i, pos) -> f1 i pos
| SizeOfE (i, pos) -> f2 i pos
| SizeOfStr (i, pos) -> f3 i pos
| AlignOf (i, pos) -> f4 i pos
| AlignOfE (e0, pos) ->
  f5 e0 (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| UnOp (u, e0, pos) ->
  f6 u e0 (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| BinOp (b, e1, e2, pos) ->
  f7 b e1 (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e1) e2
    (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e2) pos
| Question (c, c_true, c_false, pos) ->
  f8 c (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c) c_true
    (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c_true) c_false
    (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c_false) pos
| CastE (i, e0, pos) ->
  f9 i e0 (exp_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| AddrOf (lv, pos) -> f10 lv pos
| StartOf (lv, pos) -> f11 lv pos

(** val exp_rec :
    (constant -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (int option ->
    pos_t -> 'a1) -> (int option -> pos_t -> 'a1) -> (char list -> pos_t ->
    'a1) -> (int -> pos_t -> 'a1) -> (exp -> 'a1 -> pos_t -> 'a1) -> (unop ->
    exp -> 'a1 -> pos_t -> 'a1) -> (binop -> exp -> 'a1 -> exp -> 'a1 ->
    pos_t -> 'a1) -> (exp -> 'a1 -> exp -> 'a1 -> exp -> 'a1 -> pos_t -> 'a1)
    -> (int option -> exp -> 'a1 -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1)
    -> (lval -> pos_t -> 'a1) -> exp -> 'a1 **)

let rec exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 = function
| Const (c, pos) -> f c pos
| Lval (lv, pos) -> f0 lv pos
| SizeOf (i, pos) -> f1 i pos
| SizeOfE (i, pos) -> f2 i pos
| SizeOfStr (i, pos) -> f3 i pos
| AlignOf (i, pos) -> f4 i pos
| AlignOfE (e0, pos) ->
  f5 e0 (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| UnOp (u, e0, pos) ->
  f6 u e0 (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| BinOp (b, e1, e2, pos) ->
  f7 b e1 (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e1) e2
    (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e2) pos
| Question (c, c_true, c_false, pos) ->
  f8 c (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c) c_true
    (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c_true) c_false
    (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 c_false) pos
| CastE (i, e0, pos) ->
  f9 i e0 (exp_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 e0) pos
| AddrOf (lv, pos) -> f10 lv pos
| StartOf (lv, pos) -> f11 lv pos

(** val lval_rect : (lhost -> offset -> pos_t -> 'a1) -> lval -> 'a1 **)

let lval_rect f = function
| Coq_lval_intro (x, x0, x1) -> f x x0 x1

(** val lval_rec : (lhost -> offset -> pos_t -> 'a1) -> lval -> 'a1 **)

let lval_rec f = function
| Coq_lval_intro (x, x0, x1) -> f x x0 x1

(** val lhost_rect :
    (vid_t -> bool -> 'a1) -> (exp -> 'a1) -> lhost -> 'a1 **)

let lhost_rect f f0 = function
| VarLhost (x, x0) -> f x x0
| MemLhost x -> f0 x

(** val lhost_rec :
    (vid_t -> bool -> 'a1) -> (exp -> 'a1) -> lhost -> 'a1 **)

let lhost_rec f f0 = function
| VarLhost (x, x0) -> f x x0
| MemLhost x -> f0 x

(** val offset_rect :
    'a1 -> (vid_t -> offset -> 'a1 -> 'a1) -> (exp -> offset -> 'a1 -> 'a1)
    -> offset -> 'a1 **)

let rec offset_rect f f0 f1 = function
| NoOffset -> f
| FOffset (f2, o0) -> f0 f2 o0 (offset_rect f f0 f1 o0)
| IOffset (i, o0) -> f1 i o0 (offset_rect f f0 f1 o0)

(** val offset_rec :
    'a1 -> (vid_t -> offset -> 'a1 -> 'a1) -> (exp -> offset -> 'a1 -> 'a1)
    -> offset -> 'a1 **)

let rec offset_rec f f0 f1 = function
| NoOffset -> f
| FOffset (f2, o0) -> f0 f2 o0 (offset_rec f f0 f1 o0)
| IOffset (i, o0) -> f1 i o0 (offset_rec f f0 f1 o0)

type alloc =
  exp
  (* singleton inductive, whose constructor was Array *)

(** val alloc_rect : (exp -> 'a1) -> alloc -> 'a1 **)

let alloc_rect f a =
  f a

(** val alloc_rec : (exp -> 'a1) -> alloc -> 'a1 **)

let alloc_rec f a =
  f a

type cmd =
| Cset of lval * exp * pos_t
| Cexternal of lval * pos_t
| Calloc of lval * alloc * pos_t
| Csalloc of lval * char list * pos_t
| Cfalloc of lval * pid_t * pos_t
| Cassume of exp * pos_t
| Ccall of lval option * exp * exp list * pos_t
| Creturn of exp option * pos_t
| Casm of pos_t
| Cskip of pos_t

(** val cmd_rect :
    (lval -> exp -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval -> alloc
    -> pos_t -> 'a1) -> (lval -> char list -> pos_t -> 'a1) -> (lval -> pid_t
    -> pos_t -> 'a1) -> (exp -> pos_t -> 'a1) -> (lval option -> exp -> exp
    list -> pos_t -> 'a1) -> (exp option -> pos_t -> 'a1) -> (pos_t -> 'a1)
    -> (pos_t -> 'a1) -> cmd -> 'a1 **)

let cmd_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 = function
| Cset (x, x0, x1) -> f x x0 x1
| Cexternal (x, x0) -> f0 x x0
| Calloc (x, x0, x1) -> f1 x x0 x1
| Csalloc (x, x0, x1) -> f2 x x0 x1
| Cfalloc (x, x0, x1) -> f3 x x0 x1
| Cassume (x, x0) -> f4 x x0
| Ccall (x, x0, x1, x2) -> f5 x x0 x1 x2
| Creturn (x, x0) -> f6 x x0
| Casm x -> f7 x
| Cskip x -> f8 x

(** val cmd_rec :
    (lval -> exp -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval -> alloc
    -> pos_t -> 'a1) -> (lval -> char list -> pos_t -> 'a1) -> (lval -> pid_t
    -> pos_t -> 'a1) -> (exp -> pos_t -> 'a1) -> (lval option -> exp -> exp
    list -> pos_t -> 'a1) -> (exp option -> pos_t -> 'a1) -> (pos_t -> 'a1)
    -> (pos_t -> 'a1) -> cmd -> 'a1 **)

let cmd_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 = function
| Cset (x, x0, x1) -> f x x0 x1
| Cexternal (x, x0) -> f0 x x0
| Calloc (x, x0, x1) -> f1 x x0 x1
| Csalloc (x, x0, x1) -> f2 x x0 x1
| Cfalloc (x, x0, x1) -> f3 x x0 x1
| Cassume (x, x0) -> f4 x x0
| Ccall (x, x0, x1, x2) -> f5 x x0 x1 x2
| Creturn (x, x0) -> f6 x x0
| Casm x -> f7 x
| Cskip x -> f8 x

(** val pos_of_exp : exp -> pos_t **)

let pos_of_exp = function
| Const (c, pos) -> pos
| Lval (lv, pos) -> pos
| SizeOf (i, pos) -> pos
| SizeOfE (i, pos) -> pos
| SizeOfStr (i, pos) -> pos
| AlignOf (i, pos) -> pos
| AlignOfE (e0, pos) -> pos
| UnOp (u, e0, pos) -> pos
| BinOp (b, e1, e2, pos) -> pos
| Question (c, c_true, c_false, pos) -> pos
| CastE (i, e0, pos) -> pos
| AddrOf (lv, pos) -> pos
| StartOf (lv, pos) -> pos

(** val pos_of_lv : lval -> pos_t **)

let pos_of_lv = function
| Coq_lval_intro (lh, o, pos) -> pos

