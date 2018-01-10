open Pos
open TStr

type pid_t = string_t

type vid_t = string_t

type constant =
| CInt64 of int option
| CChr of int
| CReal of int * int
| CEnum

val constant_rect :
  (int option -> 'a1) -> (int -> 'a1) -> (int -> int -> 'a1) -> 'a1 ->
  constant -> 'a1

val constant_rec :
  (int option -> 'a1) -> (int -> 'a1) -> (int -> int -> 'a1) -> 'a1 ->
  constant -> 'a1

type unop =
| Neg
| BNot
| LNot

val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

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

val binop_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> binop -> 'a1

val binop_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> binop -> 'a1

val binop_dec : binop -> binop -> bool

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

val exp_rect :
  (constant -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (int option ->
  pos_t -> 'a1) -> (int option -> pos_t -> 'a1) -> (char list -> pos_t ->
  'a1) -> (int -> pos_t -> 'a1) -> (exp -> 'a1 -> pos_t -> 'a1) -> (unop ->
  exp -> 'a1 -> pos_t -> 'a1) -> (binop -> exp -> 'a1 -> exp -> 'a1 -> pos_t
  -> 'a1) -> (exp -> 'a1 -> exp -> 'a1 -> exp -> 'a1 -> pos_t -> 'a1) -> (int
  option -> exp -> 'a1 -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval ->
  pos_t -> 'a1) -> exp -> 'a1

val exp_rec :
  (constant -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (int option ->
  pos_t -> 'a1) -> (int option -> pos_t -> 'a1) -> (char list -> pos_t ->
  'a1) -> (int -> pos_t -> 'a1) -> (exp -> 'a1 -> pos_t -> 'a1) -> (unop ->
  exp -> 'a1 -> pos_t -> 'a1) -> (binop -> exp -> 'a1 -> exp -> 'a1 -> pos_t
  -> 'a1) -> (exp -> 'a1 -> exp -> 'a1 -> exp -> 'a1 -> pos_t -> 'a1) -> (int
  option -> exp -> 'a1 -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval ->
  pos_t -> 'a1) -> exp -> 'a1

val lval_rect : (lhost -> offset -> pos_t -> 'a1) -> lval -> 'a1

val lval_rec : (lhost -> offset -> pos_t -> 'a1) -> lval -> 'a1

val lhost_rect : (vid_t -> bool -> 'a1) -> (exp -> 'a1) -> lhost -> 'a1

val lhost_rec : (vid_t -> bool -> 'a1) -> (exp -> 'a1) -> lhost -> 'a1

val offset_rect :
  'a1 -> (vid_t -> offset -> 'a1 -> 'a1) -> (exp -> offset -> 'a1 -> 'a1) ->
  offset -> 'a1

val offset_rec :
  'a1 -> (vid_t -> offset -> 'a1 -> 'a1) -> (exp -> offset -> 'a1 -> 'a1) ->
  offset -> 'a1

type alloc =
  exp
  (* singleton inductive, whose constructor was Array *)

val alloc_rect : (exp -> 'a1) -> alloc -> 'a1

val alloc_rec : (exp -> 'a1) -> alloc -> 'a1

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

val cmd_rect :
  (lval -> exp -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval -> alloc
  -> pos_t -> 'a1) -> (lval -> char list -> pos_t -> 'a1) -> (lval -> pid_t
  -> pos_t -> 'a1) -> (exp -> pos_t -> 'a1) -> (lval option -> exp -> exp
  list -> pos_t -> 'a1) -> (exp option -> pos_t -> 'a1) -> (pos_t -> 'a1) ->
  (pos_t -> 'a1) -> cmd -> 'a1

val cmd_rec :
  (lval -> exp -> pos_t -> 'a1) -> (lval -> pos_t -> 'a1) -> (lval -> alloc
  -> pos_t -> 'a1) -> (lval -> char list -> pos_t -> 'a1) -> (lval -> pid_t
  -> pos_t -> 'a1) -> (exp -> pos_t -> 'a1) -> (lval option -> exp -> exp
  list -> pos_t -> 'a1) -> (exp option -> pos_t -> 'a1) -> (pos_t -> 'a1) ->
  (pos_t -> 'a1) -> cmd -> 'a1

val pos_of_exp : exp -> pos_t

val pos_of_lv : lval -> pos_t

