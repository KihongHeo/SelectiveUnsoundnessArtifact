open BinInt
open Datatypes
open VocabA

type t' =
| Entry
| Exit
| Node of int

val t'_rect : 'a1 -> 'a1 -> (int -> 'a1) -> t' -> 'a1

val t'_rec : 'a1 -> 'a1 -> (int -> 'a1) -> t' -> 'a1

type t = t'

val compare : t -> t -> t OrderedType.coq_Compare

val eq_dec : t -> t -> bool

val is_entry_node : t -> bool

val is_exit_node : t -> bool

