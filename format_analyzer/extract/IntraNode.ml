open BinInt
open Datatypes
open VocabA

type t' =
| Entry
| Exit
| Node of int

(** val t'_rect : 'a1 -> 'a1 -> (int -> 'a1) -> t' -> 'a1 **)

let t'_rect f f0 f1 = function
| Entry -> f
| Exit -> f0
| Node x -> f1 x

(** val t'_rec : 'a1 -> 'a1 -> (int -> 'a1) -> t' -> 'a1 **)

let t'_rec f f0 f1 = function
| Entry -> f
| Exit -> f0
| Node x -> f1 x

type t = t'

(** val compare : t -> t -> t OrderedType.coq_Compare **)

let compare x y =
  match x with
  | Entry ->
    (match y with
     | Entry -> OrderedType.EQ
     | _ -> OrderedType.LT)
  | Exit ->
    (match y with
     | Exit -> OrderedType.EQ
     | _ -> OrderedType.GT)
  | Node i ->
    (match y with
     | Entry -> OrderedType.GT
     | Exit -> OrderedType.LT
     | Node j ->
       (match Z.compare i j with
        | Eq -> OrderedType.EQ
        | Lt -> OrderedType.LT
        | Gt -> OrderedType.GT))

(** val eq_dec : t -> t -> bool **)

let eq_dec x y =
  if physical_eq x y
  then true
  else (match x with
        | Entry ->
          (match y with
           | Entry -> true
           | _ -> false)
        | Exit ->
          (match y with
           | Exit -> true
           | _ -> false)
        | Node i ->
          (match y with
           | Node j -> Z.eq_dec i j
           | _ -> false))

(** val is_entry_node : t -> bool **)

let is_entry_node node =
  if eq_dec node Entry then true else false

(** val is_exit_node : t -> bool **)

let is_exit_node node =
  if eq_dec node Exit then true else false

