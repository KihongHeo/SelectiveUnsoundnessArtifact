open BinInt
open List0
open TStr
open ZArith_dec

(** val c_div : int -> int -> int **)

let c_div x y =
  if coq_Z_ge_dec x 0 then Z.div x y else Z.opp (Z.div (Z.opp x) y)

(** val default : 'a1 -> 'a1 option -> 'a1 **)

let default def = function
| Some v -> v
| None -> def

(** val list_fold : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2 **)

let list_fold f l acc =
  fold_left (fun acc0 x -> f x acc0) l acc

(** val list_fold2_def :
    ('a1 -> 'a2 -> 'a3 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 -> 'a3 -> 'a3 **)

let rec list_fold2_def f l1 l2 acc default0 =
  match l1 with
  | [] ->
    (match l2 with
     | [] -> acc
     | b :: l -> default0)
  | a :: l1' ->
    (match l2 with
     | [] -> default0
     | b :: l2' -> list_fold2_def f l1' l2' (f a b acc) default0)

(** val list_fold2 :
    ('a1 -> 'a2 -> 'a3 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 -> 'a3 **)

let rec list_fold2 f l1 l2 acc =
  match l1 with
  | [] -> acc
  | a :: l1' ->
    (match l2 with
     | [] -> acc
     | b :: l2' -> list_fold2 f l1' l2' (f a b acc))

(** val print : 'a1 -> unit **)

let print = fun _ -> ()

(** val print2 : 'a1 -> 'a2 -> unit **)

let print2 = fun _ _ -> ()

(** val print_when_false :
    ('a1 -> unit) -> 'a1 -> ('a2 -> bool) -> 'a2 -> bool **)

let print_when_false = fun print print_arg step arg ->
 if step arg then true else (print print_arg; false)

(** val small_step : ('a1 -> 'a2) -> 'a1 -> 'a2 **)

let small_step f arg =
  f arg

(** val physical_eq : 'a1 -> 'a1 -> bool **)

let physical_eq = ( == )

(** val structural_eq : 'a1 -> 'a1 -> bool **)

let structural_eq = ( = )

(** val invalid_arg : string_t -> 'a1 **)

let invalid_arg = invalid_arg

