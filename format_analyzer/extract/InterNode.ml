open Datatypes
open IntraNode
open OrdersTac
open Syn
open TStr

module Pid = String_as_OT

module MO1 = 
 struct 
  module TO = 
   struct 
    type t = string_t
   end
  
  module IsTO = 
   struct 
    
   end
  
  module OrderTac = MakeOrderTac(TO)(IsTO)
  
  (** val eq_dec : string_t -> string_t -> bool **)
  
  let eq_dec =
    Pid.eq_dec
  
  (** val lt_dec : string_t -> string_t -> bool **)
  
  let lt_dec x y =
    match Pid.compare x y with
    | OrderedType.LT -> true
    | _ -> false
  
  (** val eqb : string_t -> string_t -> bool **)
  
  let eqb x y =
    if eq_dec x y then true else false
 end

module MO2 = 
 struct 
  module TO = 
   struct 
    type t = t'
   end
  
  module IsTO = 
   struct 
    
   end
  
  module OrderTac = MakeOrderTac(TO)(IsTO)
  
  (** val eq_dec : t' -> t' -> bool **)
  
  let eq_dec =
    eq_dec
  
  (** val lt_dec : t' -> t' -> bool **)
  
  let lt_dec x y =
    match compare x y with
    | OrderedType.LT -> true
    | _ -> false
  
  (** val eqb : t' -> t' -> bool **)
  
  let eqb x y =
    if eq_dec x y then true else false
 end

type t = string_t * t'

(** val compare : t -> t -> (string_t * t') OrderedType.coq_Compare **)

let compare x y =
  let (x1, x2) = x in
  let (y1, y2) = y in
  let c = Pid.compare x1 y1 in
  (match c with
   | OrderedType.LT -> OrderedType.LT
   | OrderedType.EQ ->
     let c0 = compare x2 y2 in
     (match c0 with
      | OrderedType.LT -> OrderedType.LT
      | OrderedType.EQ -> OrderedType.EQ
      | OrderedType.GT -> OrderedType.GT)
   | OrderedType.GT -> OrderedType.GT)

(** val eq_dec : t -> t -> bool **)

let eq_dec x y =
  match compare x y with
  | OrderedType.EQ -> true
  | _ -> false

(** val get_pid : t -> pid_t **)

let get_pid node =
  fst node

(** val get_node : t -> IntraNode.t **)

let get_node node =
  snd node

(** val is_entry_node : t -> bool **)

let is_entry_node node =
  is_entry_node (get_node node)

(** val is_exit_node : t -> bool **)

let is_exit_node node =
  is_exit_node (get_node node)

(** val entryof : pid_t -> t **)

let entryof f =
  (f, Entry)

(** val exitof : pid_t -> t **)

let exitof f =
  (f, Exit)

