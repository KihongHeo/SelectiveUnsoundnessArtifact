open Datatypes
open IntraNode
open OrdersTac
open Syn
open TStr

module Pid : 
 sig 
  type t = string_t
  
  val compare : t -> t -> t OrderedType.coq_Compare
  
  val eq_dec : t -> t -> bool
 end

module MO1 : 
 sig 
  module TO : 
   sig 
    type t = string_t
   end
  
  module IsTO : 
   sig 
    
   end
  
  module OrderTac : 
   sig 
    
   end
  
  val eq_dec : string_t -> string_t -> bool
  
  val lt_dec : string_t -> string_t -> bool
  
  val eqb : string_t -> string_t -> bool
 end

module MO2 : 
 sig 
  module TO : 
   sig 
    type t = t'
   end
  
  module IsTO : 
   sig 
    
   end
  
  module OrderTac : 
   sig 
    
   end
  
  val eq_dec : t' -> t' -> bool
  
  val lt_dec : t' -> t' -> bool
  
  val eqb : t' -> t' -> bool
 end

type t = string_t * t'

val compare : t -> t -> (string_t * t') OrderedType.coq_Compare

val eq_dec : t -> t -> bool

val get_pid : t -> pid_t

val get_node : t -> IntraNode.t

val is_entry_node : t -> bool

val is_exit_node : t -> bool

val entryof : pid_t -> t

val exitof : pid_t -> t

