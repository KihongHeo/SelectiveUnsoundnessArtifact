open DLat
open DProd
open DomArrayBlk
open DomBasic

module Val : 
 sig 
  module E2 : 
   sig 
    type t = PowExtProcPos.t * PowLoc.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  module E3 : 
   sig 
    type t = E2.t * ArrayBlk.t
    
    val le_dec : t -> t -> bool
    
    val eq_dec : t -> t -> bool
    
    val bot : t
    
    val join : t -> t -> t
    
    val meet : t -> t -> t
    
    val widen : t -> t -> t
    
    val narrow : t -> t -> t
    
    val coq_ILat : t coq_TCLat
   end
  
  type t = E3.t * PowProc.t
  
  val le_dec : t -> t -> bool
  
  val eq_dec : t -> t -> bool
  
  val bot : t
  
  val join : t -> t -> t
  
  val meet : t -> t -> t
  
  val widen : t -> t -> t
  
  val narrow : t -> t -> t
  
  val coq_ILat : t coq_TCLat
  
  val fst : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a1
  
  val snd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a2
  
  val thrd : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a3
  
  val frth : ((('a1 * 'a2) * 'a3) * 'a4) -> 'a4
 end

val pow_proc_pos_of_val : Val.t -> PowExtProcPos.t

val pow_loc_of_val : Val.t -> PowLoc.t

val array_of_val : Val.t -> ArrayBlk.t

val pow_proc_of_val : Val.t -> PowProc.t

val val_of_pow_proc_pos : PowExtProcPos.t -> Val.t

val val_of_pow_loc : PowLoc.t -> Val.t

val val_of_array : ArrayBlk.t -> Val.t

val val_of_pow_proc : PowProc.t -> Val.t

val modify_pow_proc_pos : Val.t -> PowExtProcPos.t -> Val.t

val modify_pow_loc : Val.t -> PowLoc.t -> Val.t

val modify_array : Val.t -> ArrayBlk.t -> Val.t

val modify_pow_proc : Val.t -> PowProc.t -> Val.t

