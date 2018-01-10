open DLat
open DProd
open DomArrayBlk
open DomBasic

module Val = Prod4(PowExtProcPos)(PowLoc)(ArrayBlk)(PowProc)

(** val pow_proc_pos_of_val : Val.t -> PowExtProcPos.t **)

let pow_proc_pos_of_val =
  Val.fst

(** val pow_loc_of_val : Val.t -> PowLoc.t **)

let pow_loc_of_val =
  Val.snd

(** val array_of_val : Val.t -> ArrayBlk.t **)

let array_of_val =
  Val.thrd

(** val pow_proc_of_val : Val.t -> PowProc.t **)

let pow_proc_of_val =
  Val.frth

(** val val_of_pow_proc_pos : PowExtProcPos.t -> Val.t **)

let val_of_pow_proc_pos x =
  (((x, PowLoc.coq_ILat.bot), ArrayBlk.coq_ILat.bot), PowProc.coq_ILat.bot)

(** val val_of_pow_loc : PowLoc.t -> Val.t **)

let val_of_pow_loc x =
  (((PowExtProcPos.coq_ILat.bot, x), ArrayBlk.coq_ILat.bot),
    PowProc.coq_ILat.bot)

(** val val_of_array : ArrayBlk.t -> Val.t **)

let val_of_array x =
  (((PowExtProcPos.coq_ILat.bot, PowLoc.coq_ILat.bot), x),
    PowProc.coq_ILat.bot)

(** val val_of_pow_proc : PowProc.t -> Val.t **)

let val_of_pow_proc x =
  (((PowExtProcPos.coq_ILat.bot, PowLoc.coq_ILat.bot),
    ArrayBlk.coq_ILat.bot), x)

(** val modify_pow_proc_pos : Val.t -> PowExtProcPos.t -> Val.t **)

let modify_pow_proc_pos x l =
  (((l, (pow_loc_of_val x)), (array_of_val x)), (pow_proc_of_val x))

(** val modify_pow_loc : Val.t -> PowLoc.t -> Val.t **)

let modify_pow_loc x l =
  ((((pow_proc_pos_of_val x), l), (array_of_val x)), (pow_proc_of_val x))

(** val modify_array : Val.t -> ArrayBlk.t -> Val.t **)

let modify_array x a =
  ((((pow_proc_pos_of_val x), (pow_loc_of_val x)), a), (pow_proc_of_val x))

(** val modify_pow_proc : Val.t -> PowProc.t -> Val.t **)

let modify_pow_proc x p =
  ((((pow_proc_pos_of_val x), (pow_loc_of_val x)), (array_of_val x)), p)

