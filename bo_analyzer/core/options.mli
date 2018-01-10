(** Commandline options *)
val opt_il : bool ref
val opt_cfg : bool ref
val opt_dug : bool ref
val opt_optil : bool ref
val opt_bb : bool ref
val opt_inline : string list ref
val opt_oct : bool ref
val opt_pack_impact : bool ref
val opt_pack_manual : bool ref
val opt_nobar : bool ref
val opt_narrow : bool ref
val opt_profile : bool ref
val opt_scaffold : bool ref
val opt_noalarm : bool ref
val opt_debug : bool ref
val opt_oct_debug : bool ref
val opt_bo : bool ref
val opt_nd : bool ref
val opt_dz : bool ref
val opt_show_all_query : bool ref
val opt_bugfinder :  int ref

val opt_unsound_loop : string BatSet.t ref
val opt_unsound_lib : string BatSet.t ref
val opt_unsound_global : string BatSet.t ref
val opt_unsound_update_all : bool ref

val opt_marshal_in : bool ref
val opt_marshal_out : bool ref
val opt_marshal_dir : string ref

val opt_print_premem : bool ref
val opt_verbose : int ref
val opt_magic : bool ref
val opts : (string * Arg.spec * string) list
