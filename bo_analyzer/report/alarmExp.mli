(** Alarm Cil.expression *)
type t = 
  | ArrayExp of Cil.lval * Cil.exp * Cil.location
  | DerefExp of Cil.exp * Cil.location
  | DivExp of Cil.exp * Cil.exp * Cil.location
  | Strcpy of Cil.exp * Cil.exp * Cil.location 
  | Strcat of Cil.exp * Cil.exp * Cil.location 
  | Strncpy of Cil.exp * Cil.exp * Cil.exp * Cil.location 
  | Memcpy of Cil.exp * Cil.exp * Cil.exp *  Cil.location
  | Memmove of Cil.exp * Cil.exp * Cil.exp * Cil.location

val collect : IntraCfg.cmd -> t list
val to_string : t -> string
