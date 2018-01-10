open BinNat

val ascii_rect :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
  char -> 'a1

val ascii_rec :
  (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
  char -> 'a1

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : int -> char

val ascii_of_N : int -> char

val ascii_of_nat : int -> char

val coq_N_of_digits : bool list -> int

val coq_N_of_ascii : char -> int

val nat_of_ascii : char -> int

val coq_Space : char

val coq_DoubleQuote : char

val coq_Beep : char

