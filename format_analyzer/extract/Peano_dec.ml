(** val coq_O_or_S : int -> int option **)

let coq_O_or_S n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    None)
    (fun n0 -> Some
    n0)
    n

(** val eq_nat_dec : int -> int -> bool **)

let rec eq_nat_dec n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      true)
      (fun m0 ->
      false)
      m)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      false)
      (fun m0 ->
      eq_nat_dec n0 m0)
      m)
    n

