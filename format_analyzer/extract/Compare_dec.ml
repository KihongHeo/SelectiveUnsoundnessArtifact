open Datatypes

(** val zerop : int -> bool **)

let zerop n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    true)
    (fun n0 ->
    false)
    n

(** val lt_eq_lt_dec : int -> int -> bool option **)

let rec lt_eq_lt_dec n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Some
      false)
      (fun m0 -> Some
      true)
      m)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      None)
      (fun m0 ->
      lt_eq_lt_dec n0 m0)
      m)
    n

(** val gt_eq_gt_dec : int -> int -> bool option **)

let gt_eq_gt_dec n m =
  lt_eq_lt_dec n m

(** val le_lt_dec : int -> int -> bool **)

let rec le_lt_dec n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    true)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      false)
      (fun m0 ->
      le_lt_dec n0 m0)
      m)
    n

(** val le_le_S_dec : int -> int -> bool **)

let le_le_S_dec n m =
  le_lt_dec n m

(** val le_ge_dec : int -> int -> bool **)

let le_ge_dec n m =
  le_lt_dec n m

(** val le_gt_dec : int -> int -> bool **)

let le_gt_dec n m =
  le_lt_dec n m

(** val le_lt_eq_dec : int -> int -> bool **)

let le_lt_eq_dec n m =
  let s = lt_eq_lt_dec n m in
  (match s with
   | Some s0 -> s0
   | None -> assert false (* absurd case *))

(** val le_dec : int -> int -> bool **)

let le_dec n m =
  le_gt_dec n m

(** val lt_dec : int -> int -> bool **)

let lt_dec n m =
  le_dec (Pervasives.succ n) m

(** val gt_dec : int -> int -> bool **)

let gt_dec n m =
  lt_dec m n

(** val ge_dec : int -> int -> bool **)

let ge_dec n m =
  le_dec m n

(** val nat_compare : int -> int -> comparison **)

let rec nat_compare n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      Eq)
      (fun n0 ->
      Lt)
      m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      Gt)
      (fun m' ->
      nat_compare n' m')
      m)
    n

(** val nat_compare_alt : int -> int -> comparison **)

let nat_compare_alt n m =
  match lt_eq_lt_dec n m with
  | Some s -> if s then Lt else Eq
  | None -> Gt

(** val leb : int -> int -> bool **)

let rec leb m x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    true)
    (fun m' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      false)
      (fun n' ->
      leb m' n')
      x)
    m

