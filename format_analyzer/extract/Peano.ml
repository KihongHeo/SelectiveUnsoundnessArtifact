(** val pred : int -> int **)

let pred n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    n)
    (fun u ->
    u)
    n

(** val plus : int -> int -> int **)

let rec plus n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    m)
    (fun p -> Pervasives.succ
    (plus p m))
    n

(** val mult : int -> int -> int **)

let rec mult n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    0)
    (fun p ->
    plus m (mult p m))
    n

(** val minus : int -> int -> int **)

let rec minus n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    n)
    (fun k ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      n)
      (fun l ->
      minus k l)
      m)
    n

(** val max : int -> int -> int **)

let rec max n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      n)
      (fun m' -> Pervasives.succ
      (max n' m'))
      m)
    n

(** val min : int -> int -> int **)

let rec min n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    0)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      0)
      (fun m' -> Pervasives.succ
      (min n' m'))
      m)
    n

(** val nat_iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec nat_iter n f x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    x)
    (fun n' ->
    f (nat_iter n' f x))
    n

