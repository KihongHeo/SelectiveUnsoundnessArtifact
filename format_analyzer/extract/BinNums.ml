(** val positive_rect :
    (int -> 'a1 -> 'a1) -> (int -> 'a1 -> 'a1) -> 'a1 -> int -> 'a1 **)

let rec positive_rect f f0 f1 p =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun p0 ->
    f p0 (positive_rect f f0 f1 p0))
    (fun p0 ->
    f0 p0 (positive_rect f f0 f1 p0))
    (fun _ ->
    f1)
    p

(** val positive_rec :
    (int -> 'a1 -> 'a1) -> (int -> 'a1 -> 'a1) -> 'a1 -> int -> 'a1 **)

let rec positive_rec f f0 f1 p =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun p0 ->
    f p0 (positive_rec f f0 f1 p0))
    (fun p0 ->
    f0 p0 (positive_rec f f0 f1 p0))
    (fun _ ->
    f1)
    p

(** val coq_N_rect : 'a1 -> (int -> 'a1) -> int -> 'a1 **)

let coq_N_rect f f0 n =
  (fun f0 fp n -> if n=0 then f0 () else fp n)
    (fun _ ->
    f)
    (fun x ->
    f0 x)
    n

(** val coq_N_rec : 'a1 -> (int -> 'a1) -> int -> 'a1 **)

let coq_N_rec f f0 n =
  (fun f0 fp n -> if n=0 then f0 () else fp n)
    (fun _ ->
    f)
    (fun x ->
    f0 x)
    n

(** val coq_Z_rect : 'a1 -> (int -> 'a1) -> (int -> 'a1) -> int -> 'a1 **)

let coq_Z_rect f f0 f1 z =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    f)
    (fun x ->
    f0 x)
    (fun x ->
    f1 x)
    z

(** val coq_Z_rec : 'a1 -> (int -> 'a1) -> (int -> 'a1) -> int -> 'a1 **)

let coq_Z_rec f f0 f1 z =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    f)
    (fun x ->
    f0 x)
    (fun x ->
    f1 x)
    z

