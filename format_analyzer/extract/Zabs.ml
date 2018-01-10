(** val coq_Zabs_intro : int -> 'a1 -> 'a1 -> 'a1 **)

let coq_Zabs_intro n x x0 =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    x0)
    (fun p ->
    x0)
    (fun p ->
    x)
    n

(** val coq_Zabs_dec : int -> bool **)

let coq_Zabs_dec x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    false)
    (fun p ->
    true)
    (fun p ->
    false)
    x

