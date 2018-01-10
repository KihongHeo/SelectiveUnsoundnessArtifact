open BinInt

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_Z_of_nat_complete_inf : int -> int **)

let coq_Z_of_nat_complete_inf x =
  Z.to_nat x

(** val coq_Z_of_nat_set : (int -> 'a1) -> int -> 'a1 **)

let coq_Z_of_nat_set h x =
  let s = coq_Z_of_nat_complete_inf x in h s

(** val natlike_rec : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1 **)

let natlike_rec ho hrec x =
  coq_Z_of_nat_set (fun n ->
    let rec f n0 =
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ ->
        ho)
        (fun n1 ->
        hrec (Z.of_nat n1) __ (f n1))
        n0
    in f n) x

(** val natlike_rec2 : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1 **)

let rec natlike_rec2 x x0 z =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    x)
    (fun p ->
    let y = Z.pred p in x0 y __ (natlike_rec2 x x0 y))
    (fun p -> assert false
    (* absurd case *))
    z

(** val natlike_rec3 : 'a1 -> (int -> __ -> 'a1 -> 'a1) -> int -> 'a1 **)

let rec natlike_rec3 x x0 z =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    x)
    (fun p ->
    x0 p __ (let y = Z.pred p in natlike_rec3 x x0 y))
    (fun p -> assert false
    (* absurd case *))
    z

(** val coq_Zlt_0_rec :
    (int -> (int -> __ -> 'a1) -> __ -> 'a1) -> int -> 'a1 **)

let rec coq_Zlt_0_rec x x0 =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ ->
    x 0 (fun y _ -> assert false (* absurd case *)) __)
    (fun p ->
    x p (fun y _ -> coq_Zlt_0_rec x y) __)
    (fun p -> assert false
    (* absurd case *))
    x0

(** val coq_Z_lt_rec : (int -> (int -> __ -> 'a1) -> 'a1) -> int -> 'a1 **)

let coq_Z_lt_rec x x0 =
  coq_Zlt_0_rec (fun x1 x2 _ -> x x1 x2) x0

(** val coq_Zlt_lower_bound_rec :
    int -> (int -> (int -> __ -> 'a1) -> __ -> 'a1) -> int -> 'a1 **)

let coq_Zlt_lower_bound_rec z x x0 =
  coq_Zlt_0_rec (fun x1 iH _ ->
    x (Z.add x1 z) (fun y _ -> iH (Z.sub y z) __) __) (Z.sub x0 z)

