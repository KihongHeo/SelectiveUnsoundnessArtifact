open BinNat
open BinPos
open Bool
open Datatypes

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Z = 
 struct 
  type t = int
  
  (** val zero : int **)
  
  let zero =
    0
  
  (** val one : int **)
  
  let one =
    1
  
  (** val two : int **)
  
  let two =
    ((fun p->2*p) 1)
  
  (** val double : int -> int **)
  
  let double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p -> ((fun p->2*p)
      p))
      (fun p -> (~-) ((fun p->2*p)
      p))
      x
  
  (** val succ_double : int -> int **)
  
  let succ_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      1)
      (fun p -> ((fun p->1+2*p)
      p))
      (fun p -> (~-)
      (Pos.pred_double p))
      x
  
  (** val pred_double : int -> int **)
  
  let pred_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (~-)
      1)
      (fun p ->
      (Pos.pred_double p))
      (fun p -> (~-) ((fun p->1+2*p)
      p))
      x
  
  (** val pos_sub : int -> int -> int **)
  
  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q ->
        double (pos_sub p q))
        (fun q ->
        succ_double (pos_sub p q))
        (fun _ -> ((fun p->2*p)
        p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q ->
        pred_double (pos_sub p q))
        (fun q ->
        double (pos_sub p q))
        (fun _ ->
        (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (~-) ((fun p->2*p)
        q))
        (fun q -> (~-)
        (Pos.pred_double q))
        (fun _ ->
        0)
        y)
      x
  
  (** val add : int -> int -> int **)
  
  let add = (+)
  
  (** val opp : int -> int **)
  
  let opp = (~-)
  
  (** val succ : int -> int **)
  
  let succ = Pervasives.succ
  
  (** val pred : int -> int **)
  
  let pred = Pervasives.pred
  
  (** val sub : int -> int -> int **)
  
  let sub = (-)
  
  (** val mul : int -> int -> int **)
  
  let mul = ( * )
  
  (** val pow_pos : int -> int -> int **)
  
  let pow_pos z n =
    Pos.iter n (mul z) 1
  
  (** val pow : int -> int -> int **)
  
  let pow x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      1)
      (fun p ->
      pow_pos x p)
      (fun p ->
      0)
      y
  
  (** val square : int -> int **)
  
  let square x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      (Pos.square p))
      (fun p ->
      (Pos.square p))
      x
  
  (** val compare : int -> int -> comparison **)
  
  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt
  
  (** val sgn : int -> int **)
  
  let sgn z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      1)
      (fun p -> (~-)
      1)
      z
  
  (** val leb : int -> int -> bool **)
  
  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true
  
  (** val ltb : int -> int -> bool **)
  
  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false
  
  (** val geb : int -> int -> bool **)
  
  let geb x y =
    match compare x y with
    | Lt -> false
    | _ -> true
  
  (** val gtb : int -> int -> bool **)
  
  let gtb x y =
    match compare x y with
    | Gt -> true
    | _ -> false
  
  (** val eqb : int -> int -> bool **)
  
  let rec eqb x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        true)
        (fun p ->
        false)
        (fun p ->
        false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        false)
        (fun q ->
        Pos.eqb p q)
        (fun p0 ->
        false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        false)
        (fun p0 ->
        false)
        (fun q ->
        Pos.eqb p q)
        y)
      x
  
  (** val max : int -> int -> int **)
  
  let max = Pervasives.max
  
  (** val min : int -> int -> int **)
  
  let min = Pervasives.min
  
  (** val abs : int -> int **)
  
  let abs = Pervasives.abs
  
  (** val abs_nat : int -> int **)
  
  let abs_nat z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      Pos.to_nat p)
      (fun p ->
      Pos.to_nat p)
      z
  
  (** val abs_N : int -> int **)
  
  let abs_N = Pervasives.abs
  
  (** val to_nat : int -> int **)
  
  let to_nat z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      Pos.to_nat p)
      (fun p ->
      0)
      z
  
  (** val to_N : int -> int **)
  
  let to_N z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      p)
      (fun p ->
      0)
      z
  
  (** val of_nat : int -> int **)
  
  let of_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      0)
      (fun n0 ->
      (Pos.of_succ_nat n0))
      n
  
  (** val of_N : int -> int **)
  
  let of_N = fun p -> p
  
  (** val to_pos : int -> int **)
  
  let to_pos z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      1)
      (fun p ->
      p)
      (fun p ->
      1)
      z
  
  (** val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let iter n f x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      x)
      (fun p ->
      Pos.iter p f x)
      (fun p ->
      x)
      n
  
  (** val pos_div_eucl : int -> int -> int * int **)
  
  let rec pos_div_eucl a b =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul ((fun p->2*p) 1) r) 1 in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul ((fun p->2*p) 1) r in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun _ ->
      if leb ((fun p->2*p) 1) b then (0, 1) else (1, 0))
      a
  
  (** val div_eucl : int -> int -> int * int **)
  
  let div_eucl a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0,
      0))
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0,
        0))
        (fun p ->
        pos_div_eucl a' b)
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q),
           0))
           (fun p -> ((opp (add q 1)),
           (add b r)))
           (fun p -> ((opp (add q 1)),
           (add b r)))
           r))
        b)
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0,
        0))
        (fun p ->
        let (q, r) = pos_div_eucl a' b in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q),
           0))
           (fun p0 -> ((opp (add q 1)),
           (sub b r)))
           (fun p0 -> ((opp (add q 1)),
           (sub b r)))
           r))
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in (q, (opp r)))
        b)
      a
  
  (** val div : int -> int -> int **)
  
  let div a b =
    let (q, x) = div_eucl a b in q
  
  (** val modulo : int -> int -> int **)
  
  let modulo a b =
    let (x, r) = div_eucl a b in r
  
  (** val quotrem : int -> int -> int * int **)
  
  let quotrem a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0,
      0))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0,
        a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (of_N r)))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (of_N r)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0,
        a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (opp (of_N r))))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (opp (of_N r))))
        b)
      a
  
  (** val quot : int -> int -> int **)
  
  let quot a b =
    fst (quotrem a b)
  
  (** val rem : int -> int -> int **)
  
  let rem a b =
    snd (quotrem a b)
  
  (** val even : int -> bool **)
  
  let even z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      true)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        false)
        (fun p0 ->
        true)
        (fun _ ->
        false)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        false)
        (fun p0 ->
        true)
        (fun _ ->
        false)
        p)
      z
  
  (** val odd : int -> bool **)
  
  let odd z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      false)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        true)
        (fun p0 ->
        false)
        (fun _ ->
        true)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        true)
        (fun p0 ->
        false)
        (fun _ ->
        true)
        p)
      z
  
  (** val div2 : int -> int **)
  
  let div2 z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        (Pos.div2 p))
        (fun p0 ->
        (Pos.div2 p))
        (fun _ ->
        0)
        p)
      (fun p -> (~-)
      (Pos.div2_up p))
      z
  
  (** val quot2 : int -> int **)
  
  let quot2 z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 ->
        (Pos.div2 p))
        (fun p0 ->
        (Pos.div2 p))
        (fun _ ->
        0)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p0 -> (~-)
        (Pos.div2 p))
        (fun p0 -> (~-)
        (Pos.div2 p))
        (fun _ ->
        0)
        p)
      z
  
  (** val log2 : int -> int **)
  
  let log2 z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p ->
        (Pos.size p))
        (fun p ->
        (Pos.size p))
        (fun _ ->
        0)
        p0)
      (fun p ->
      0)
      z
  
  (** val sqrtrem : int -> int * int **)
  
  let sqrtrem n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0,
      0))
      (fun p ->
      let (s, m) = Pos.sqrtrem p in
      (match m with
       | Pos.IsPos r -> (s, r)
       | _ -> (s, 0)))
      (fun p -> (0,
      0))
      n
  
  (** val sqrt : int -> int **)
  
  let sqrt n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun p ->
      (Pos.sqrt p))
      (fun p ->
      0)
      n
  
  (** val gcd : int -> int -> int **)
  
  let gcd a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      abs b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        abs a)
        (fun b0 ->
        (Pos.gcd a0 b0))
        (fun b0 ->
        (Pos.gcd a0 b0))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        abs a)
        (fun b0 ->
        (Pos.gcd a0 b0))
        (fun b0 ->
        (Pos.gcd a0 b0))
        b)
      a
  
  (** val ggcd : int -> int -> int * (int * int) **)
  
  let ggcd a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> ((abs b), (0,
      (sgn b))))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> ((abs a), ((sgn a),
        0)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in let (aa, bb) = p in (g, (aa, bb)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (aa, ((~-) bb))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> ((abs a), ((sgn a),
        0)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (((~-) aa), bb)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (((~-) aa), ((~-) bb))))
        b)
      a
  
  (** val testbit : int -> int -> bool **)
  
  let testbit a n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      odd a)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        false)
        (fun a0 ->
        Pos.testbit a0 p)
        (fun a0 ->
        negb (N.testbit (Pos.pred_N a0) p))
        a)
      (fun p ->
      false)
      n
  
  (** val shiftl : int -> int -> int **)
  
  let shiftl a n =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      a)
      (fun p ->
      Pos.iter p (mul ((fun p->2*p) 1)) a)
      (fun p ->
      Pos.iter p div2 a)
      n
  
  (** val shiftr : int -> int -> int **)
  
  let shiftr a n =
    shiftl a (opp n)
  
  (** val coq_lor : int -> int -> int **)
  
  let coq_lor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 ->
        (Pos.coq_lor a0 b0))
        (fun b0 -> (~-)
        (N.succ_pos (N.ldiff (Pos.pred_N b0) a0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 -> (~-)
        (N.succ_pos (N.ldiff (Pos.pred_N a0) b0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a
  
  (** val coq_land : int -> int -> int **)
  
  let coq_land a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        0)
        (fun b0 ->
        of_N (Pos.coq_land a0 b0))
        (fun b0 ->
        of_N (N.ldiff a0 (Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        0)
        (fun b0 ->
        of_N (N.ldiff b0 (Pos.pred_N a0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a
  
  (** val ldiff : int -> int -> int **)
  
  let ldiff a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      0)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 ->
        of_N (Pos.ldiff a0 b0))
        (fun b0 ->
        of_N (N.coq_land a0 (Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lor (Pos.pred_N a0) b0)))
        (fun b0 ->
        of_N (N.ldiff (Pos.pred_N b0) (Pos.pred_N a0)))
        b)
      a
  
  (** val coq_lxor : int -> int -> int **)
  
  let coq_lxor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 ->
        of_N (Pos.coq_lxor a0 b0))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lxor a0 (Pos.pred_N b0))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        a)
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lxor (Pos.pred_N a0) b0)))
        (fun b0 ->
        of_N (N.coq_lxor (Pos.pred_N a0) (Pos.pred_N b0)))
        b)
      a
  
  (** val eq_dec : int -> int -> bool **)
  
  let eq_dec x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        true)
        (fun p ->
        false)
        (fun p ->
        false)
        y)
      (fun x0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        false)
        (fun p0 ->
        Pos.eq_dec x0 p0)
        (fun p0 ->
        false)
        y)
      (fun x0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ ->
        false)
        (fun p0 ->
        false)
        (fun p0 ->
        Pos.eq_dec x0 p0)
        y)
      x
  
  module Private_BootStrap = 
   struct 
    
   end
  
  (** val leb_spec0 : int -> int -> reflect **)
  
  let leb_spec0 x y =
    iff_reflect (leb x y)
  
  (** val ltb_spec0 : int -> int -> reflect **)
  
  let ltb_spec0 x y =
    iff_reflect (ltb x y)
  
  module Private_OrderTac = 
   struct 
    module IsTotal = 
     struct 
      
     end
    
    module Tac = 
     struct 
      
     end
   end
  
  (** val sqrt_up : int -> int **)
  
  let sqrt_up a =
    match compare 0 a with
    | Lt -> succ (sqrt (pred a))
    | _ -> 0
  
  (** val log2_up : int -> int **)
  
  let log2_up a =
    match compare 1 a with
    | Lt -> succ (log2 (pred a))
    | _ -> 0
  
  module Private_NZDiv = 
   struct 
    
   end
  
  module Private_Div = 
   struct 
    module Quot2Div = 
     struct 
      (** val div : int -> int -> int **)
      
      let div =
        quot
      
      (** val modulo : int -> int -> int **)
      
      let modulo =
        rem
     end
    
    module NZQuot = 
     struct 
      
     end
   end
  
  (** val lcm : int -> int -> int **)
  
  let lcm a b =
    abs (mul a (div b (gcd a b)))
  
  (** val eqb_spec : int -> int -> reflect **)
  
  let eqb_spec x y =
    iff_reflect (eqb x y)
  
  (** val b2z : bool -> int **)
  
  let b2z = function
  | true -> 1
  | false -> 0
  
  (** val setbit : int -> int -> int **)
  
  let setbit a n =
    coq_lor a (shiftl 1 n)
  
  (** val clearbit : int -> int -> int **)
  
  let clearbit a n =
    ldiff a (shiftl 1 n)
  
  (** val lnot : int -> int **)
  
  let lnot a =
    pred (opp a)
  
  (** val ones : int -> int **)
  
  let ones n =
    pred (shiftl 1 n)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Dec = 
   struct 
    (** val max_case_strong :
        int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__
        -> 'a1) -> 'a1 **)
    
    let max_case_strong n m compat hl hr =
      let c = coq_CompSpec2Type n m (compare n m) in
      (match c with
       | CompGtT -> compat n (max n m) __ (hl __)
       | _ -> compat m (max n m) __ (hr __))
    
    (** val max_case :
        int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let max_case n m x x0 x1 =
      max_case_strong n m x (fun _ -> x0) (fun _ -> x1)
    
    (** val max_dec : int -> int -> bool **)
    
    let max_dec n m =
      max_case n m (fun x y _ h0 -> h0) true false
    
    (** val min_case_strong :
        int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__
        -> 'a1) -> 'a1 **)
    
    let min_case_strong n m compat hl hr =
      let c = coq_CompSpec2Type n m (compare n m) in
      (match c with
       | CompGtT -> compat m (min n m) __ (hr __)
       | _ -> compat n (min n m) __ (hl __))
    
    (** val min_case :
        int -> int -> (int -> int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let min_case n m x x0 x1 =
      min_case_strong n m x (fun _ -> x0) (fun _ -> x1)
    
    (** val min_dec : int -> int -> bool **)
    
    let min_dec n m =
      min_case n m (fun x y _ h0 -> h0) true false
   end
  
  (** val max_case_strong :
      int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n m x x0 =
    Private_Dec.max_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : int -> int -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n m x x0 =
    max_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : int -> int -> bool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong :
      int -> int -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n m x x0 =
    Private_Dec.min_case_strong n m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : int -> int -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n m x x0 =
    min_case_strong n m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : int -> int -> bool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

module Pos2Z = 
 struct 
  
 end

module Z2Pos = 
 struct 
  
 end

