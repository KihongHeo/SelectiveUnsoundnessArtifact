(** Product domain  *)

open AbsDom
open Vocab

module Make4 (A:LAT) (B:LAT) (C:LAT) (D:LAT) =
struct
  type t = A.t * B.t * C.t * D.t
  
  let make (a,b,c,d) = (a,b,c,d)

  let compare = compare
  let fst (a,_,_,_) = a
  let snd (_,b,_,_) = b
  let trd (_,_,c,_) = c
  let frth (_,_,_,d) = d


  let le ((a1,b1,c1,d1) as x) ((a2,b2,c2,d2) as y) = 
    if x == y then true
    else (A.le a1 a2) && (B.le b1 b2) && (C.le c1 c2) && (D.le d1 d2)
  let eq ((a1,b1,c1,d1) as x) ((a2,b2,c2,d2) as y) = 
    if x == y then true
    else (A.eq a1 a2) && (B.eq b1 b2) && (C.eq c1 c2) && (D.eq d1 d2)
  let join ((a1,b1,c1,d1) as x) ((a2,b2,c2,d2) as y) = 
    if x == y then y
    else (A.join a1 a2, B.join b1 b2, C.join c1 c2, D.join d1 d2)
  let meet (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.meet a1 a2, B.meet b1 b2, C.meet c1 c2, D.meet d1 d2)
  let widen (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.widen a1 a2, B.widen b1 b2, C.widen c1 c2, D.widen d1 d2)
  let narrow (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.narrow a1 a2, B.narrow b1 b2, C.narrow c1 c2, D.narrow d1 d2)

  let bot = (A.bot, B.bot, C.bot, D.bot)

  let to_string x = 
    "("^(A.to_string (fst x))^", "^(B.to_string (snd x))^", "^(C.to_string (trd x))
    ^", "^(D.to_string (frth x))^")"
end

module Make5 (A:LAT) (B:LAT) (C:LAT) (D:LAT) (E:LAT) =
struct
  type t = A.t * B.t * C.t * D.t * E.t

  let compare = compare
  let fst (a,_,_,_,_) = a
  let snd (_,b,_,_,_) = b
  let trd (_,_,c,_,_) = c
  let frth (_,_,_,d,_) = d
  let fifth (_,_,_,_,e) = e

  let make (a,b,c,d,e) = (a,b,c,d,e)
  let le ((a1,b1,c1,d1,e1) as x) ((a2,b2,c2,d2,e2) as y) = 
    if x == y then true
    else (A.le a1 a2) && (B.le b1 b2) && (C.le c1 c2) && (D.le d1 d2) && (E.le e1 e2)
  let eq ((a1,b1,c1,d1,e1) as x) ((a2,b2,c2,d2,e2) as y) = 
    if x == y then true
    else (A.eq a1 a2) && (B.eq b1 b2) && (C.eq c1 c2) && (D.eq d1 d2) && (E.eq e1 e2)
  let join ((a1,b1,c1,d1,e1) as x) ((a2,b2,c2,d2,e2) as y) = 
    if x == y then y
    else (A.join a1 a2, B.join b1 b2, C.join c1 c2, D.join d1 d2, E.join e1 e2)
  let meet (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.meet a1 a2, B.meet b1 b2, C.meet c1 c2, D.meet d1 d2, E.meet e1 e2)
  let widen (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.widen a1 a2, B.widen b1 b2, C.widen c1 c2, D.widen d1 d2, E.widen e1 e2)
  let narrow (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.narrow a1 a2, B.narrow b1 b2, C.narrow c1 c2, D.narrow d1 d2, E.narrow e1 e2)

  let bot = (A.bot, B.bot, C.bot, D.bot, E.bot)

  let to_string x = 
    "("^(A.to_string (fst x))^", "^(B.to_string (snd x))^", "^(C.to_string (trd x))
    ^", "^(D.to_string (frth x))^", "^(E.to_string (fifth x))^")"
end
