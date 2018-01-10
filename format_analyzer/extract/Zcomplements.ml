open BinInt
open Wf_Z
open Zabs

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val floor_pos : int -> int **)

let rec floor_pos a =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun b' -> (fun p->2*p)
    (floor_pos b'))
    (fun a' -> (fun p->2*p)
    (floor_pos a'))
    (fun _ ->
    1)
    a

(** val floor : int -> int **)

let floor a =
  (floor_pos a)

(** val coq_Z_lt_abs_rec :
    (int -> (int -> __ -> 'a1) -> 'a1) -> int -> 'a1 **)

let coq_Z_lt_abs_rec hP p =
  let h =
    coq_Z_lt_rec (fun x h _ ->
      ((hP x (fun m _ ->
         let (x0, x1) = h (Z.abs m) __ __ in
         if coq_Zabs_dec m then x0 else x1)),
      (hP (Z.opp x) (fun m _ ->
        let (x0, x1) = h (Z.abs m) __ __ in if coq_Zabs_dec m then x0 else x1))))
      (Z.abs p)
  in
  if coq_Zabs_dec p
  then let (x, x0) = h __ in x
  else let (x, x0) = h __ in x0

(** val coq_Zlength_aux : int -> 'a1 list -> int **)

let rec coq_Zlength_aux acc = function
| [] -> acc
| a :: l0 -> coq_Zlength_aux (Z.succ acc) l0

(** val coq_Zlength : 'a1 list -> int **)

let coq_Zlength l =
  coq_Zlength_aux 0 l

