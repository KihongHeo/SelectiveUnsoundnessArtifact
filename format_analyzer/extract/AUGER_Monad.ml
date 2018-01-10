type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 'm coq_Monad = { ret : (__ -> __ -> 'm);
                      bind : (__ -> 'm -> __ -> (__ -> 'm) -> 'm) }

(** val coq_Monad_rect :
    ((__ -> __ -> 'a1) -> (__ -> 'a1 -> __ -> (__ -> 'a1) -> 'a1) -> __ -> __
    -> __ -> __ -> 'a2) -> 'a1 coq_Monad -> 'a2 **)

let coq_Monad_rect f m =
  let { ret = x; bind = x0 } = m in f x x0 __ __ __ __

(** val coq_Monad_rec :
    ((__ -> __ -> 'a1) -> (__ -> 'a1 -> __ -> (__ -> 'a1) -> 'a1) -> __ -> __
    -> __ -> __ -> 'a2) -> 'a1 coq_Monad -> 'a2 **)

let coq_Monad_rec f m =
  let { ret = x; bind = x0 } = m in f x x0 __ __ __ __

(** val ret : 'a1 coq_Monad -> 'a2 -> 'a1 **)

let ret monad x =
  let { ret = ret0; bind = bind0 } = monad in Obj.magic ret0 __ x

(** val bind : 'a1 coq_Monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad x x0 =
  let { ret = ret0; bind = bind0 } = monad in Obj.magic bind0 __ x __ x0

(** val wbind : 'a1 coq_Monad -> 'a1 -> 'a1 -> 'a1 **)

let wbind m ma mb =
  bind m ma (fun x -> mb)

(** val liftM : 'a1 coq_Monad -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let liftM m f ma =
  bind m ma (fun a -> ret m (f a))

(** val join : 'a1 coq_Monad -> 'a1 -> 'a1 **)

let join m mma =
  bind m mma (fun ma -> ma)

(** val coq_Maybe : __ option coq_Monad **)

let coq_Maybe =
  { ret = (Obj.magic (fun _ x -> Some x)); bind = (fun _ m _ f ->
    match m with
    | Some a -> f a
    | None -> None) }

