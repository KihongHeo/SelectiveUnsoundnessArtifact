type __ = Obj.t

type 'm coq_Monad = { ret : (__ -> __ -> 'm);
                      bind : (__ -> 'm -> __ -> (__ -> 'm) -> 'm) }

val coq_Monad_rect :
  ((__ -> __ -> 'a1) -> (__ -> 'a1 -> __ -> (__ -> 'a1) -> 'a1) -> __ -> __
  -> __ -> __ -> 'a2) -> 'a1 coq_Monad -> 'a2

val coq_Monad_rec :
  ((__ -> __ -> 'a1) -> (__ -> 'a1 -> __ -> (__ -> 'a1) -> 'a1) -> __ -> __
  -> __ -> __ -> 'a2) -> 'a1 coq_Monad -> 'a2

val ret : 'a1 coq_Monad -> 'a2 -> 'a1

val bind : 'a1 coq_Monad -> 'a1 -> ('a2 -> 'a1) -> 'a1

val wbind : 'a1 coq_Monad -> 'a1 -> 'a1 -> 'a1

val liftM : 'a1 coq_Monad -> ('a2 -> 'a3) -> 'a1 -> 'a1

val join : 'a1 coq_Monad -> 'a1 -> 'a1

val coq_Maybe : __ option coq_Monad

