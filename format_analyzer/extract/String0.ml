(** val string_rect :
    'a1 -> (char -> char list -> 'a1 -> 'a1) -> char list -> 'a1 **)

let rec string_rect f f0 = function
| [] -> f
| a::s0 -> f0 a s0 (string_rect f f0 s0)

(** val string_rec :
    'a1 -> (char -> char list -> 'a1 -> 'a1) -> char list -> 'a1 **)

let rec string_rec f f0 = function
| [] -> f
| a::s0 -> f0 a s0 (string_rec f f0 s0)

(** val string_dec : char list -> char list -> bool **)

let rec string_dec s s0 =
  match s with
  | [] ->
    (match s0 with
     | [] -> true
     | a::s1 -> false)
  | a::s1 ->
    (match s0 with
     | [] -> false
     | a0::s2 -> if (=) a a0 then string_dec s1 s2 else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val length : char list -> int **)

let rec length = function
| [] -> 0
| c::s' -> Pervasives.succ (length s')

(** val get : int -> char list -> char option **)

let rec get n = function
| [] -> None
| c::s' ->
  ((fun fO fS n -> if n=0 then fO () else fS (n-1))
     (fun _ -> Some
     c)
     (fun n' ->
     get n' s')
     n)

(** val substring : int -> int -> char list -> char list **)

let rec substring n m s =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      [])
      (fun m' ->
      match s with
      | [] -> s
      | c::s' -> c::(substring 0 m' s'))
      m)
    (fun n' ->
    match s with
    | [] -> s
    | c::s' -> substring n' m s')
    n

(** val prefix : char list -> char list -> bool **)

let rec prefix s1 s2 =
  match s1 with
  | [] -> true
  | a::s1' ->
    (match s2 with
     | [] -> false
     | b::s2' -> if (=) a b then prefix s1' s2' else false)

(** val index : int -> char list -> char list -> int option **)

let rec index n s1 s2 = match s2 with
| [] ->
  ((fun fO fS n -> if n=0 then fO () else fS (n-1))
     (fun _ ->
     match s1 with
     | [] -> Some 0
     | a::s1' -> None)
     (fun n' ->
     None)
     n)
| b::s2' ->
  ((fun fO fS n -> if n=0 then fO () else fS (n-1))
     (fun _ ->
     if prefix s1 s2
     then Some 0
     else (match index 0 s1 s2' with
           | Some n0 -> Some (Pervasives.succ n0)
           | None -> None))
     (fun n' ->
     match index n' s1 s2' with
     | Some n0 -> Some (Pervasives.succ n0)
     | None -> None)
     n)

(** val findex : int -> char list -> char list -> int **)

let findex n s1 s2 =
  match index n s1 s2 with
  | Some n0 -> n0
  | None -> 0

(** val coq_HelloWorld : char list **)

let coq_HelloWorld =
  '\t'::('"'::('H'::('e'::('l'::('l'::('o'::(' '::('w'::('o'::('r'::('l'::('d'::('!'::('"'::('\007'::('\n'::[]))))))))))))))))

