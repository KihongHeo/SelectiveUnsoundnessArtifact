open Arg

let opt_il = ref false
let opt_inline = ref ["alloc"; "strdup"]
let opt_narrow = ref false
let opt_densify = ref false
let opt_validate_bool = ref false
let opt_validate () = opt_densify := true
                    ; opt_validate_bool := true
let opt_sound = ref false
let opt_unsound = ref false
let opt_marshal_bool = ref false
let opt_marshal_filename = ref ""
let opt_marshal output = opt_marshal_bool := true
                       ; opt_marshal_filename := output
                       ; opt_densify := true
let opt_solver = ref "~/project/open-wbo/open-wbo_static"
let opt_encoding_mode = ref 3
let opt_alarm = ref ""
let opt_debug = ref false
let opt_nobar = ref false
let opt_pfs = ref 0
let opt_rank_random =ref false
let opt_optil = ref false

let opt_unsound_lib = ref BatSet.empty

let opt_print_all_query = ref true

type wv =
  { w1 : int; w2 : int; w3 : int; w4 : int; w5 : int
  ; w6 : int; w7 : int; w8 : int; w9 : int; w10 : int
  ; w11 : int; w12 : int; w13 : int; w14 : int; w15 : int
  ; w16 : int; w17 : int; w18 : int; w19 : int; w20 : int
  ; w21 : int; w22 : int; w23 : int; w24 : int; w25 : int
  ; w26 : int; w27 : int; w28 : int; w29 : int; w30 : int
  ; w31 : int; w32 : int; w33 : int; w34 : int; w35 : int
  ; w36 : int; w37 : int; w38 : int; w39 : int; w40 : int
  ; w41 : int; w42 : int; w43 : int; w44 : int; w45 : int
  ; w46 : int; w47 : int; w48 : int; w49 : int; w50 : int
  ; w51 : int; w52 : int; w53 : int; w54 : int; w55 : int
  ; w56 : int; w57 : int; w58 : int; w59 : int; w60 : int }

let wv =
  ref { w1 = 5; w2 = 10; w3 = 1; w4 = 0; w5 = 5
      ; w6 = 5; w7 = 1; w8 = 1; w9 = 5; w10 = 10
      ; w11 = 1; w12 = 1; w13 = 5; w14 = 5; w15 = 0
      ; w16 = 1; w17 = 5; w18 = 1; w19 = 5; w20 = 1
      ; w21 = 10; w22 = 1; w23 = 5; w24 = 1; w25 = 5
      ; w26 = 1; w27 = 1; w28 = 1; w29 = 10; w30 = 0
      ; w31 = 1; w32 = 5; w33 = 5; w34 = 1; w35 = 10
      ; w36 = 1; w37 = 5; w38 = 1; w39 = 10; w40 = 0
      ; w41 = 1; w42 = 1; w43 = 5; w44 = 5; w45 = 5
      ; w46 = 5; w47 = 1; w48 = 1; w49 = 1; w50 = 1
      ; w51 = 1; w52 = 1; w53 = 5; w54 = 5; w55 = 5
      ; w56 = 5; w57 = 1; w58 = 1; w59 = 1; w60 = 1 }

let set_w1 i = wv := {!wv with w1 = i}
let set_w2 i = wv := {!wv with w2 = i}
let set_w3 i = wv := {!wv with w3 = i}
let set_w4 i = wv := {!wv with w4 = i}
let set_w5 i = wv := {!wv with w5 = i}
let set_w6 i = wv := {!wv with w6 = i}
let set_w7 i = wv := {!wv with w7 = i}
let set_w8 i = wv := {!wv with w8 = i}
let set_w9 i = wv := {!wv with w9 = i}
let set_w10 i = wv := {!wv with w10 = i}
let set_w11 i = wv := {!wv with w11 = i}
let set_w12 i = wv := {!wv with w12 = i}
let set_w13 i = wv := {!wv with w13 = i}
let set_w14 i = wv := {!wv with w14 = i}
let set_w15 i = wv := {!wv with w15 = i}
let set_w16 i = wv := {!wv with w16 = i}
let set_w17 i = wv := {!wv with w17 = i}
let set_w18 i = wv := {!wv with w18 = i}
let set_w19 i = wv := {!wv with w19 = i}
let set_w20 i = wv := {!wv with w20 = i}
let set_w21 i = wv := {!wv with w21 = i}
let set_w22 i = wv := {!wv with w22 = i}
let set_w23 i = wv := {!wv with w23 = i}
let set_w24 i = wv := {!wv with w24 = i}
let set_w25 i = wv := {!wv with w25 = i}
let set_w26 i = wv := {!wv with w26 = i}
let set_w27 i = wv := {!wv with w27 = i}
let set_w28 i = wv := {!wv with w28 = i}
let set_w29 i = wv := {!wv with w29 = i}
let set_w30 i = wv := {!wv with w30 = i}
let set_w31 i = wv := {!wv with w31 = i}
let set_w32 i = wv := {!wv with w32 = i}
let set_w33 i = wv := {!wv with w33 = i}
let set_w34 i = wv := {!wv with w34 = i}
let set_w35 i = wv := {!wv with w35 = i}
let set_w36 i = wv := {!wv with w36 = i}
let set_w37 i = wv := {!wv with w37 = i}
let set_w38 i = wv := {!wv with w38 = i}
let set_w39 i = wv := {!wv with w39 = i}
let set_w40 i = wv := {!wv with w40 = i}
let set_w41 i = wv := {!wv with w41 = i}
let set_w42 i = wv := {!wv with w42 = i}
let set_w43 i = wv := {!wv with w43 = i}
let set_w44 i = wv := {!wv with w44 = i}
let set_w45 i = wv := {!wv with w45 = i}
let set_w46 i = wv := {!wv with w46 = i}
let set_w47 i = wv := {!wv with w47 = i}
let set_w48 i = wv := {!wv with w48 = i}
let set_w49 i = wv := {!wv with w49 = i}
let set_w50 i = wv := {!wv with w50 = i}
let set_w51 i = wv := {!wv with w51 = i}
let set_w52 i = wv := {!wv with w52 = i}
let set_w53 i = wv := {!wv with w53 = i}
let set_w54 i = wv := {!wv with w54 = i}
let set_w55 i = wv := {!wv with w55 = i}
let set_w56 i = wv := {!wv with w56 = i}
let set_w57 i = wv := {!wv with w57 = i}
let set_w58 i = wv := {!wv with w58 = i}
let set_w59 i = wv := {!wv with w59 = i}
let set_w60 i = wv := {!wv with w60 = i}

let opts =
  [ ("-il", (Arg.Set opt_il), "Show the input program in IL")
  ; ("-sound", (Arg.Set opt_sound), "fully sound on lib calls")
  ; ("-unsound", (Arg.Set opt_unsound), "fully unsound on lib calls")
  ; ("-inline", (Arg.String (fun s -> opt_inline := s::(!opt_inline))), "Inline *alloc* functions")
  ; ("-narrow", (Arg.Set opt_narrow), "Do narrowing")
  ; ("-validate", (Arg.Unit opt_validate), "Do validation")
  ; ("-debug", (Arg.Set opt_debug), "Debug mode")
  ; ("-nobar", (Arg.Set opt_nobar), "No progress bar")
  ; ("-pfs", (Arg.Set_int opt_pfs), "Apply flow-sensitivity partially. -pfs [0-100] (0: flow-insensitivity, 100: full flow-sensitivity). default = 10 ")
  ; ("-rank_random", (Arg.Set opt_rank_random), "Random variable ranking")
  ; ("-optil", (Arg.Set opt_optil), "Optimize IL")
  ; ("-all_query", (Arg.Set opt_print_all_query), "Print all queries")
  ; ("-marshal", (Arg.String opt_marshal), "Do marshalling for dug e.g.) -marshal [target_filename]")
  ; ("-solver", (Arg.Set_string opt_solver), "Set path for maxsat solver e.g.) -solver [target_filename] (default:~/project/open-wbo/open-wbo_static)")
  ; ("-encoding_mode", (Arg.Set_int opt_encoding_mode), "Choose the encoding mode -encoding_mode [1-3] (1: Naive mode, 2: Initial scc mode, 3: Scc with friend (recommand)). default = 3 ")
  ; ("-alarms", (Arg.Set_string opt_alarm), "Set marshaling alarm number")
  ; ("-unsound_lib", (Arg.String (fun s -> opt_unsound_lib := BatSet.add s (!opt_unsound_lib))), "Apply unsoundness on lib calls")
  ; ("-wv",
     (Arg.Tuple 
        [ Int set_w1; Int set_w2; Int set_w3; Int set_w4; Int set_w5
        ; Int set_w6; Int set_w7; Int set_w8; Int set_w9; Int set_w10
        ; Int set_w11; Int set_w12; Int set_w13; Int set_w14; Int set_w15
        ; Int set_w16; Int set_w17; Int set_w18; Int set_w19; Int set_w20
        ; Int set_w21; Int set_w22; Int set_w23; Int set_w24; Int set_w25
        ; Int set_w26; Int set_w27; Int set_w28; Int set_w29; Int set_w30
        ; Int set_w31; Int set_w32; Int set_w33; Int set_w34; Int set_w35
        ; Int set_w36; Int set_w37; Int set_w38; Int set_w39; Int set_w40
        ; Int set_w41; Int set_w42; Int set_w43; Int set_w44; Int set_w45
        ; Int set_w46; Int set_w47; Int set_w48; Int set_w49; Int set_w50
        ; Int set_w51; Int set_w52; Int set_w53; Int set_w54; Int set_w55
        ; Int set_w56; Int set_w57; Int set_w58; Int set_w59; Int set_w60 ]),
     "Weight vector for ranking flow-sensitive locations")
  ]
