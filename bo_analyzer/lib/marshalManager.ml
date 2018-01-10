
let output : string -> 'a -> unit
= fun name data ->
  let chan = open_out (!Options.opt_marshal_dir ^ "/" ^ name) in
  Marshal.to_channel chan data [];
  close_out chan

let input : string -> 'a
= fun name -> 
  let chan = open_in (!Options.opt_marshal_dir ^ "/" ^ name) in
  let data = Marshal.from_channel chan in
  close_in chan;
  data
