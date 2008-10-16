(* Shamelessly inspired of http://theora.org/doc/libtheora-1.0beta1/ *)

open Theora

let infile = ref "input.ogg"
let outfile = ref "output.raw"
let debug = ref false

let () =
  Arg.parse
    [
      "-d", Arg.Set debug, "Show debugging messages";
      "-i", Arg.Set_string infile, "Input file";
      "-o", Arg.Set_string outfile, "Output file";
    ]
    ignore
    "thdecode [options]"

let () =
  let sync,fd = Ogg.Sync.create_from_file !infile in
  let eos = ref false in
  let rec fill os = 
    let page = Ogg.Sync.read sync in
    try
      Ogg.Stream.put_page os page ;
      if Ogg.Page.eos page then eos := true
    with
       | Ogg.Bad_data -> fill os (*Do not care about page that are not for us.. *)
  in
  (** Test wether the stream is theora *)
  let test_theora () = 
    (** Get First page *)
    let page = Ogg.Sync.read sync in
    (** Check wether this is a b_o_s *)
    if not (Ogg.Page.bos page) then raise Not_found ;
    (** Create a stream with this ID *)
    let serial = Ogg.Page.serialno page in
    Printf.printf "Testing stream %nx\n" serial ;
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page ;
    let packet = Ogg.Stream.get_packet os in
    (** Test header. Do not catch anything, first page should be sufficient *)
    if not (Decoder.check packet) then
      raise Not_found;
    Printf.printf "Got a theora stream !\n" ;
    (** Decode headers *)
    fill os;
    let packet2 = Ogg.Stream.get_packet os in
    fill os;
    let packet3 = Ogg.Stream.get_packet os in
    serial,os,Decoder.create packet packet2 packet3
  in
  (** Now find a theora stream *)
  let rec init () = 
    try 
      test_theora ()
    with
    (** Not_found is not catched: ogg stream always start
        with all b_o_s and we don't care about sequenced streams here *)
      | Ogg.Bad_data -> 
         ( Printf.printf "This stream was not theora..\n"; flush_all ();
           init () )
  in
  let serial,os,(t,info,vendor,comments) = init () in
  Printf.printf 
     "Ogg logical stream %nx is Theora %dx%d %.02f fps video\n"
     serial info.width info.height
     ((float_of_int info.fps_numerator) /. (float_of_int info.fps_denominator)) ;
  Printf.printf "Encoded frame content is %dx%d with %dx%d offset\n"
     info.frame_width info.frame_height info.offset_x 
     info.offset_y ;
  Printf.printf "YUV4MPEG2 W%d H%d F%d:%d I%c A%d:%d\n" 
     info.width info.height info.fps_numerator
     info.fps_denominator 'p' 
     info.aspect_numerator info.aspect_denominator ;
  Printf.printf "Vendor: %s\n" vendor ;
  List.iter (fun (x,y) -> Printf.printf "%s: %s\n" x y) comments ;
  flush_all ();
  let oc = open_out !outfile in
  let out = output_string oc in
  let f () = 
    try
      let buffer = Decoder.get_yuv t os in
      out buffer.y;
      out buffer.u;
      out buffer.v;
      if !debug then (Printf.eprintf "One more buffer !\n")
    with
      | Ogg.Not_enough_data -> fill os
  in
  while not !eos do
    f ()
  done;
  Unix.close fd
