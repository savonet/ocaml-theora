(* Shamelessly inspired of http://theora.org/doc/libtheora-1.0beta1/ *)

open Theora

let infile = ref "input.ogg"
let outfile = ref "output.ogg"
let debug = ref false

let quality = ref 5

let () =
  Arg.parse
    [
      "-d", Arg.Set debug, "Show debugging messages";
      "-o", Arg.Set_string outfile, "Output file";
      "-q", Arg.Set_int quality, "Quality of the compression";
      "-i", Arg.Set_string infile, "Input file";
    ]
    ignore
    "thranscode [options]"

let eos = ref false

let in_init () =
  let sync,fd = Ogg.Sync.create_from_file !infile in
  let rec fill os =
    let page = Ogg.Sync.read sync in
    try
      Ogg.Stream.put_page os page ;
      if Ogg.Page.eos page then eos := true
    with
       | Ogg.Bad_data -> fill os (* Do not care about page that are not for us.. *)
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
  t,os,fill,info,fd

let out_init info =
  let oc = open_out !outfile in
  let out s = output_string oc s; flush oc in
  let os = Ogg.Stream.create () in
  let t = Encoder.create info in
    Encoder.encode_header t os;
    out (Ogg.Stream.pageout os);
    let comment = ["artitst", "test artist"; "title", "test title"] in
    Encoder.encode_comments os comment;
    Encoder.encode_tables t os;
    out (Ogg.Stream.flush os);
    t,os,out

let () = 
  let dec,is,fill,info,fd = in_init () in
  let info = 
  {
    width = info.width;
    height = info.height;
    frame_width = info.frame_width;
    frame_height = info.frame_height;
    offset_x = info.offset_x;
    offset_y = info.offset_y;
    fps_numerator = info.fps_numerator;
    fps_denominator = info.fps_denominator;
    aspect_numerator = info.aspect_numerator;
    aspect_denominator = info.aspect_denominator;
    colorspace = info.colorspace;
    target_bitrate = info.target_bitrate;
    quality = !quality;
    quick_p = true;
    version_major = 0;
    version_minor = 0;
    version_subminor = 0;
    dropframes_p = false;
    keyframe_auto_p = true;
    keyframe_frequency = 64;
    keyframe_frequency_force = 64;
    keyframe_data_target_bitrate = (info.target_bitrate * 3 / 2);
    keyframe_auto_threshold = 80;
    keyframe_mindistance = 8;
    noise_sensitivity = 1;
    sharpness = 1; (* ??? *)
    pixelformat = PF_420
  }
  in
  let enc,os,out = out_init info in
  let rec generator () =
    try
      Decoder.get_yuv dec is 
    with 
      | Ogg.Not_enough_data -> (fill is; generator ()) 
  in
  while not !eos do
    let op = Encoder.encode_page enc os generator in
    let s_o_p (h,b) = h ^ b in
    let op = s_o_p op in
      out op
  done;
  Unix.close fd
