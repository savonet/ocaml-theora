(* Shamelessly inspired of examples/encoder_example.c in libtheora sources. *)

open Theora

let outfile = ref "output.ogg"
let debug = ref false

let frame_x = 640
let frame_y = 480
(* Theora has a divisible-by-sixteen restriction for the encoded video size. *)
(* Scale the frame size up to the nearest /16 and calculate offsets. *)
let video_x = ((frame_x + 15) lsr 4) lsl 4
let video_y = ((frame_y + 15) lsr 4) lsl 4
(* We force the offset to be even. This ensures that the chroma samples align
 * properly with the luma samples. *)
let frame_x_offset = ((video_x - frame_x) / 2) land (lnot 1)
let frame_y_offset = ((video_y - frame_y) / 2) land (lnot 1)
let video_r = 100000
let quality = ref 5

let () =
  Arg.parse
    [
      "-d", Arg.Set debug, "Show debugging messages";
      "-o", Arg.Set_string outfile, "Output file";
      "-q", Arg.Set_int quality, "Quality of the compression";
    ]
    ignore
    "thencode [options]"

let info = 
{
    width = video_x;
    height = video_y;
    frame_width = frame_x;
    frame_height = frame_y;
    offset_x = frame_x_offset;
    offset_y = frame_y_offset;
    fps_numerator = 24;
    fps_denominator = 1;
    aspect_numerator = 1;
    aspect_denominator = 1;
    colorspace = CS_unspecified;
    target_bitrate = video_r;
    quality = !quality;
    quick_p = true;
    version_major = 0;
    version_minor = 0;
    version_subminor = 0;
    dropframes_p = false;
    keyframe_auto_p = true;
    keyframe_frequency = 64;
    keyframe_frequency_force = 64;
    keyframe_data_target_bitrate = (video_r * 3 / 2);
    keyframe_auto_threshold = 80;
    keyframe_mindistance = 8;
    noise_sensitivity = 1;
    sharpness = 1; (* ??? *)
    pixelformat = PF_420
}

let () = Random.self_init ()

let random_string n =
  let s = String.create n in
    for i = 0 to n - 1 do
      s.[i] <- char_of_int (Random.int 256)
    done;
    s

let generator () =
  {
    y_width = frame_x;
    y_height = frame_y;
    uv_width = frame_x / 2;
    uv_height = frame_y / 2;
    y = random_string (frame_x * frame_y);
    u = random_string (frame_x * frame_y / 4);
    v = random_string (frame_x * frame_y / 4);
  }

let () =
  let oc = open_out !outfile in
  let out = output_string oc in
  let os = Ogg.Stream.create () in
  let t = Encoder.create info in
    Encoder.encode_header t os;
    out (Ogg.Stream.pageout os);
    let comment = ["artitst", "test artist"; "title", "test title"] in
    Encoder.encode_comments os comment;
    Encoder.encode_tables t os;
    out (Ogg.Stream.flush os);
    while true do
      let op = Encoder.encode_page t os generator in
      let s_o_p (h,b) = h ^ b in
      let op = s_o_p op in
        out op;
        if !debug then Printf.eprintf "One more page (%d bytes)!\n%!" (String.length op)
    done;
    close_out oc
