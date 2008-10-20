(*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-theora.
 *
 * ocaml-theora is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-theora is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-theora; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Functions for encoding theora files using libtheora.
  *
  * @author Samuel Mimram
  *)

exception Internal_error
exception Invalid_data
exception Unknown_error of int

let () =
  Callback.register_exception "theora_exn_fault" Internal_error;
  Callback.register_exception "theora_exn_inval" Invalid_data;
  Callback.register_exception "theora_exn_unknown" (Unknown_error 0)

external version_string : unit -> string = "ocaml_theora_version_string"

external version_number : unit -> int = "ocaml_theora_version_number"

let version_number () =
  let n = version_number () in
    n lsr 16,
    (n lsr 8) land 0xff,
    n land 0xff

type colorspace =
  | CS_unspecified
  | CS_ITU_REC_470M
  | CS_ITU_REC_470BG
  | CS_NSPACES

type pixelformat =
  | PF_420
  | PF_reserved
  | PF_422
  | PF_444

type info = 
    {
      width : int;
      height : int;
      frame_width : int;
      frame_height : int;
      offset_x : int;
      offset_y : int;
      fps_numerator : int;
      fps_denominator : int;
      aspect_numerator : int;
      aspect_denominator : int;
      colorspace : colorspace;
      target_bitrate : int;
      quality : int;
      quick_p : bool;

      (* Decode only *)
      version_major : int;
      version_minor : int;
      version_subminor : int;

      (* Encode only *)
      dropframes_p : bool;
      keyframe_auto_p : bool;
      keyframe_frequency : int;
      keyframe_frequency_force : int;
      keyframe_data_target_bitrate : int;
      keyframe_auto_threshold : int;
      keyframe_mindistance : int;
      noise_sensitivity : int;
      sharpness : int;
      pixelformat : pixelformat;
    }

type data_buffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type yuv_buffer =
    {
      y_width : int;
      y_height : int;
      y_stride : int;
      uv_width : int;
      uv_height : int;
      uv_stride : int;
      y : data_buffer;
      u : data_buffer;
      v : data_buffer;
    }

type comment

external create_comment : (string*string) array -> comment = "ocaml_theora_comment_create"

let encoder_tag = "ocaml-theora by the savonet team (http://savonet.sf.net/)"

module Encoder =
struct
  type t

  external create : info -> t = "ocaml_theora_encode_init"

  external encode_header : t -> Ogg.Stream.t -> unit = "ocaml_theora_encode_header"

  external encode_comments : Ogg.Stream.t -> (string*string) array -> unit = "ocaml_theora_encode_comments"

   let encode_comments stream comments =
     let comments = ("ENCODER", encoder_tag)::comments in
       encode_comments stream (Array.of_list comments)

  external encode_tables : t -> Ogg.Stream.t -> unit = "ocaml_theora_encode_tables"

  (* TODO: encode page could be done in caml using encode_buffer. *)
  external encode_page : t -> Ogg.Stream.t -> (unit -> yuv_buffer) -> Ogg.Page.t = "ocaml_theora_encode_page"

  external encode_buffer : t -> Ogg.Stream.t -> yuv_buffer -> unit = "ocaml_theora_encode_buffer"
end

module Decoder =
struct
  type t

  external check : Ogg.Stream.packet -> bool = "caml_theora_check"

  external create : Ogg.Stream.packet -> Ogg.Stream.packet -> Ogg.Stream.packet -> t*info*(string array) = "ocaml_theora_create"

  let create p1 p2 p3 =
    let t,info,comment = create p1 p2 p3 in
    let vendor,comment =
      match Array.to_list comment with
        | e :: l -> e,l
        | [] -> "",[]
    in
    let split s =
      try
        let pos = String.index s '=' in
        String.sub s 0 pos,String.sub s (pos+1) ((String.length s) - pos - 1)
      with
        | Not_found -> "",s
    in
    t,info,vendor,(List.map split comment)

  external get_yuv : t -> Ogg.Stream.t -> yuv_buffer = "ocaml_theora_decode_YUVout"
end
