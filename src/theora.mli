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

(** {2 Exceptions} *)

(** General failure. *)
exception Internal_error
(** Library encountered invalid internal data. *)
exception Invalid_data
(** An unhandled error happened. *)
exception Unknown_error of int

(** {2 General functions} *)

(**
  * Retrieve a human-readable string to identify the encoder vendor and version.
  *)
val version_string : unit -> string

(** Retrive the major, minor and sub version numbers of the encoder. *)
val version_number : unit -> int * int * int

(** {2 Types and datastructures} *)

(** A Colorspace. *)
type colorspace =
  | CS_unspecified (** The colorspace is unknown or unspecified *)
  | CS_ITU_REC_470M (** This is the best option for 'NTSC' content *)
  | CS_ITU_REC_470BG (** This is the best option for 'PAL' content *)
  | CS_NSPACES (** This marks the end of the defined colorspaces *)

(**
  * A Chroma subsampling
  *
  * These enumerate the available chroma subsampling options supported
  * by the theora format. See Section 4.4 of the specification for
  * exact definitions.
  *)
type pixelformat =
  | PF_420 (** Chroma subsampling by 2 in each direction (4:2:0) *)
  | PF_reserved (** Reserved value *)
  | PF_422 (** Horizonatal chroma subsampling by 2 (4:2:2) *)
  | PF_444 (** No chroma subsampling at all (4:4:4) *)

(**
  * Theora bitstream info.
  * Contains the basic playback parameters for a stream,
  * corresponds to the initial 'info' header packet.
  *
  * Encoded theora frames must be a multiple of 16 is size;
  * this is what the width and height members represent. To
  * handle other sizes, a crop rectangle is specified in
  * [frame_height] and [frame_width], [offset_x] and [offset_y]. The
  * offset and size should still be a multiple of 2 to avoid
  * chroma sampling shifts. Offset values in this structure
  * are measured from the upper left of the image.
  *
  * Frame rate, in frames per second, is stored as a rational
  * fraction. So is the aspect ratio. Note that this refers
  * to the aspect ratio of the frame pixels, not of the
  * overall frame itself.
  *
  * See the example code for use of the other parameters and
  * good default settings for the encoder parameters.
  *
  * This type is private since it needs private theora parameters.
  * Une the [new_info] function to create an empty one.*)
type info = 
    {
      width : int; (** encoded frame width (should be divisible by 16) *)
      height : int; (** encoded frame height (should be divisible by 16) *)
      frame_width : int; (** display frame width *)
      frame_height : int; (** display frame height *)
      offset_x : int; (** horizontal offset of the displayed frame *)
      offset_y : int; (** vertical offset of the displayed frame *)
      fps_numerator : int; (** frame rate numerator *)
      fps_denominator : int; (** frame rate denominator *)
      aspect_numerator : int; (** pixel aspect ratio numerator *)
      aspect_denominator : int; (** pixel aspect ratio denominator *)
      colorspace : colorspace; (** colorspace *)
      target_bitrate : int; (** nominal bitrate in bits per second (between 45kbps and 2000kbps) *)
      quality : int; (** Nominal quality setting, (between 0 and 63) *)
      quick_p : bool; (** Quick encode/decode *)

      (** Decode only *)
      version_major : int;
      version_minor : int;
      version_subminor : int;

      (** Encode only *)
      dropframes_p : bool;
      keyframe_auto_p : bool;
      keyframe_frequency : int;
      keyframe_frequency_force : int; (** also used for decode init to get granpos shift correct *)
      keyframe_data_target_bitrate : int;
      keyframe_auto_threshold : int;
      keyframe_mindistance : int;
      noise_sensitivity : int;
      sharpness : int;
      pixelformat : pixelformat; (** chroma subsampling mode to expect *)
    }

(**
  * A YUV buffer for passing uncompressed frames to and from the codec.
  * This holds a Y'CbCr frame in planar format. The CbCr planes can be
  * subsampled and have their own separate dimensions and row stride
  * offsets. Note that the strides may be negative in some
  * configurations. For theora the width and height of the largest plane
  * must be a multiple of 16. The actual meaningful picture size and
  * offset are stored in the [info] structure; frames returned by
  * the decoder may need to be cropped for display.
  *
  * All samples are 8 bits. Within each plane samples are ordered by
  * row from the top of the frame to the bottom. Within each row samples
  * are ordered from left to right.
  *)
type yuv_buffer =
    {
      y_width : int; (** Width of the Y' luminance plane *)
      y_height : int; (** Height of the luminance plane *)
      uv_width : int; (** Width of the Cb and Cr chroma planes *)
      uv_height : int; (** Height of the chroma planes *)
      y : string; (** luminance data *)
      u : string; (** Cb data *)
      v : string; (** Cr data *)
    }

(** {2 Encoding} *)


module Encoder :
sig
  type t

  (** Initialize a [state] handle for decoding. *)
  val create : info -> t

  (**
    * Request a packet containing the initial header.
    * The header data is placed in an [Ogg.packet] value.
    *)
  val encode_header : t -> Ogg.Stream.t -> unit

  (**
    * Encode a comment header packet from provided vendor name and metadata (i.e.
    * a list of (tag, value) couples).
    *)
  val encode_comments : Ogg.Stream.t -> (string*string) list -> unit

  (**
    * Request a packet containing the codebook tables for the stream.
    * The codebook data is placed in an [Ogg.packet] value.
    *)
  val encode_tables : t -> Ogg.Stream.t -> unit

  (**
    * Encode data until a page is filled.
    *)
  val encode_page : t -> Ogg.Stream.t -> (unit -> yuv_buffer) -> Ogg.Page.t

  (** Encode a buffer. *)
  val encode_buffer : t -> Ogg.Stream.t -> yuv_buffer -> unit
end

module Decoder :
sig
  type t

  (**
    * Check wether an ogg logical stream contains theora data
    *
    * This function shall be called just after you put 
    * the first page in the stream. See examples/thdecode.ml 
    *
    * Raises [Ogg.Bad_data] if the stream does not contain theora data. *)
  val check : Ogg.Stream.packet -> bool

  (** Initialize the decoding structure. Needs the first
    * 3 packets of the logical stream. *)
  val create : Ogg.Stream.packet -> Ogg.Stream.packet -> Ogg.Stream.packet 
                  -> t*info*string*(string*string) list

 (**
   * Output the next available frame of decoded YUV data. 
   *
   * Raises [Ogg.Not_enough_data] if the Ogg.Stream.t which
   * has been used to initialize the handler does not contain
   * enought data. You should submit a new page to it, and 
   * run this function again until it returns. *)
  val get_yuv : t -> Ogg.Stream.t -> yuv_buffer
end

