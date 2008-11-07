/*
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
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <string.h>
#include <assert.h>
#include <stdio.h>

#include <theora/theora.h>

#include <ocaml-ogg.h>

/***** Error handling ******/

static void check_err(int n)
{
  switch(n)
  {
    case 0:
      return;

    case OC_FAULT:
      caml_raise_constant(*caml_named_value("theora_exn_fault"));

    case OC_EINVAL:
    case OC_VERSION:
    case OC_NEWPACKET:
    case OC_BADPACKET:
    case OC_NOTFORMAT:
    case OC_BADHEADER:
      caml_raise_constant(*caml_named_value("theora_exn_inval"));

    default:
      caml_raise_with_arg(*caml_named_value("theora_exn_unknown"), Val_int(n));
  }
}

/***** Version *****/

CAMLprim value ocaml_theora_version_string(value unit)
{
  return caml_copy_string(theora_version_string());
}

CAMLprim value ocaml_theora_version_number(value unit)
{
  return Val_int(theora_version_number());
}

/***** Helper functions *****/

static theora_colorspace cs_of_val(value v)
{
  switch(Int_val(v))
  {
    case 0:
      return OC_CS_UNSPECIFIED;

    case 1:
      return OC_CS_ITU_REC_470M;

    case 2:
      return OC_CS_ITU_REC_470BG;

    case 3:
      return OC_CS_NSPACES;

    default:
      assert(0);
  }
}

static theora_pixelformat pf_of_val(value v)
{
  switch(Int_val(v))
  {
    case 0:
      return OC_PF_420;

    case 1:
      return OC_PF_RSVD;

    case 2:
      return OC_PF_422;

    case 3:
      return OC_PF_444;

    default:
      assert(0);
  }
}

/* ti is *not* allocated: codec_setup may be allocated by 
 * theora_info_init and its memory lost. You better check what you need */
static theora_info *info_of_val(value v, theora_info *ti)
{
  int i = 0;

  ti->width = Int_val(Field(v, i++));
  ti->height = Int_val(Field(v, i++));
  ti->frame_width = Int_val(Field(v, i++));
  ti->frame_height = Int_val(Field(v, i++));
  ti->offset_x = Int_val(Field(v, i++));
  ti->offset_y = Int_val(Field(v, i++));
  ti->fps_numerator = Int_val(Field(v, i++));
  ti->fps_denominator = Int_val(Field(v, i++));
  ti->aspect_numerator = Int_val(Field(v, i++));
  ti->aspect_denominator = Int_val(Field(v, i++));
  ti->colorspace = cs_of_val(Field(v, i++));
  ti->target_bitrate = Int_val(Field(v, i++));
  ti->quality = Int_val(Field(v, i++));
  ti->quick_p = Bool_val(Field(v, i++));
  ti->version_major = Int_val(Field(v, i++));
  ti->version_minor = Int_val(Field(v, i++));
  ti->version_subminor = Int_val(Field(v, i++));
  ti->dropframes_p = Bool_val(Field(v, i++));
  ti->keyframe_auto_p = Bool_val(Field(v, i++));
  ti->keyframe_frequency = Int_val(Field(v, i++));
  ti->keyframe_frequency_force = Int_val(Field(v, i++));
  ti->keyframe_data_target_bitrate = Int_val(Field(v, i++));
  ti->keyframe_auto_threshold = Int_val(Field(v, i++));
  ti->keyframe_mindistance = Int_val(Field(v, i++));
  ti->noise_sensitivity = Int_val(Field(v, i++));
  ti->sharpness = Int_val(Field(v, i++));
  ti->pixelformat = pf_of_val(Field(v, i++));

  return ti;
}


static value val_of_info(theora_info *ti)
{
  CAMLparam0();
  CAMLlocal1 (v);
  int i = 0;
  v = caml_alloc_tuple(27);
  Store_field (v, i++, Val_int(ti->width));
  Store_field (v, i++, Val_int(ti->height));
  Store_field (v, i++, Val_int(ti->frame_width));
  Store_field (v, i++, Val_int(ti->frame_height));
  Store_field (v, i++, Val_int(ti->offset_x));
  Store_field (v, i++, Val_int(ti->offset_y));
  Store_field (v, i++, Val_int(ti->fps_numerator));
  Store_field (v, i++, Val_int(ti->fps_denominator));
  Store_field (v, i++, Val_int(ti->aspect_numerator));
  Store_field (v, i++, Val_int(ti->aspect_denominator));
  Store_field (v, i++, Val_int(ti->colorspace));
  Store_field (v, i++, Val_int(ti->target_bitrate));
  Store_field (v, i++, Val_int(ti->quality));
  Store_field (v, i++, Val_bool(ti->quick_p));
  Store_field (v, i++, Val_int(ti->version_major));
  Store_field (v, i++, Val_int(ti->version_minor));
  Store_field (v, i++, Val_int(ti->version_subminor));
  Store_field (v, i++, Val_bool(ti->dropframes_p));
  Store_field (v, i++, Val_bool(ti->keyframe_auto_p));
  Store_field (v, i++, Val_int(ti->keyframe_frequency));
  Store_field (v, i++, Val_int(ti->keyframe_frequency_force));
  Store_field (v, i++, Val_int(ti->keyframe_data_target_bitrate));
  Store_field (v, i++, Val_int(ti->keyframe_auto_threshold));
  Store_field (v, i++, Val_int(ti->keyframe_mindistance));
  Store_field (v, i++, Val_int(ti->noise_sensitivity));
  Store_field (v, i++, Val_int(ti->sharpness));
  Store_field (v, i++, Val_int(ti->pixelformat));

  CAMLreturn(v);
}

static yuv_buffer *yuv_of_val(value v, yuv_buffer *yb)
{
    int i = 0;
    struct caml_ba_array *ba;
    yb->y_width = Int_val(Field(v, i++));
    yb->y_height = Int_val(Field(v, i++));
    yb->y_stride = Int_val(Field(v, i++));
    yb->uv_width = Int_val(Field(v, i++));
    yb->uv_height = Int_val(Field(v, i++));
    yb->uv_stride = Int_val(Field(v, i++));

    ba = Caml_ba_array_val(Field(v, i++));
    if (ba->dim[0] != yb->y_stride*yb->y_height)
      caml_raise_constant(*caml_named_value("theora_exn_inval"));    
    yb->y = (unsigned char *)ba->data;

    ba = Caml_ba_array_val(Field(v, i++));
    if (ba->dim[0] != yb->uv_stride*yb->uv_height)
      caml_raise_constant(*caml_named_value("theora_exn_inval"));
    yb->u = (unsigned char *)ba->data;

    ba = Caml_ba_array_val(Field(v, i++));
    if (ba->dim[0] != yb->uv_stride*yb->uv_height)
      caml_raise_constant(*caml_named_value("theora_exn_inval"));
    yb->v = (unsigned char *)ba->data;

    return yb;
}

/* The result must be freed afterwards! */
/* This should not be called in a blocking section. */
static value val_of_yuv(yuv_buffer *yb)
{
  CAMLparam0();
  CAMLlocal4 (ret,y,u,v);
  int i = 0;
  intnat len;
  ret = caml_alloc_tuple(9);
  unsigned char *data;

  Store_field (ret, i++, Val_int(yb->y_width));
  Store_field (ret, i++, Val_int(yb->y_height));
  Store_field (ret, i++, Val_int(yb->y_stride));
  Store_field (ret, i++, Val_int(yb->uv_width));
  Store_field (ret, i++, Val_int(yb->uv_height));
  Store_field (ret, i++, Val_int(yb->uv_stride));

  len = yb->y_stride*yb->y_height;
  data = malloc(len);
  if (data == NULL)
    caml_failwith("malloc");
  y = caml_ba_alloc(CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, data, &len);
  memcpy(data,yb->y,len);
  Store_field (ret, i++, y);

  len = yb->uv_stride*yb->uv_height;
  data = malloc(len);
  if (data == NULL)
    caml_failwith("malloc");
  u = caml_ba_alloc(CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, data, &len);
  memcpy(data,yb->u,len);
  Store_field (ret, i++, u);

  data = malloc(len);
  if (data == NULL)
    caml_failwith("malloc");
  v = caml_ba_alloc(CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, data, &len);
  memcpy(data,yb->v,len);
  Store_field (ret, i++, v);

  CAMLreturn(ret);
}

/***** State *****/

typedef struct state_t
{
  theora_state ts;
  theora_info ti;
  /* Used for encoding only */
  ogg_int64_t nframes;
} state_t;

#define Theora_state_val(v) (*((state_t**)Data_custom_val(v)))

static void finalize_state(value s)
{
  state_t *state = Theora_state_val(s);
  theora_clear(&state->ts);
  theora_info_clear(&state->ti);
  free(state);
}

static struct custom_operations state_ops =
{
  "ocaml_theora_state",
  finalize_state,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/** Encoding API **/

CAMLprim value ocaml_theora_encode_init(value info)
{
  CAMLparam1(info);
  CAMLlocal1(ans);
  int ret;
  state_t *state = malloc(sizeof(state_t));
  state->nframes = 0;
  theora_info_init(&state->ti);
  info_of_val(info, &state->ti);
  ret = theora_encode_init(&state->ts, &state->ti);
  if (ret < 0)
  {
    theora_info_clear(&state->ti);
    free(state);
    check_err(ret);
  }

  ans = caml_alloc_custom(&state_ops, sizeof(state_t*), 1, 0);
  Theora_state_val(ans) = state;

  CAMLreturn(ans);
}

CAMLprim value ocaml_theora_encode_header(value t_state, value o_stream_state)
{
  CAMLparam2(t_state, o_stream_state);
  state_t *state = Theora_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;

  check_err(theora_encode_header(&state->ts, &op));
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_theora_encode_tables(value t_state, value o_stream_state)
{
  CAMLparam2(t_state, o_stream_state);
  state_t *state = Theora_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;

  check_err(theora_encode_tables(&state->ts, &op));
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

/* Inspired of examples/encoder_example.c in theora sources. */
CAMLprim value ocaml_theora_encode_page(value t_state, value o_stream_state, value feed)
{
  CAMLparam3(t_state, feed, o_stream_state);
  CAMLlocal1(v);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  state_t *state = Theora_state_val(t_state);
  yuv_buffer yb;
  /* You'll go to Hell for using static variables */
  ogg_page og;
  ogg_packet op;
  int ret;

  /* Is there a video page flushed? If not, work until there is. */
  while(1)
  {
    if(ogg_stream_pageout(os, &og) > 0)
    {
      CAMLreturn(value_of_page(&og));
    }
    assert(!ogg_stream_eos(os)); /* TODO: raise End_of_stream */

    /* Read and process more video. */

    /* Encode the theora packet. */
    v = caml_callback(feed, Val_unit);
    yuv_of_val(v,&yb);

    caml_enter_blocking_section();
    ret = theora_encode_YUVin(&state->ts, &yb);
    caml_leave_blocking_section();
    state->nframes++;

    if (ret != 0)
      /* TODO:
       * \retval OC_EINVAL Encoder is not ready, or is finished.
       * \retval -1 The size of the given frame differs from those previously input */
      check_err(ret);

    /* TODO: second argument should be 1 if it's the last frame. */
    ret = theora_encode_packetout(&state->ts, 0, &op);
    /* TODO:
     * \retval 0 No internal storage exists OR no packet is ready
     * \retval -1 The encoding process has completed
     * \retval 1 Success */
    if (ret != 1)
      check_err(ret);

    /* Put the packet in the ogg stream. */
    ogg_stream_packetin(os, &op);
  }
}

CAMLprim value ocaml_theora_encode_buffer(value t_state, value o_stream_state, value frame)
{
  CAMLparam3(t_state, o_stream_state, frame);
  CAMLlocal1(v);
  state_t *state = Theora_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  yuv_buffer yb;
  ogg_packet op;
  int ret;

  assert(!ogg_stream_eos(os)); /* TODO: raise End_of_stream */

  /* Encode the theora packet. */
  yuv_of_val(frame,&yb);

  caml_enter_blocking_section();
  ret = theora_encode_YUVin(&state->ts, &yb);
  caml_leave_blocking_section();
  state->nframes++;
  if (ret != 0)
    /* TODO:
     * \retval OC_EINVAL Encoder is not ready, or is finished.
     * \retval -1 The size of the given frame differs from those previously input */
    check_err(ret);

  /* TODO: second argument should be 1 if it's the last frame. */
  ret = theora_encode_packetout(&state->ts, 0, &op);
  /* TODO:
   * \retval 0 No internal storage exists OR no packet is ready
   * \retval -1 The encoding process has completed
   * \retval 1 Success */
  if (ret != 1)
    check_err(ret);

  /* Put the packet in the ogg stream. */
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_theora_encode_eos(value t_state, value o_stream_state)
{
  CAMLparam2(t_state,o_stream_state);
  state_t *state = Theora_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  
  op.packet = (unsigned char *)NULL;
  op.bytes = 0;
  op.b_o_s = 0;
  op.e_o_s = 1;
  op.granulepos = state->ts.granulepos+1;
  /* Header contains 3 packets. */
  op.packetno = 3+state->nframes+1;
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);   

}

CAMLprim value ocaml_theora_encode_comments(value o_stream_state, value comments)
{
  CAMLparam2(o_stream_state, comments);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  theora_comment tc;
  int i;

  theora_comment_init(&tc);
  for(i = 0; i < Wosize_val(comments); i++)
    theora_comment_add_tag(&tc, String_val(Field(Field(comments, i), 0)), String_val(Field(Field(comments, i), 1)));
  theora_encode_comment(&tc, &op);
  ogg_stream_packetin(os, &op);
  theora_comment_clear(&tc);

  CAMLreturn(Val_unit);
}


/** Decoding API **/

CAMLprim value caml_theora_check(value packet)
{
  CAMLparam1(packet);
  ogg_packet *op = Packet_val(packet);

  theora_info ti;
  theora_comment tc;
  theora_comment_init(&tc);
  theora_info_init(&ti);
  int ret;

  ret = theora_decode_header(&ti, &tc, op);

  theora_comment_clear(&tc);
  theora_info_clear(&ti);

  if (ret == 0) 
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

CAMLprim value ocaml_theora_create(value packet1, value packet2, value packet3)
{
  CAMLparam3(packet1,packet2,packet3);
  CAMLlocal4(ret,t,comment,tmp);
  state_t *state = malloc(sizeof(state_t));
  /* Not used for decoding.. */
  state->nframes = 0;
  theora_comment tc;
  ogg_packet *op1 = Packet_val(packet1);
  ogg_packet *op2 = Packet_val(packet2);
  ogg_packet *op3 = Packet_val(packet3);
  theora_comment_init(&tc);
  theora_info_init(&state->ti);

  if (theora_decode_header(&state->ti, &tc, op1) != 0)
  {
    theora_comment_clear(&tc);
    theora_info_clear(&state->ti);
    free(state);
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  }

  if (theora_decode_header(&state->ti, &tc, op2) != 0)
  {
    theora_comment_clear(&tc);
    theora_info_clear(&state->ti);
    free(state);
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  }

  if (theora_decode_header(&state->ti, &tc, op3) != 0)
  {
    theora_comment_clear(&tc);
    theora_info_clear(&state->ti);
    free(state);
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  }

  comment = caml_alloc_tuple(tc.comments + 1);
  Store_field(comment,0,caml_copy_string(tc.vendor));
  if(tc.comments){
    int i;
    int len;
    for(i=0;i<tc.comments;i++){
      if(tc.user_comments[i]){
        len=tc.comment_lengths[i];
        tmp=caml_alloc_string(len);
        memcpy(String_val(tmp),tc.user_comments[i],len);
        Store_field(comment,i+1,tmp);
      }
    }
  }

  theora_decode_init(&state->ts, &state->ti);

  t = caml_alloc_custom(&state_ops, sizeof(state_t*), 1, 0);
  Theora_state_val(t) = state;

  ret = caml_alloc_tuple(3);
  Store_field (ret, 0, t);
  Store_field (ret, 1, val_of_info(&state->ti));
  Store_field (ret, 2, comment);

  theora_comment_clear(&tc);

  CAMLreturn(ret);
}

CAMLprim value ocaml_theora_decode_YUVout(value decoder, value _os)
{
  CAMLparam2(decoder,_os);
  ogg_stream_state *os = Stream_state_val(_os);
  state_t *state = Theora_state_val(decoder);
  yuv_buffer yb;
  ogg_packet op;

  if (ogg_stream_packetout(os,&op) == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  check_err(theora_decode_packetin(&state->ts,&op));

  caml_enter_blocking_section();
  theora_decode_YUVout(&state->ts,&yb);
  caml_leave_blocking_section();

  CAMLreturn(val_of_yuv(&yb));
}

/* Ogg skeleton interface */

/* Wrappers */
static void write32le(unsigned char *ptr,ogg_uint32_t v)
{
  ptr[0]=v&0xff;
  ptr[1]=(v>>8)&0xff;
  ptr[2]=(v>>16)&0xff;
  ptr[3]=(v>>24)&0xff;
}

static void write64le(unsigned char *ptr,ogg_int64_t v)
{
  ogg_uint32_t hi=v>>32;
  ptr[0]=v&0xff;
  ptr[1]=(v>>8)&0xff;
  ptr[2]=(v>>16)&0xff;
  ptr[3]=(v>>24)&0xff;
  ptr[4]=hi&0xff;
  ptr[5]=(hi>>8)&0xff;
  ptr[6]=(hi>>16)&0xff;
  ptr[7]=(hi>>24)&0xff;
}

/* Values from http://xiph.org/ogg/doc/skeleton.html */
#define FISBONE_IDENTIFIER "fisbone\0"
#define FISBONE_MESSAGE_HEADER_OFFSET 44
#define FISBONE_SIZE 52

/* Code from theorautils.c in ffmpeg2theora */
CAMLprim value ocaml_theora_skeleton_fisbone(value serial, value info, value start, value content)
{
  CAMLparam4(serial,info,start,content);
  CAMLlocal1(packet);
  ogg_packet op;
  theora_info ti;
  info_of_val(info,&ti); 
  int len = FISBONE_SIZE+caml_string_length(content);

  memset (&op, 0, sizeof (op));
  op.packet = malloc(len);
  if (op.packet == NULL)
    caml_failwith("malloc");

  memset (op.packet, 0, len);
  /* it will be the fisbone packet for the theora video */
  memcpy (op.packet, FISBONE_IDENTIFIER, 8); /* identifier */
  write32le(op.packet+8, FISBONE_MESSAGE_HEADER_OFFSET); /* offset of the message header fields */
  write32le(op.packet+12, Nativeint_val(serial)); /* serialno of the theora stream */
  write32le(op.packet+16, 3); /* number of header packets */
  /* granulerate, temporal resolution of the bitstream in samples/microsecond */
  write64le(op.packet+20, (ogg_int64_t)ti.fps_numerator); /* granulrate numerator */
  write64le(op.packet+28, (ogg_int64_t)ti.fps_denominator); /* granulrate denominator */
  write64le(op.packet+36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet+44, 0); /* preroll, for theora its 0 */
  *(op.packet+48) = theora_granule_shift (&ti); /* granule shift */
  memcpy(op.packet+FISBONE_SIZE, String_val(content), caml_string_length(content)); /* message header field */

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}

