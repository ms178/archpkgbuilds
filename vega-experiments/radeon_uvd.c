/**************************************************************************
 *
 * Copyright 2011 Advanced Micro Devices, Inc.
 *
 * SPDX-License-Identifier: MIT
 *
 **************************************************************************/

#include "radeon_uvd.h"

#include "pipe/p_video_codec.h"
#include "radeon_video.h"
#include "radeonsi/si_pipe.h"
#include "util/u_memory.h"
#include "util/u_video.h"
#include "util/vl_zscan_data.h"
#include "vl/vl_defines.h"
#include <sys/types.h>

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <immintrin.h> // For SSE2/AVX intrinsics

#if defined(__GNUC__) || defined(__clang__)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x)   (x)
#define UNLIKELY(x) (x)
#endif

#define NUM_BUFFERS 4

#define NUM_MPEG2_REFS 6
#define NUM_H264_REFS  17
#define NUM_VC1_REFS   5

#define FB_BUFFER_OFFSET         0x1000
#define FB_BUFFER_SIZE           2048
#define FB_BUFFER_SIZE_TONGA     (2048 * 64)
#define IT_SCALING_TABLE_SIZE    992
#define UVD_SESSION_CONTEXT_SIZE (128 * 1024)

/* UVD decoder representation */
struct ruvd_decoder {
   struct pipe_video_codec base;

   ruvd_set_dtb set_dtb;

   unsigned stream_handle;
   unsigned stream_type;
   unsigned frame_number;

   struct pipe_screen *screen;
   struct radeon_winsys *ws;
   struct radeon_cmdbuf cs;

   unsigned cur_buffer;

   struct rvid_buffer msg_fb_it_buffers[NUM_BUFFERS];
   struct ruvd_msg *msg;
   uint32_t *fb;
   unsigned fb_size;
   uint8_t *it;

   struct rvid_buffer bs_buffers[NUM_BUFFERS];
   void *bs_ptr;
   unsigned bs_size;

   struct rvid_buffer dpb;
   bool use_legacy;
   struct rvid_buffer ctx;
   struct rvid_buffer sessionctx;
   struct {
      unsigned data0;
      unsigned data1;
      unsigned cmd;
      unsigned cntl;
   } reg;

   void *render_pic_list[16];
};

/* flush IB to the hardware */
static int flush(struct ruvd_decoder *dec, unsigned flags, struct pipe_fence_handle **fence)
{
   return dec->ws->cs_flush(&dec->cs, flags, fence);
}

static int ruvd_dec_fence_wait(struct pipe_video_codec *decoder,
                               struct pipe_fence_handle *fence,
                               uint64_t timeout)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;
   return dec->ws->fence_wait(dec->ws, fence, timeout);
}

static void ruvd_dec_destroy_fence(struct pipe_video_codec *decoder,
                                   struct pipe_fence_handle *fence)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;

   dec->ws->fence_reference(dec->ws, &fence, NULL);
}

/* add a new set register command to the IB */
static inline __attribute__((always_inline)) void set_reg(struct ruvd_decoder *dec, unsigned reg,
                                                          uint32_t val)
{
   radeon_emit(&dec->cs, RUVD_PKT0(reg >> 2, 0));
   radeon_emit(&dec->cs, val);
}

/* send a command to the VCPU through the GPCOM registers */
static inline __attribute__((always_inline)) void
send_cmd(struct ruvd_decoder *dec, unsigned cmd, struct pb_buffer_lean *buf, uint32_t off,
         unsigned usage, enum radeon_bo_domain domain)
{
   int reloc_idx;

   reloc_idx = dec->ws->cs_add_buffer(&dec->cs, buf, usage | RADEON_USAGE_SYNCHRONIZED, domain);
   if (LIKELY(!dec->use_legacy)) {
      uint64_t addr;
      addr = dec->ws->buffer_get_virtual_address(buf);
      addr = addr + off;
      set_reg(dec, dec->reg.data0, addr);
      set_reg(dec, dec->reg.data1, addr >> 32);
   } else {
      off += dec->ws->buffer_get_reloc_offset(buf);
      set_reg(dec, RUVD_GPCOM_VCPU_DATA0, off);
      set_reg(dec, RUVD_GPCOM_VCPU_DATA1, reloc_idx * 4);
   }
   set_reg(dec, dec->reg.cmd, cmd << 1);
}

/* do the codec needs an IT buffer ?*/
static inline __attribute__((always_inline)) bool have_it(struct ruvd_decoder *dec)
{
   return dec->stream_type == RUVD_CODEC_H264_PERF || dec->stream_type == RUVD_CODEC_H265;
}

/* map the next available message/feedback/itscaling buffer */
static void map_msg_fb_it_buf(struct ruvd_decoder *dec)
{
   struct rvid_buffer *buf;
   uint8_t *ptr;

   /* grab the current message/feedback buffer */
   buf = &dec->msg_fb_it_buffers[dec->cur_buffer];

   /* and map it for CPU access */
   ptr =
      dec->ws->buffer_map(dec->ws, buf->res->buf, NULL, PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);

   if (UNLIKELY(!ptr)) {
      RVID_ERR("Failed to map msg/fb/it buffer\n");
      dec->msg = NULL;
      dec->fb = NULL;
      dec->it = NULL;
      return;
   }

   /* calc buffer offsets */
   dec->msg = (struct ruvd_msg *)ptr;
   memset(dec->msg, 0, sizeof(*dec->msg));

   dec->fb = (uint32_t *)(ptr + FB_BUFFER_OFFSET);
   if (have_it(dec))
      dec->it = (uint8_t *)(ptr + FB_BUFFER_OFFSET + dec->fb_size);
   else
      dec->it = NULL;
}

/* unmap and send a message command to the VCPU */
static void send_msg_buf(struct ruvd_decoder *dec)
{
   struct rvid_buffer *buf;

   /* ignore the request if message/feedback buffer isn't mapped */
   if (!dec->msg || !dec->fb)
      return;

   /* grab the current message buffer */
   buf = &dec->msg_fb_it_buffers[dec->cur_buffer];

   /* unmap the buffer */
   dec->ws->buffer_unmap(dec->ws, buf->res->buf);
   dec->msg = NULL;
   dec->fb = NULL;
   dec->it = NULL;

   if (dec->sessionctx.res)
      send_cmd(dec, RUVD_CMD_SESSION_CONTEXT_BUFFER, dec->sessionctx.res->buf, 0,
               RADEON_USAGE_READWRITE, RADEON_DOMAIN_VRAM);

   /* and send it to the hardware */
   send_cmd(dec, RUVD_CMD_MSG_BUFFER, buf->res->buf, 0, RADEON_USAGE_READ, RADEON_DOMAIN_GTT);
}

/* cycle to the next set of buffers */
static inline __attribute__((always_inline)) void next_buffer(struct ruvd_decoder *dec)
{
   ++dec->cur_buffer;
   dec->cur_buffer %= NUM_BUFFERS;
}

/* convert the profile into something UVD understands */
static uint32_t profile2stream_type(struct ruvd_decoder *dec, unsigned family)
{
   switch (u_reduce_video_profile(dec->base.profile)) {
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      return (family >= CHIP_TONGA) ? RUVD_CODEC_H264_PERF : RUVD_CODEC_H264;

   case PIPE_VIDEO_FORMAT_VC1:
      return RUVD_CODEC_VC1;

   case PIPE_VIDEO_FORMAT_MPEG12:
      return RUVD_CODEC_MPEG2;

   case PIPE_VIDEO_FORMAT_MPEG4:
      return RUVD_CODEC_MPEG4;

   case PIPE_VIDEO_FORMAT_HEVC:
      return RUVD_CODEC_H265;

   case PIPE_VIDEO_FORMAT_JPEG:
      return RUVD_CODEC_MJPEG;

   default:
      assert(0);
      return 0;
   }
}

static inline unsigned get_h264_num_dpb_buffers(unsigned level, unsigned fs_in_mb)
{
   if (UNLIKELY(fs_in_mb == 0))
      return 184320; /* Avoid division by zero, return max */

   switch (level) {
   case 30: return 8100 / fs_in_mb;
   case 31: return 18000 / fs_in_mb;
   case 32: return 20480 / fs_in_mb;
   case 41: return 32768 / fs_in_mb;
   case 42: return 34816 / fs_in_mb;
   case 50: return 110400 / fs_in_mb;
   case 51: return 184320 / fs_in_mb;
   default: return 184320 / fs_in_mb;
   }
}

static unsigned calc_ctx_size_h264_perf(struct ruvd_decoder *dec)
{
   unsigned width_in_mb, height_in_mb, ctx_size;
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   unsigned max_references = dec->base.max_references + 1;

   // picture width & height in 16 pixel units
   width_in_mb = width / VL_MACROBLOCK_WIDTH;
   height_in_mb = align(height / VL_MACROBLOCK_HEIGHT, 2);

   if (LIKELY(!dec->use_legacy)) {
      unsigned fs_in_mb = width_in_mb * height_in_mb;
      unsigned num_dpb_buffer_lean = get_h264_num_dpb_buffers(dec->base.level, fs_in_mb);
      num_dpb_buffer_lean++;
      max_references = MAX2(MIN2(NUM_H264_REFS, num_dpb_buffer_lean), max_references);
      ctx_size = max_references * align(width_in_mb * height_in_mb * 192, 256);
   } else {
      // the firmware seems to always assume a minimum of ref frames
      max_references = MAX2(NUM_H264_REFS, max_references);
      // macroblock context buffer
      ctx_size = align(width_in_mb * height_in_mb * max_references * 192, 256);
   }

   return ctx_size;
}

static unsigned calc_ctx_size_h265_main(struct ruvd_decoder *dec)
{
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   unsigned max_references = dec->base.max_references + 1;

   if (dec->base.width * dec->base.height >= 4096 * 2000)
      max_references = MAX2(max_references, 8);
   else
      max_references = MAX2(max_references, 17);

   width = align(width, 16);
   height = align(height, 16);
   return ((width + 255) / 16) * ((height + 255) / 16) * 16 * max_references + 52 * 1024;
}

static unsigned calc_ctx_size_h265_main10(struct ruvd_decoder *dec,
                                          struct pipe_h265_picture_desc *pic)
{
   unsigned log2_ctb_size, width_in_ctb, height_in_ctb, num_16x16_block_per_ctb;
   unsigned context_buffer_size_per_ctb_row, cm_buffer_size, max_mb_address, db_left_tile_pxl_size;
   unsigned db_left_tile_ctx_size = 4096 / 16 * (32 + 16 * 4);

   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);
   unsigned coeff_10bit =
      (pic->pps->sps->bit_depth_luma_minus8 || pic->pps->sps->bit_depth_chroma_minus8) ? 2 : 1;

   unsigned max_references = dec->base.max_references + 1;

   if (dec->base.width * dec->base.height >= 4096 * 2000)
      max_references = MAX2(max_references, 8);
   else
      max_references = MAX2(max_references, 17);

   log2_ctb_size = pic->pps->sps->log2_min_luma_coding_block_size_minus3 + 3 +
                   pic->pps->sps->log2_diff_max_min_luma_coding_block_size;

   width_in_ctb = (width + ((1 << log2_ctb_size) - 1)) >> log2_ctb_size;
   height_in_ctb = (height + ((1 << log2_ctb_size) - 1)) >> log2_ctb_size;

   num_16x16_block_per_ctb = ((1 << log2_ctb_size) >> 4) * ((1 << log2_ctb_size) >> 4);
   context_buffer_size_per_ctb_row = align(width_in_ctb * num_16x16_block_per_ctb * 16, 256);

   // OPTIMIZED: Replaced floating-point ceil() with pure integer arithmetic.
   // Original was: max_mb_address = (unsigned)ceil(height * 8 / 2048.0);
   // Simplified:   max_mb_address = (unsigned)ceil(height / 256.0);
   // Integer equivalent:
   max_mb_address = (height + 255) / 256;

   cm_buffer_size = max_references * context_buffer_size_per_ctb_row * height_in_ctb;
   db_left_tile_pxl_size = coeff_10bit * (max_mb_address * 2 * 2048 + 1024);

   return cm_buffer_size + db_left_tile_ctx_size + db_left_tile_pxl_size;
}

static inline __attribute__((always_inline, const)) unsigned
get_db_pitch_alignment(const struct ruvd_decoder *dec)
{
   /* Vega10+ requires 32, older GPUs require 16 */
   bool is_vega10_plus = ((const struct si_screen *)dec->screen)->info.family >= CHIP_VEGA10;
   return 16 + (16 * is_vega10_plus);
}

/* calculate size of reference picture buffer */
static unsigned calc_dpb_size(struct ruvd_decoder *dec)
{
   unsigned width_in_mb, height_in_mb, image_size, dpb_size;

   // always align them to MB size for dpb calculation
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   // always one more for currently decoded picture
   unsigned max_references = dec->base.max_references + 1;

   // aligned size of a single frame
   image_size = align(width, get_db_pitch_alignment(dec)) * height;
   image_size += image_size / 2;
   image_size = align(image_size, 1024);

   // picture width & height in 16 pixel units
   width_in_mb = width / VL_MACROBLOCK_WIDTH;
   height_in_mb = align(height / VL_MACROBLOCK_HEIGHT, 2);

   switch (u_reduce_video_profile(dec->base.profile)) {
   case PIPE_VIDEO_FORMAT_MPEG4_AVC: {
      if (LIKELY(!dec->use_legacy)) {
         unsigned fs_in_mb = width_in_mb * height_in_mb;
         unsigned alignment = 64, num_dpb_buffer_lean;

         if (dec->stream_type == RUVD_CODEC_H264_PERF)
            alignment = 256;

         num_dpb_buffer_lean = get_h264_num_dpb_buffers(dec->base.level, fs_in_mb);
         num_dpb_buffer_lean++;
         max_references = MAX2(MIN2(NUM_H264_REFS, num_dpb_buffer_lean), max_references);
         dpb_size = image_size * max_references;
         if ((dec->stream_type != RUVD_CODEC_H264_PERF) ||
             (((struct si_screen *)dec->screen)->info.family < CHIP_POLARIS10)) {
            dpb_size += max_references * align(width_in_mb * height_in_mb * 192, alignment);
            dpb_size += align(width_in_mb * height_in_mb * 32, alignment);
         }
      } else {
         // the firmware seems to always assume a minimum of ref frames
         max_references = MAX2(NUM_H264_REFS, max_references);
         // reference picture buffer
         dpb_size = image_size * max_references;
         if ((dec->stream_type != RUVD_CODEC_H264_PERF) ||
             (((struct si_screen *)dec->screen)->info.family < CHIP_POLARIS10)) {
            // macroblock context buffer
            dpb_size += width_in_mb * height_in_mb * max_references * 192;
            // IT surface buffer
            dpb_size += width_in_mb * height_in_mb * 32;
         }
      }
      break;
   }

   case PIPE_VIDEO_FORMAT_HEVC:
      if (dec->base.width * dec->base.height >= 4096 * 2000)
         max_references = MAX2(max_references, 8);
      else
         max_references = MAX2(max_references, 17);

      width = align(width, 16);
      height = align(height, 16);
      if (dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10)
         dpb_size = align((align(width, get_db_pitch_alignment(dec)) * height * 9) / 4, 256) *
                    max_references;
      else
         dpb_size = align((align(width, get_db_pitch_alignment(dec)) * height * 3) / 2, 256) *
                    max_references;
      break;

   case PIPE_VIDEO_FORMAT_VC1:
      // the firmware seems to always assume a minimum of ref frames
      max_references = MAX2(NUM_VC1_REFS, max_references);

      // reference picture buffer
      dpb_size = image_size * max_references;

      // CONTEXT_BUFFER
      dpb_size += width_in_mb * height_in_mb * 128;

      // IT surface buffer
      dpb_size += width_in_mb * 64;

      // DB surface buffer
      dpb_size += width_in_mb * 128;

      // BP
      dpb_size += align(MAX2(width_in_mb, height_in_mb) * 7 * 16, 64);
      break;

   case PIPE_VIDEO_FORMAT_MPEG12:
      // reference picture buffer, must be big enough for all frames
      dpb_size = image_size * NUM_MPEG2_REFS;
      break;

   case PIPE_VIDEO_FORMAT_MPEG4:
      // reference picture buffer
      dpb_size = image_size * max_references;

      // CM
      dpb_size += width_in_mb * height_in_mb * 64;

      // IT surface buffer
      dpb_size += align(width_in_mb * height_in_mb * 32, 64);

      dpb_size = MAX2(dpb_size, 30 * 1024 * 1024);
      break;

   case PIPE_VIDEO_FORMAT_JPEG:
      dpb_size = 0;
      break;

   default:
      // something is missing here
      assert(0);

      // at least use a sane default value
      dpb_size = 32 * 1024 * 1024;
      break;
   }
   return dpb_size;
}

/* free associated data in the video buffer callback */
static void ruvd_destroy_associated_data(void *data)
{
   /* NOOP, since we only use an intptr */
}

/* get h264 specific message bits */
static struct ruvd_h264 get_h264_msg(struct ruvd_decoder *dec, struct pipe_h264_picture_desc *pic)
{
   struct ruvd_h264 result;
   memset(&result, 0, sizeof(result));

   switch (pic->base.profile) {
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_BASELINE:
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_CONSTRAINED_BASELINE:
      result.profile = RUVD_H264_PROFILE_BASELINE;
      break;
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_MAIN:
      result.profile = RUVD_H264_PROFILE_MAIN;
      break;
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH:
      result.profile = RUVD_H264_PROFILE_HIGH;
      break;
   default:
      assert(0);
      break;
   }

   result.level = dec->base.level;

   // --- OPTIMIZED BATCHED FLAG CONSTRUCTION ---
   // Compute the entire SPS flags field in a local register, then write once.
   uint32_t sps_flags = 0;
   sps_flags |= (uint32_t)pic->pps->sps->direct_8x8_inference_flag << 0;
   sps_flags |= (uint32_t)pic->pps->sps->mb_adaptive_frame_field_flag << 1;
   sps_flags |= (uint32_t)pic->pps->sps->frame_mbs_only_flag << 2;
   sps_flags |= (uint32_t)pic->pps->sps->delta_pic_order_always_zero_flag << 3;
   sps_flags |= (uint32_t)pic->pps->sps->gaps_in_frame_num_value_allowed_flag << 5;
   result.sps_info_flags = sps_flags;

   result.chroma_format = pic->pps->sps->chroma_format_idc;
   result.bit_depth_luma_minus8 = pic->pps->sps->bit_depth_luma_minus8;
   result.bit_depth_chroma_minus8 = pic->pps->sps->bit_depth_chroma_minus8;
   result.log2_max_frame_num_minus4 = pic->pps->sps->log2_max_frame_num_minus4;
   result.pic_order_cnt_type = pic->pps->sps->pic_order_cnt_type;
   result.log2_max_pic_order_cnt_lsb_minus4 = pic->pps->sps->log2_max_pic_order_cnt_lsb_minus4;

   // Compute the entire PPS flags field in a local register, then write once.
   uint32_t pps_flags = 0;
   pps_flags |= (uint32_t)pic->pps->transform_8x8_mode_flag << 0;
   pps_flags |= (uint32_t)pic->pps->redundant_pic_cnt_present_flag << 1;
   pps_flags |= (uint32_t)pic->pps->constrained_intra_pred_flag << 2;
   pps_flags |= (uint32_t)pic->pps->deblocking_filter_control_present_flag << 3;
   pps_flags |= (uint32_t)pic->pps->weighted_bipred_idc << 4;
   pps_flags |= (uint32_t)pic->pps->weighted_pred_flag << 6;
   pps_flags |= (uint32_t)pic->pps->bottom_field_pic_order_in_frame_present_flag << 7;
   pps_flags |= (uint32_t)pic->pps->entropy_coding_mode_flag << 8;
   result.pps_info_flags = pps_flags;
   // --- END OF OPTIMIZED LOGIC ---

   result.num_slice_groups_minus1 = pic->pps->num_slice_groups_minus1;
   result.slice_group_map_type = pic->pps->slice_group_map_type;
   result.slice_group_change_rate_minus1 = pic->pps->slice_group_change_rate_minus1;
   result.pic_init_qp_minus26 = pic->pps->pic_init_qp_minus26;
   result.chroma_qp_index_offset = pic->pps->chroma_qp_index_offset;
   result.second_chroma_qp_index_offset = pic->pps->second_chroma_qp_index_offset;

   memcpy(result.scaling_list_4x4, pic->pps->ScalingList4x4, 6 * 16);
   memcpy(result.scaling_list_8x8, pic->pps->ScalingList8x8, 2 * 64);

   if (dec->stream_type == RUVD_CODEC_H264_PERF) {
      memcpy(dec->it, result.scaling_list_4x4, 6 * 16);
      memcpy((dec->it + 96), result.scaling_list_8x8, 2 * 64);
   }

   result.num_ref_frames = pic->num_ref_frames;
   result.num_ref_idx_l0_active_minus1 = pic->num_ref_idx_l0_active_minus1;
   result.num_ref_idx_l1_active_minus1 = pic->num_ref_idx_l1_active_minus1;
   result.frame_num = pic->frame_num;
   memcpy(result.frame_num_list, pic->frame_num_list, 4 * 16);
   result.curr_field_order_cnt_list[0] = pic->field_order_cnt[0];
   result.curr_field_order_cnt_list[1] = pic->field_order_cnt[1];
   memcpy(result.field_order_cnt_list, pic->field_order_cnt_list, 4 * 16 * 2);

   result.decoded_pic_idx = pic->frame_num;

   return result;
}

/* get h265 specific message bits */
static struct ruvd_h265 get_h265_msg(struct ruvd_decoder *dec, struct pipe_video_buffer *target,
                                     struct pipe_h265_picture_desc *pic)
{
   struct ruvd_h265 result;
   unsigned i;

   memset(&result, 0, sizeof(result));

   result.sps_info_flags = 0;
   result.sps_info_flags |= pic->pps->sps->scaling_list_enabled_flag << 0;
   result.sps_info_flags |= pic->pps->sps->amp_enabled_flag << 1;
   result.sps_info_flags |= pic->pps->sps->sample_adaptive_offset_enabled_flag << 2;
   result.sps_info_flags |= pic->pps->sps->pcm_enabled_flag << 3;
   result.sps_info_flags |= pic->pps->sps->pcm_loop_filter_disabled_flag << 4;
   result.sps_info_flags |= pic->pps->sps->long_term_ref_pics_present_flag << 5;
   result.sps_info_flags |= pic->pps->sps->sps_temporal_mvp_enabled_flag << 6;
   result.sps_info_flags |= pic->pps->sps->strong_intra_smoothing_enabled_flag << 7;
   result.sps_info_flags |= pic->pps->sps->separate_colour_plane_flag << 8;
   if (((struct si_screen *)dec->screen)->info.family == CHIP_CARRIZO) {
      result.sps_info_flags |= 1 << 9;
   }

   result.chroma_format = pic->pps->sps->chroma_format_idc;
   result.bit_depth_luma_minus8 = pic->pps->sps->bit_depth_luma_minus8;
   result.bit_depth_chroma_minus8 = pic->pps->sps->bit_depth_chroma_minus8;
   result.log2_max_pic_order_cnt_lsb_minus4 = pic->pps->sps->log2_max_pic_order_cnt_lsb_minus4;
   result.sps_max_dec_pic_buffering_minus1 = pic->pps->sps->sps_max_dec_pic_buffering_minus1;
   result.log2_min_luma_coding_block_size_minus3 =
      pic->pps->sps->log2_min_luma_coding_block_size_minus3;
   result.log2_diff_max_min_luma_coding_block_size =
      pic->pps->sps->log2_diff_max_min_luma_coding_block_size;
   result.log2_min_transform_block_size_minus2 =
      pic->pps->sps->log2_min_transform_block_size_minus2;
   result.log2_diff_max_min_transform_block_size =
      pic->pps->sps->log2_diff_max_min_transform_block_size;
   result.max_transform_hierarchy_depth_inter = pic->pps->sps->max_transform_hierarchy_depth_inter;
   result.max_transform_hierarchy_depth_intra = pic->pps->sps->max_transform_hierarchy_depth_intra;
   result.pcm_sample_bit_depth_luma_minus1 = pic->pps->sps->pcm_sample_bit_depth_luma_minus1;
   result.pcm_sample_bit_depth_chroma_minus1 = pic->pps->sps->pcm_sample_bit_depth_chroma_minus1;
   result.log2_min_pcm_luma_coding_block_size_minus3 =
      pic->pps->sps->log2_min_pcm_luma_coding_block_size_minus3;
   result.log2_diff_max_min_pcm_luma_coding_block_size =
      pic->pps->sps->log2_diff_max_min_pcm_luma_coding_block_size;
   result.num_short_term_ref_pic_sets = pic->pps->sps->num_short_term_ref_pic_sets;

   result.pps_info_flags = 0;
   result.pps_info_flags |= pic->pps->dependent_slice_segments_enabled_flag << 0;
   result.pps_info_flags |= pic->pps->output_flag_present_flag << 1;
   result.pps_info_flags |= pic->pps->sign_data_hiding_enabled_flag << 2;
   result.pps_info_flags |= pic->pps->cabac_init_present_flag << 3;
   result.pps_info_flags |= pic->pps->constrained_intra_pred_flag << 4;
   result.pps_info_flags |= pic->pps->transform_skip_enabled_flag << 5;
   result.pps_info_flags |= pic->pps->cu_qp_delta_enabled_flag << 6;
   result.pps_info_flags |= pic->pps->pps_slice_chroma_qp_offsets_present_flag << 7;
   result.pps_info_flags |= pic->pps->weighted_pred_flag << 8;
   result.pps_info_flags |= pic->pps->weighted_bipred_flag << 9;
   result.pps_info_flags |= pic->pps->transquant_bypass_enabled_flag << 10;
   result.pps_info_flags |= pic->pps->tiles_enabled_flag << 11;
   result.pps_info_flags |= pic->pps->entropy_coding_sync_enabled_flag << 12;
   result.pps_info_flags |= pic->pps->uniform_spacing_flag << 13;
   result.pps_info_flags |= pic->pps->loop_filter_across_tiles_enabled_flag << 14;
   result.pps_info_flags |= pic->pps->pps_loop_filter_across_slices_enabled_flag << 15;
   result.pps_info_flags |= pic->pps->deblocking_filter_override_enabled_flag << 16;
   result.pps_info_flags |= pic->pps->pps_deblocking_filter_disabled_flag << 17;
   result.pps_info_flags |= pic->pps->lists_modification_present_flag << 18;
   result.pps_info_flags |= pic->pps->slice_segment_header_extension_present_flag << 19;

   result.num_extra_slice_header_bits = pic->pps->num_extra_slice_header_bits;
   result.num_long_term_ref_pic_sps = pic->pps->sps->num_long_term_ref_pics_sps;
   result.num_ref_idx_l0_default_active_minus1 = pic->pps->num_ref_idx_l0_default_active_minus1;
   result.num_ref_idx_l1_default_active_minus1 = pic->pps->num_ref_idx_l1_default_active_minus1;
   result.pps_cb_qp_offset = pic->pps->pps_cb_qp_offset;
   result.pps_cr_qp_offset = pic->pps->pps_cr_qp_offset;
   result.pps_beta_offset_div2 = pic->pps->pps_beta_offset_div2;
   result.pps_tc_offset_div2 = pic->pps->pps_tc_offset_div2;
   result.diff_cu_qp_delta_depth = pic->pps->diff_cu_qp_delta_depth;
   result.num_tile_columns_minus1 = pic->pps->num_tile_columns_minus1;
   result.num_tile_rows_minus1 = pic->pps->num_tile_rows_minus1;
   result.log2_parallel_merge_level_minus2 = pic->pps->log2_parallel_merge_level_minus2;
   result.init_qp_minus26 = pic->pps->init_qp_minus26;

   for (i = 0; i < 19; ++i) {
      result.column_width_minus1[i] = pic->pps->column_width_minus1[i];
   }

   for (i = 0; i < 21; ++i) {
      result.row_height_minus1[i] = pic->pps->row_height_minus1[i];
   }

   result.num_delta_pocs_ref_rps_idx = pic->NumDeltaPocsOfRefRpsIdx;
   result.curr_poc = pic->CurrPicOrderCntVal;

   // 1. Build a bitmask of currently active reference picture indices.
   uint64_t active_ref_mask = 0;
   for (i = 0; i < 16; ++i) {
      if (!pic->ref[i]) {
         break; // End of reference list
      }
      uintptr_t ref_idx = (uintptr_t)vl_video_buffer_get_associated_data(pic->ref[i], &dec->base);
      if (ref_idx < 16) { // Ensure index is valid before setting bit
         active_ref_mask |= (1ULL << ref_idx);
      }
   }

   // 2. Evict stale pictures in a single pass using the bitmask.
   for (i = 0; i < 16; ++i) {
      if (dec->render_pic_list[i] && !(active_ref_mask & (1ULL << i))) {
         dec->render_pic_list[i] = NULL;
      }
   }

   // 3. Find a free slot for the new target and assign its index.
   result.curr_idx = 16; // Use an invalid index as a sentinel
   for (i = 0; i < 16; ++i) {
      if (dec->render_pic_list[i] == NULL) {
         dec->render_pic_list[i] = target;
         result.curr_idx = i;
         break;
      }
   }
   // This should never happen in a compliant stream, but assert for robustness.
   assert(result.curr_idx < 16 && "No free slot in render picture list!");

   vl_video_buffer_set_associated_data(target, &dec->base, (void *)(uintptr_t)result.curr_idx,
                                       &ruvd_destroy_associated_data);

   // Populate the firmware's reference list using the indices.
   for (i = 0; i < 16; ++i) {
      result.poc_list[i] = pic->PicOrderCntVal[i];
      if (pic->ref[i]) {
         result.ref_pic_list[i] = (uintptr_t)vl_video_buffer_get_associated_data(pic->ref[i], &dec->base);
      } else {
         result.ref_pic_list[i] = 0x7F; // Sentinel for unused entry
      }
   }

   for (i = 0; i < 8; ++i) {
      result.ref_pic_set_st_curr_before[i] = 0xFF;
      result.ref_pic_set_st_curr_after[i] = 0xFF;
      result.ref_pic_set_lt_curr[i] = 0xFF;
   }

   for (i = 0; i < pic->NumPocStCurrBefore; ++i) {
      result.ref_pic_set_st_curr_before[i] = pic->RefPicSetStCurrBefore[i];
   }

   for (i = 0; i < pic->NumPocStCurrAfter; ++i) {
      result.ref_pic_set_st_curr_after[i] = pic->RefPicSetStCurrAfter[i];
   }

   for (i = 0; i < pic->NumPocLtCurr; ++i) {
      result.ref_pic_set_lt_curr[i] = pic->RefPicSetLtCurr[i];
   }

   for (i = 0; i < 6; ++i) {
      result.ucScalingListDCCoefSizeID2[i] = pic->pps->sps->ScalingListDCCoeff16x16[i];
   }

   for (i = 0; i < 2; ++i) {
      result.ucScalingListDCCoefSizeID3[i] = pic->pps->sps->ScalingListDCCoeff32x32[i];
   }

   memcpy(dec->it, pic->pps->sps->ScalingList4x4, 6 * 16);
   memcpy(dec->it + 96, pic->pps->sps->ScalingList8x8, 6 * 64);
   memcpy(dec->it + 480, pic->pps->sps->ScalingList16x16, 6 * 64);
   memcpy(dec->it + 864, pic->pps->sps->ScalingList32x32, 2 * 64);

   if (pic->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10) {
      if (target->buffer_format == PIPE_FORMAT_P010 || target->buffer_format == PIPE_FORMAT_P016) {
         result.p010_mode = 1;
         result.msb_mode = 1;
      } else {
         result.luma_10to8 = 5;
         result.chroma_10to8 = 5;
         result.sclr_luma10to8 = 4;
         result.sclr_chroma10to8 = 4;
      }
   }

   /* TODO
    *  result.highestTid;
    *  result.isNonRef;
    *
    *  IDRPicFlag;
    *  RAPPicFlag;
    *  NumPocTotalCurr;
    *  NumShortTermPictureSliceHeaderBits;
    *  NumLongTermPictureSliceHeaderBits;
    *
    *  IsLongTerm[16];
    */

   return result;
}

/* get vc1 specific message bits */
static struct ruvd_vc1 get_vc1_msg(struct pipe_vc1_picture_desc *pic)
{
   struct ruvd_vc1 result;

   memset(&result, 0, sizeof(result));

   switch (pic->base.profile) {
   case PIPE_VIDEO_PROFILE_VC1_SIMPLE:
      result.profile = RUVD_VC1_PROFILE_SIMPLE;
      result.level = 1;
      break;

   case PIPE_VIDEO_PROFILE_VC1_MAIN:
      result.profile = RUVD_VC1_PROFILE_MAIN;
      result.level = 2;
      break;

   case PIPE_VIDEO_PROFILE_VC1_ADVANCED:
      result.profile = RUVD_VC1_PROFILE_ADVANCED;
      result.level = 4;
      break;

   default:
      assert(0);
   }

   /* fields common for all profiles */
   result.sps_info_flags |= pic->postprocflag << 7;
   result.sps_info_flags |= pic->pulldown << 6;
   result.sps_info_flags |= pic->interlace << 5;
   result.sps_info_flags |= pic->tfcntrflag << 4;
   result.sps_info_flags |= pic->finterpflag << 3;
   result.sps_info_flags |= pic->psf << 1;

   result.pps_info_flags |= pic->range_mapy_flag << 31;
   result.pps_info_flags |= pic->range_mapy << 28;
   result.pps_info_flags |= pic->range_mapuv_flag << 27;
   result.pps_info_flags |= pic->range_mapuv << 24;
   result.pps_info_flags |= pic->multires << 21;
   result.pps_info_flags |= pic->maxbframes << 16;
   result.pps_info_flags |= pic->overlap << 11;
   result.pps_info_flags |= pic->quantizer << 9;
   result.pps_info_flags |= pic->panscan_flag << 7;
   result.pps_info_flags |= pic->refdist_flag << 6;
   result.pps_info_flags |= pic->vstransform << 0;

   /* some fields only apply to main/advanced profile */
   if (pic->base.profile != PIPE_VIDEO_PROFILE_VC1_SIMPLE) {
      result.pps_info_flags |= pic->syncmarker << 20;
      result.pps_info_flags |= pic->rangered << 19;
      result.pps_info_flags |= pic->loopfilter << 5;
      result.pps_info_flags |= pic->fastuvmc << 4;
      result.pps_info_flags |= pic->extended_mv << 3;
      result.pps_info_flags |= pic->extended_dmv << 8;
      result.pps_info_flags |= pic->dquant << 1;
   }

   result.chroma_format = 1;

#if 0
   //(((unsigned int)(pPicParams->advance.reserved1)) << SPS_INFO_VC1_RESERVED_SHIFT)
   uint32_t  slice_count
   uint8_t   picture_type
   uint8_t   frame_coding_mode
   uint8_t   deblockEnable
   uint8_t   pquant
#endif

   return result;
}

/* extract the frame number from a referenced video buffer */
static uint32_t get_ref_pic_idx(struct ruvd_decoder *dec, struct pipe_video_buffer *ref)
{
   const uint32_t current_frame = dec->frame_number;
   const uint32_t max_frame = (current_frame > 0) ? current_frame - 1 : 0;

   // Fallback for invalid reference is the most recent possible frame.
   if (UNLIKELY(!ref))
      return max_frame;

   uintptr_t frame_in = (uintptr_t)vl_video_buffer_get_associated_data(ref, &dec->base);
   uint32_t frame = (uint32_t)frame_in;

   // The valid range of frames is a sliding window of size NUM_MPEG2_REFS
   // ending at the frame just before the current one.
   const uint32_t min_frame =
      (current_frame > NUM_MPEG2_REFS) ? current_frame - NUM_MPEG2_REFS : 0;

   // OPTIMIZED: Use explicit, separate clamping operations.
   // This form is highly friendly to modern compilers, which will generate
   // branch-free conditional move (cmov) instructions.
   if (frame > max_frame)
      frame = max_frame;
   if (frame < min_frame)
      frame = min_frame;

   return frame;
}

/* get mpeg2 specific msg bits */
static struct ruvd_mpeg2 get_mpeg2_msg(struct ruvd_decoder *dec,
                                       struct pipe_mpeg12_picture_desc *pic)
{
   const int *zscan = pic->alternate_scan ? vl_zscan_alternate : vl_zscan_normal;
   struct ruvd_mpeg2 result;
   unsigned i;

   memset(&result, 0, sizeof(result));
   result.decoded_pic_idx = dec->frame_number;
   for (i = 0; i < 2; ++i)
      result.ref_pic_idx[i] = get_ref_pic_idx(dec, pic->ref[i]);

   if (pic->intra_matrix) {
      result.load_intra_quantiser_matrix = 1;
      for (i = 0; i < 64; ++i) {
         result.intra_quantiser_matrix[i] = pic->intra_matrix[zscan[i]];
      }
   }
   if (pic->non_intra_matrix) {
      result.load_nonintra_quantiser_matrix = 1;
      for (i = 0; i < 64; ++i) {
         result.nonintra_quantiser_matrix[i] = pic->non_intra_matrix[zscan[i]];
      }
   }

   result.profile_and_level_indication = 0;
   result.chroma_format = 0x1;

   result.picture_coding_type = pic->picture_coding_type;
   result.f_code[0][0] = pic->f_code[0][0] + 1;
   result.f_code[0][1] = pic->f_code[0][1] + 1;
   result.f_code[1][0] = pic->f_code[1][0] + 1;
   result.f_code[1][1] = pic->f_code[1][1] + 1;
   result.intra_dc_precision = pic->intra_dc_precision;
   result.pic_structure = pic->picture_structure;
   result.top_field_first = pic->top_field_first;
   result.frame_pred_frame_dct = pic->frame_pred_frame_dct;
   result.concealment_motion_vectors = pic->concealment_motion_vectors;
   result.q_scale_type = pic->q_scale_type;
   result.intra_vlc_format = pic->intra_vlc_format;
   result.alternate_scan = pic->alternate_scan;

   return result;
}

/* get mpeg4 specific msg bits */
static struct ruvd_mpeg4 get_mpeg4_msg(struct ruvd_decoder *dec,
                                       struct pipe_mpeg4_picture_desc *pic)
{
   struct ruvd_mpeg4 result;
   unsigned i;

   memset(&result, 0, sizeof(result));
   result.decoded_pic_idx = dec->frame_number;
   for (i = 0; i < 2; ++i)
      result.ref_pic_idx[i] = get_ref_pic_idx(dec, pic->ref[i]);

   result.variant_type = 0;
   result.profile_and_level_indication = 0xF0; // ASP Level0

   result.video_object_layer_verid = 0x5; // advanced simple
   result.video_object_layer_shape = 0x0; // rectangular

   result.video_object_layer_width = dec->base.width;
   result.video_object_layer_height = dec->base.height;

   result.vop_time_increment_resolution = pic->vop_time_increment_resolution;

   result.flags |= pic->short_video_header << 0;
   // result.flags |= obmc_disable << 1;
   result.flags |= pic->interlaced << 2;
   result.flags |= 1 << 3; // load_intra_quant_mat
   result.flags |= 1 << 4; // load_nonintra_quant_mat
   result.flags |= pic->quarter_sample << 5;
   result.flags |= 1 << 6; // complexity_estimation_disable
   result.flags |= pic->resync_marker_disable << 7;
   // result.flags |= data_partitioned << 8;
   // result.flags |= reversible_vlc << 9;
   result.flags |= 0 << 10; // newpred_enable
   result.flags |= 0 << 11; // reduced_resolution_vop_enable
   // result.flags |= scalability << 12;
   // result.flags |= is_object_layer_identifier << 13;
   // result.flags |= fixed_vop_rate << 14;
   // result.flags |= newpred_segment_type << 15;

   result.quant_type = pic->quant_type;

   for (i = 0; i < 64; ++i) {
      result.intra_quant_mat[i] = pic->intra_matrix[vl_zscan_normal[i]];
      result.nonintra_quant_mat[i] = pic->non_intra_matrix[vl_zscan_normal[i]];
   }

   /*
    *  int32_t    trd [2]
    *  int32_t    trb [2]
    *  uint8_t    vop_coding_type
    *  uint8_t    vop_fcode_forward
    *  uint8_t    vop_fcode_backward
    *  uint8_t    rounding_control
    *  uint8_t    alternate_vertical_scan_flag
    *  uint8_t    top_field_first
    */

   return result;
}

/**
 * destroy this video decoder
 */
static void ruvd_destroy(struct pipe_video_codec *decoder)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;
   unsigned i;

   assert(decoder);

   map_msg_fb_it_buf(dec);
   if (LIKELY(dec->msg)) {
      dec->msg->size = sizeof(*dec->msg);
      dec->msg->msg_type = RUVD_MSG_DESTROY;
      dec->msg->stream_handle = dec->stream_handle;
      send_msg_buf(dec);

      flush(dec, 0, NULL);
   }

   dec->ws->cs_destroy(&dec->cs);

   for (i = 0; i < NUM_BUFFERS; ++i) {
      si_vid_destroy_buffer(&dec->msg_fb_it_buffers[i]);
      si_vid_destroy_buffer(&dec->bs_buffers[i]);
   }

   si_vid_destroy_buffer(&dec->dpb);
   si_vid_destroy_buffer(&dec->ctx);
   si_vid_destroy_buffer(&dec->sessionctx);

   FREE(dec);
}

/**
 * start decoding of a new frame
 */
static void ruvd_begin_frame(struct pipe_video_codec *decoder, struct pipe_video_buffer *target,
                             struct pipe_picture_desc *picture)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;
   uintptr_t frame;

   assert(decoder);

   frame = ++dec->frame_number;
   vl_video_buffer_set_associated_data(target, decoder, (void *)frame,
                                       &ruvd_destroy_associated_data);

   dec->bs_size = 0;
   dec->bs_ptr = dec->ws->buffer_map(dec->ws, dec->bs_buffers[dec->cur_buffer].res->buf, NULL,
                                     PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
}

/**
 * decode a macroblock
 */
static void ruvd_decode_macroblock(struct pipe_video_codec *decoder,
                                   struct pipe_video_buffer *target,
                                   struct pipe_picture_desc *picture,
                                   const struct pipe_macroblock *macroblocks,
                                   unsigned num_macroblocks)
{
   /* not supported (yet) */
   assert(0);
}

/**
 * decode a bitstream
 */
static void ruvd_decode_bitstream(struct pipe_video_codec *decoder,
                                  struct pipe_video_buffer *target,
                                  struct pipe_picture_desc *picture, unsigned num_buffers,
                                  const void *const *restrict buffers,
                                  const unsigned *restrict sizes)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;
   unsigned i;

   if (UNLIKELY(!dec->bs_ptr)) {
      return;
   }

   // Calculate total size needed first to avoid re-checking in the loop.
   unsigned long total_incoming_size = 0;
   for (i = 0; i < num_buffers; ++i) {
      total_incoming_size += sizes[i];
   }

   unsigned long required_total_size = dec->bs_size + total_incoming_size;
   struct rvid_buffer *buf = &dec->bs_buffers[dec->cur_buffer];

   if (UNLIKELY(required_total_size > buf->res->buf->size)) {
      dec->ws->buffer_unmap(dec->ws, buf->res->buf);
      dec->bs_ptr = NULL;

      required_total_size = align(required_total_size, 128);

      if (!dec->bs_size) {
         struct rvid_buffer old_buf = *buf;
         if (UNLIKELY(!si_vid_create_buffer(dec->screen, buf, required_total_size, buf->usage))) {
            RVID_ERR("Can't create bitstream buffer!");
            return;
         }
         si_vid_destroy_buffer(&old_buf);
      } else if (UNLIKELY(!si_vid_resize_buffer(dec->base.context, buf, required_total_size, NULL))) {
         RVID_ERR("Can't resize bitstream buffer!");
         return;
      }

      dec->bs_ptr = dec->ws->buffer_map(dec->ws, buf->res->buf, NULL,
                                        PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
      if (UNLIKELY(!dec->bs_ptr)) {
         return;
      }
   }

   // The destination pointer is calculated from the immutable base pointer and the current size.
   uint8_t *dest_base = (uint8_t *)dec->bs_ptr;
   uint8_t *current_dest = dest_base + dec->bs_size;

   // --- High-Performance Cache-Bypassing Copy ---
   for (i = 0; i < num_buffers; ++i) {
      const uint8_t *src = (const uint8_t *)buffers[i];
      size_t n = sizes[i];

      // Use a standard memcpy for small chunks where intrinsic overhead isn't worth it.
      if (n < 128) {
         memcpy(current_dest, src, n);
         current_dest += n;
         continue;
      }

      // Handle unaligned start of destination if necessary. This should be rare.
      size_t dest_align_offset = (uintptr_t)current_dest & 0xF;
      if (dest_align_offset) {
         size_t align_bytes = 16 - dest_align_offset;
         // CRITICAL FIX: Ensure we do not copy more bytes than are available in the source chunk.
         if (align_bytes > n) {
            align_bytes = n;
         }
         memcpy(current_dest, src, align_bytes);
         current_dest += align_bytes;
         src += align_bytes;
         n -= align_bytes;
      }

      // Main aligned streaming copy loop.
      const __m128i *s = (__m128i *)src;
      __m128i *d = (__m128i *)current_dest;
      size_t num_vectors = n / 16;
      for (size_t j = 0; j < num_vectors; ++j) {
         __m128i data = _mm_loadu_si128(s++); // Use unaligned load for source flexibility.
         _mm_stream_si128(d++, data);        // Use aligned stream for destination performance.
      }

      // Handle the remainder.
      size_t remainder_offset = num_vectors * 16;
      memcpy(current_dest + remainder_offset, src + remainder_offset, n & 0xF);

      current_dest += n;
   }

   // Update the total size after all copies are done.
   dec->bs_size = current_dest - dest_base;
}

/**
 * end decoding of the current frame
 */
static int ruvd_end_frame(struct pipe_video_codec *decoder, struct pipe_video_buffer *target,
                          struct pipe_picture_desc *picture)
{
   struct ruvd_decoder *dec = (struct ruvd_decoder *)decoder;
   struct pb_buffer_lean *dt;
   struct rvid_buffer *msg_fb_it_buf, *bs_buf;
   unsigned bs_size;

   assert(decoder);

   if (UNLIKELY(!dec->bs_ptr))
      return 1;

   msg_fb_it_buf = &dec->msg_fb_it_buffers[dec->cur_buffer];
   bs_buf = &dec->bs_buffers[dec->cur_buffer];

   bs_size = align(dec->bs_size, 128);
   // Pad the bitstream buffer with zeros to the next 128-byte boundary.
   memset((uint8_t *)dec->bs_ptr + dec->bs_size, 0, bs_size - dec->bs_size);
   dec->ws->buffer_unmap(dec->ws, bs_buf->res->buf);

   map_msg_fb_it_buf(dec);
   if (UNLIKELY(!dec->msg))
      return 1;

   dec->msg->size = sizeof(*dec->msg);
   dec->msg->msg_type = RUVD_MSG_DECODE;
   dec->msg->stream_handle = dec->stream_handle;
   dec->msg->status_report_feedback_number = dec->frame_number;

   dec->msg->body.decode.stream_type = dec->stream_type;
   dec->msg->body.decode.decode_flags = 0x1;
   dec->msg->body.decode.width_in_samples = dec->base.width;
   dec->msg->body.decode.height_in_samples = dec->base.height;

   if ((picture->profile == PIPE_VIDEO_PROFILE_VC1_SIMPLE) ||
       (picture->profile == PIPE_VIDEO_PROFILE_VC1_MAIN)) {
      dec->msg->body.decode.width_in_samples =
         align(dec->msg->body.decode.width_in_samples, 16) / 16;
      dec->msg->body.decode.height_in_samples =
         align(dec->msg->body.decode.height_in_samples, 16) / 16;
   }

   if (dec->dpb.res)
      dec->msg->body.decode.dpb_size = dec->dpb.res->buf->size;
   dec->msg->body.decode.bsd_size = bs_size;
   dec->msg->body.decode.db_pitch = align(dec->base.width, get_db_pitch_alignment(dec));

   if (dec->stream_type == RUVD_CODEC_H264_PERF &&
       ((struct si_screen *)dec->screen)->info.family >= CHIP_POLARIS10)
      dec->msg->body.decode.dpb_reserved = dec->ctx.res->buf->size;

   dt = dec->set_dtb(dec->msg, (struct vl_video_buffer *)target);
   if (((struct si_screen *)dec->screen)->info.family >= CHIP_STONEY)
      dec->msg->body.decode.dt_wa_chroma_top_offset = dec->msg->body.decode.dt_pitch / 2;

   switch (u_reduce_video_profile(picture->profile)) {
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      dec->msg->body.decode.codec.h264 =
         get_h264_msg(dec, (struct pipe_h264_picture_desc *)picture);
      break;

   case PIPE_VIDEO_FORMAT_HEVC:
      dec->msg->body.decode.codec.h265 =
         get_h265_msg(dec, target, (struct pipe_h265_picture_desc *)picture);
      if (dec->ctx.res == NULL) {
         unsigned ctx_size;
         if (dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10)
            ctx_size = calc_ctx_size_h265_main10(dec, (struct pipe_h265_picture_desc *)picture);
         else
            ctx_size = calc_ctx_size_h265_main(dec);
         if (!si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT)) {
            RVID_ERR("Can't allocated context buffer.\n");
         }
      }

      if (dec->ctx.res)
         dec->msg->body.decode.dpb_reserved = dec->ctx.res->buf->size;
      break;

   case PIPE_VIDEO_FORMAT_VC1:
      dec->msg->body.decode.codec.vc1 = get_vc1_msg((struct pipe_vc1_picture_desc *)picture);
      break;

   case PIPE_VIDEO_FORMAT_MPEG12:
      dec->msg->body.decode.codec.mpeg2 =
         get_mpeg2_msg(dec, (struct pipe_mpeg12_picture_desc *)picture);
      break;

   case PIPE_VIDEO_FORMAT_MPEG4:
      dec->msg->body.decode.codec.mpeg4 =
         get_mpeg4_msg(dec, (struct pipe_mpeg4_picture_desc *)picture);
      break;

   case PIPE_VIDEO_FORMAT_JPEG:
      break;

   default:
      assert(0);
      return 1;
   }

   dec->msg->body.decode.db_surf_tile_config = dec->msg->body.decode.dt_surf_tile_config;
   dec->msg->body.decode.extension_support = 0x1;

   /* set at least the feedback buffer size */
   dec->fb[0] = dec->fb_size;

   send_msg_buf(dec);

   if (dec->dpb.res)
      send_cmd(dec, RUVD_CMD_DPB_BUFFER, dec->dpb.res->buf, 0, RADEON_USAGE_READWRITE,
               RADEON_DOMAIN_VRAM);

   if (dec->ctx.res)
      send_cmd(dec, RUVD_CMD_CONTEXT_BUFFER, dec->ctx.res->buf, 0, RADEON_USAGE_READWRITE,
               RADEON_DOMAIN_VRAM);
   send_cmd(dec, RUVD_CMD_BITSTREAM_BUFFER, bs_buf->res->buf, 0, RADEON_USAGE_READ,
            RADEON_DOMAIN_GTT);
   send_cmd(dec, RUVD_CMD_DECODING_TARGET_BUFFER, dt, 0, RADEON_USAGE_WRITE, RADEON_DOMAIN_VRAM);
   send_cmd(dec, RUVD_CMD_FEEDBACK_BUFFER, msg_fb_it_buf->res->buf, FB_BUFFER_OFFSET,
            RADEON_USAGE_WRITE, RADEON_DOMAIN_GTT);
   if (have_it(dec))
      send_cmd(dec, RUVD_CMD_ITSCALING_TABLE_BUFFER, msg_fb_it_buf->res->buf,
               FB_BUFFER_OFFSET + dec->fb_size, RADEON_USAGE_READ, RADEON_DOMAIN_GTT);
   set_reg(dec, dec->reg.cntl, 1);

   flush(dec, picture->flush_flags, picture->out_fence);
   next_buffer(dec);
   return 0;
}


/**
 * flush any outstanding command buffers to the hardware
 */
static void ruvd_flush(struct pipe_video_codec *decoder)
{
}

/**
 * create and UVD decoder
 */
struct pipe_video_codec *si_common_uvd_create_decoder(struct pipe_context *context,
                                                      const struct pipe_video_codec *templ,
                                                      ruvd_set_dtb set_dtb)
{
   struct si_context *sctx = (struct si_context *)context;
   struct radeon_winsys *ws = sctx->ws;
   unsigned dpb_size;
   unsigned width = templ->width, height = templ->height;
   unsigned bs_buf_size;
   struct ruvd_decoder *dec;
   int r, i;

   switch (u_reduce_video_profile(templ->profile)) {
   case PIPE_VIDEO_FORMAT_MPEG12:
   case PIPE_VIDEO_FORMAT_MPEG4:
      width = align(width, VL_MACROBLOCK_WIDTH);
      height = align(height, VL_MACROBLOCK_HEIGHT);
      break;
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      width = align(width, VL_MACROBLOCK_WIDTH);
      height = align(height, VL_MACROBLOCK_HEIGHT);
      break;

   default:
      break;
   }

   dec = CALLOC_STRUCT(ruvd_decoder);

   if (UNLIKELY(!dec))
      return NULL;

   if (!sctx->screen->info.is_amdgpu)
      dec->use_legacy = true;

   dec->base = *templ;
   dec->base.context = context;
   dec->base.width = width;
   dec->base.height = height;

   dec->base.destroy = ruvd_destroy;
   dec->base.begin_frame = ruvd_begin_frame;
   dec->base.decode_macroblock = ruvd_decode_macroblock;
   dec->base.decode_bitstream = ruvd_decode_bitstream;
   dec->base.end_frame = ruvd_end_frame;
   dec->base.flush = ruvd_flush;
   dec->base.fence_wait = ruvd_dec_fence_wait;
   dec->base.destroy_fence = ruvd_dec_destroy_fence;

   dec->stream_type = profile2stream_type(dec, sctx->family);
   dec->set_dtb = set_dtb;
   dec->stream_handle = si_vid_alloc_stream_handle();
   dec->screen = context->screen;
   dec->ws = ws;

   if (UNLIKELY(!ws->cs_create(&dec->cs, sctx->ctx, AMD_IP_UVD, NULL, NULL))) {
      RVID_ERR("Can't get command submission context.\n");
      goto error;
   }

   for (i = 0; i < 16; i++)
      dec->render_pic_list[i] = NULL;
   dec->fb_size = (sctx->family == CHIP_TONGA) ? FB_BUFFER_SIZE_TONGA : FB_BUFFER_SIZE;
   bs_buf_size = align(width * height / 32, 128);
   for (i = 0; i < NUM_BUFFERS; ++i) {
      unsigned msg_fb_it_size = FB_BUFFER_OFFSET + dec->fb_size;
      STATIC_ASSERT(sizeof(struct ruvd_msg) <= FB_BUFFER_OFFSET);
      if (have_it(dec))
         msg_fb_it_size += IT_SCALING_TABLE_SIZE;
      if (UNLIKELY(!si_vid_create_buffer(dec->screen, &dec->msg_fb_it_buffers[i], msg_fb_it_size,
                                PIPE_USAGE_STAGING))) {
         RVID_ERR("Can't allocated message buffers.\n");
         goto error;
      }

      if (UNLIKELY(!si_vid_create_buffer(dec->screen, &dec->bs_buffers[i], bs_buf_size,
                                PIPE_USAGE_STAGING))) {
         RVID_ERR("Can't allocated bitstream buffers.\n");
         goto error;
      }
   }

   dpb_size = calc_dpb_size(dec);
   if (dpb_size) {
      if (UNLIKELY(!si_vid_create_buffer(dec->screen, &dec->dpb, dpb_size, PIPE_USAGE_DEFAULT))) {
         RVID_ERR("Can't allocated dpb.\n");
         goto error;
      }
   }

   if (dec->stream_type == RUVD_CODEC_H264_PERF && sctx->family >= CHIP_POLARIS10) {
      unsigned ctx_size = calc_ctx_size_h264_perf(dec);
      if (UNLIKELY(
            !si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT))) {
         RVID_ERR("Can't allocated context buffer.\n");
         goto error;
      }
   }

   if (sctx->family >= CHIP_POLARIS10) {
      if (UNLIKELY(!si_vid_create_buffer(dec->screen, &dec->sessionctx, UVD_SESSION_CONTEXT_SIZE,
                                PIPE_USAGE_DEFAULT))) {
         RVID_ERR("Can't allocated session ctx.\n");
         goto error;
      }
   }

   if (sctx->family >= CHIP_VEGA10) {
      dec->reg.data0 = RUVD_GPCOM_VCPU_DATA0_SOC15;
      dec->reg.data1 = RUVD_GPCOM_VCPU_DATA1_SOC15;
      dec->reg.cmd = RUVD_GPCOM_VCPU_CMD_SOC15;
      dec->reg.cntl = RUVD_ENGINE_CNTL_SOC15;
   } else {
      dec->reg.data0 = RUVD_GPCOM_VCPU_DATA0;
      dec->reg.data1 = RUVD_GPCOM_VCPU_DATA1;
      dec->reg.cmd = RUVD_GPCOM_VCPU_CMD;
      dec->reg.cntl = RUVD_ENGINE_CNTL;
   }

   map_msg_fb_it_buf(dec);
   if (UNLIKELY(!dec->msg))
      goto error;

   dec->msg->size = sizeof(*dec->msg);
   dec->msg->msg_type = RUVD_MSG_CREATE;
   dec->msg->stream_handle = dec->stream_handle;
   dec->msg->body.create.stream_type = dec->stream_type;
   dec->msg->body.create.width_in_samples = dec->base.width;
   dec->msg->body.create.height_in_samples = dec->base.height;
   dec->msg->body.create.dpb_size = dpb_size;
   send_msg_buf(dec);
   r = flush(dec, 0, NULL);
   if (UNLIKELY(r))
      goto error;

   next_buffer(dec);

   return &dec->base;

error:
   __attribute__((cold));
   dec->ws->cs_destroy(&dec->cs);

   for (i = 0; i < NUM_BUFFERS; ++i) {
      si_vid_destroy_buffer(&dec->msg_fb_it_buffers[i]);
      si_vid_destroy_buffer(&dec->bs_buffers[i]);
   }

   si_vid_destroy_buffer(&dec->dpb);
   si_vid_destroy_buffer(&dec->ctx);
   si_vid_destroy_buffer(&dec->sessionctx);

   FREE(dec);

   return NULL;
}


/* calculate top/bottom offset */
static unsigned texture_offset(struct radeon_surf *surface, unsigned layer,
                               enum ruvd_surface_type type)
{
   switch (type) {
   default:
   case RUVD_SURFACE_TYPE_LEGACY:
      return (uint64_t)surface->u.legacy.level[0].offset_256B * 256 +
             layer * (uint64_t)surface->u.legacy.level[0].slice_size_dw * 4;
      break;
   case RUVD_SURFACE_TYPE_GFX9:
      return surface->u.gfx9.surf_offset + layer * surface->u.gfx9.surf_slice_size;
      break;
   }
}

/* hw encode the aspect of macro tiles */
static inline unsigned macro_tile_aspect(unsigned val)
{
   /* __builtin_ctz(0) is undefined, so guard against it. */
   return val ? __builtin_ctz(val) : 0;
}

/* hw encode the bank width and height */
static inline unsigned bank_wh(unsigned val)
{
   /* __builtin_ctz(0) is undefined, so guard against it. */
   return val ? __builtin_ctz(val) : 0;
}

/**
 * fill decoding target field from the luma and chroma surfaces
 */
void si_uvd_set_dt_surfaces(struct ruvd_msg *msg, struct radeon_surf *luma,
                            struct radeon_surf *chroma, enum ruvd_surface_type type)
{
   switch (type) {
   default:
   case RUVD_SURFACE_TYPE_LEGACY:
      msg->body.decode.dt_pitch = luma->u.legacy.level[0].nblk_x * luma->blk_w;
      switch (luma->u.legacy.level[0].mode) {
      case RADEON_SURF_MODE_LINEAR_ALIGNED:
         msg->body.decode.dt_tiling_mode = RUVD_TILE_LINEAR;
         msg->body.decode.dt_array_mode = RUVD_ARRAY_MODE_LINEAR;
         break;
      case RADEON_SURF_MODE_1D:
         msg->body.decode.dt_tiling_mode = RUVD_TILE_8X8;
         msg->body.decode.dt_array_mode = RUVD_ARRAY_MODE_1D_THIN;
         break;
      case RADEON_SURF_MODE_2D:
         msg->body.decode.dt_tiling_mode = RUVD_TILE_8X8;
         msg->body.decode.dt_array_mode = RUVD_ARRAY_MODE_2D_THIN;
         break;
      default:
         assert(0);
         break;
      }

      msg->body.decode.dt_luma_top_offset = texture_offset(luma, 0, type);
      if (chroma)
         msg->body.decode.dt_chroma_top_offset = texture_offset(chroma, 0, type);
      if (msg->body.decode.dt_field_mode) {
         msg->body.decode.dt_luma_bottom_offset = texture_offset(luma, 1, type);
         if (chroma)
            msg->body.decode.dt_chroma_bottom_offset = texture_offset(chroma, 1, type);
      } else {
         msg->body.decode.dt_luma_bottom_offset = msg->body.decode.dt_luma_top_offset;
         msg->body.decode.dt_chroma_bottom_offset = msg->body.decode.dt_chroma_top_offset;
      }

      if (chroma) {
         assert(luma->u.legacy.bankw == chroma->u.legacy.bankw);
         assert(luma->u.legacy.bankh == chroma->u.legacy.bankh);
         assert(luma->u.legacy.mtilea == chroma->u.legacy.mtilea);
      }

      msg->body.decode.dt_surf_tile_config |= RUVD_BANK_WIDTH(bank_wh(luma->u.legacy.bankw));
      msg->body.decode.dt_surf_tile_config |= RUVD_BANK_HEIGHT(bank_wh(luma->u.legacy.bankh));
      msg->body.decode.dt_surf_tile_config |=
         RUVD_MACRO_TILE_ASPECT_RATIO(macro_tile_aspect(luma->u.legacy.mtilea));
      break;
   case RUVD_SURFACE_TYPE_GFX9:
      msg->body.decode.dt_pitch = luma->u.gfx9.surf_pitch * luma->blk_w;
      msg->body.decode.dt_wa_chroma_bottom_offset = luma->u.gfx9.swizzle_mode;
      msg->body.decode.dt_luma_top_offset = texture_offset(luma, 0, type);
      msg->body.decode.dt_chroma_top_offset = texture_offset(chroma, 0, type);
      if (msg->body.decode.dt_field_mode) {
         msg->body.decode.dt_luma_bottom_offset = texture_offset(luma, 1, type);
         msg->body.decode.dt_chroma_bottom_offset = texture_offset(chroma, 1, type);
      } else {
         msg->body.decode.dt_luma_bottom_offset = msg->body.decode.dt_luma_top_offset;
         msg->body.decode.dt_chroma_bottom_offset = msg->body.decode.dt_chroma_top_offset;
      }
      msg->body.decode.dt_surf_tile_config = 0;
      break;
   }
}
