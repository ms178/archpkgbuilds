/*
 * Copyright © 2025 Advanced Micro Devices, Inc.
 * SPDX-License-Identifier: MIT
 */

#include "pipe/p_video_codec.h"

#include "util/vl_vlc.h"
#include "util/u_video.h"

#include "va_private.h"

#include <limits.h>
#include <string.h>

static VAStatus
handlePictureParameterBuffer(vlVaDriver *drv, vlVaContext *context, vlVaBuffer *buf)
{
   VAStatus va_status;
   enum pipe_video_format format;

   if (!drv || !context || !buf)
      return VA_STATUS_ERROR_INVALID_CONTEXT;

   va_status = VA_STATUS_SUCCESS;
   format = u_reduce_video_profile(context->templat.profile);

   switch (format) {
   case PIPE_VIDEO_FORMAT_MPEG12:
      vlVaHandlePictureParameterBufferMPEG12(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      vlVaHandlePictureParameterBufferH264(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_VC1:
      vlVaHandlePictureParameterBufferVC1(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_HEVC:
      vlVaHandlePictureParameterBufferHEVC(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_JPEG:
      vlVaHandlePictureParameterBufferMJPEG(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_VP9:
      vlVaHandlePictureParameterBufferVP9(drv, context, buf);
      break;
   case PIPE_VIDEO_FORMAT_AV1:
      va_status = vlVaHandlePictureParameterBufferAV1(drv, context, buf);
      break;
   default:
      break;
   }

   /* Create the decoder once max_references is known. */
   if (!context->decoder) {
      if (!context->target)
         return VA_STATUS_ERROR_INVALID_CONTEXT;

      mtx_lock(&context->mutex);

      if (!context->decoder) {
         if (format == PIPE_VIDEO_FORMAT_MPEG4_AVC) {
            context->templat.level = u_get_h264_level(context->templat.width,
                                                      context->templat.height,
                                                      &context->templat.max_references);
         }

         context->decoder = drv->pipe->create_video_codec(drv->pipe, &context->templat);
         if (context->decoder)
            context->needs_begin_frame = true;
      }

      mtx_unlock(&context->mutex);

      if (!context->decoder)
         return VA_STATUS_ERROR_ALLOCATION_FAILED;
   }

   if (format == PIPE_VIDEO_FORMAT_VP9) {
      mtx_lock(&context->mutex);
      if (context->decoder) {
         context->decoder->width = context->desc.vp9.picture_parameter.frame_width;
         context->decoder->height = context->desc.vp9.picture_parameter.frame_height;
      }
      mtx_unlock(&context->mutex);
   }

   return va_status;
}

static void
handleIQMatrixBuffer(vlVaContext *context, vlVaBuffer *buf)
{
   if (!context || !buf)
      return;

   switch (u_reduce_video_profile(context->templat.profile)) {
   case PIPE_VIDEO_FORMAT_MPEG12:
      vlVaHandleIQMatrixBufferMPEG12(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      vlVaHandleIQMatrixBufferH264(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_HEVC:
      vlVaHandleIQMatrixBufferHEVC(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_JPEG:
      vlVaHandleIQMatrixBufferMJPEG(context, buf);
      break;
   default:
      break;
   }
}

static void
handleSliceParameterBuffer(vlVaContext *context, vlVaBuffer *buf)
{
   if (!context || !buf)
      return;

   switch (u_reduce_video_profile(context->templat.profile)) {
   case PIPE_VIDEO_FORMAT_MPEG12:
      vlVaHandleSliceParameterBufferMPEG12(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_VC1:
      vlVaHandleSliceParameterBufferVC1(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      vlVaHandleSliceParameterBufferH264(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_HEVC:
      vlVaHandleSliceParameterBufferHEVC(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_JPEG:
      vlVaHandleSliceParameterBufferMJPEG(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_VP9:
      vlVaHandleSliceParameterBufferVP9(context, buf);
      break;
   case PIPE_VIDEO_FORMAT_AV1:
      vlVaHandleSliceParameterBufferAV1(context, buf);
      break;
   default:
      break;
   }
}

static unsigned int
bufHasStartcode(const vlVaBuffer *buf, unsigned int code, unsigned int bits)
{
   struct vl_vlc vlc = {0};
   int i;

   if (!buf || !buf->data || buf->size == 0 || bits == 0 || bits > 32)
      return 0;

   /* Search the first 64 bytes for a start code. */
   vl_vlc_init(&vlc, 1, (const void * const *)&buf->data, &buf->size);

   for (i = 0; i < 64 && vl_vlc_bits_left(&vlc) >= bits; ++i) {
      if (vl_vlc_peekbits(&vlc, bits) == code)
         return 1;
      vl_vlc_eatbits(&vlc, 8);
      vl_vlc_fillbits(&vlc);
   }

   return 0;
}

static VAStatus
handleVAProtectedSliceDataBufferType(vlVaContext *context, vlVaBuffer *buf)
{
   static const uint8_t cookie[] = {'w', 'v', 'c', 'e', 'n', 'c', 's', 'b'};
   uint8_t *drm_key;
   const uint8_t *encrypted_data;
   unsigned int drm_key_size;

   if (!context || !buf)
      return VA_STATUS_ERROR_INVALID_CONTEXT;

   if (!context->desc.base.protected_playback)
      return VA_STATUS_ERROR_INVALID_CONTEXT;

   if (!buf->data && buf->size != 0)
      return VA_STATUS_ERROR_INVALID_BUFFER;

   if (buf->size > UINT_MAX)
      return VA_STATUS_ERROR_INVALID_BUFFER;

   encrypted_data = (const uint8_t *)buf->data;
   drm_key_size = (unsigned int)buf->size;

   drm_key = REALLOC(context->desc.base.decrypt_key,
                     context->desc.base.key_size,
                     drm_key_size);
   if (!drm_key)
      return VA_STATUS_ERROR_ALLOCATION_FAILED;

   context->desc.base.decrypt_key = drm_key;
   if (drm_key_size > 0)
      memcpy(context->desc.base.decrypt_key, encrypted_data, drm_key_size);
   context->desc.base.key_size = drm_key_size;

   /* Always assign cenc to avoid stale state across buffers. */
   context->desc.base.cenc =
      drm_key_size >= sizeof(cookie) &&
      memcmp(encrypted_data, cookie, sizeof(cookie)) == 0;

   return VA_STATUS_SUCCESS;
}

static inline void
vlVaAddSliceDataBuffer(vlVaContext *context, const void *data, unsigned size)
{
   util_dynarray_append(&context->bs.buffers, data);
   util_dynarray_append(&context->bs.sizes, size);
}

static VAStatus
handleVASliceDataBufferType(vlVaContext *context, vlVaBuffer *buf)
{
   enum pipe_video_format format = u_reduce_video_profile(context->templat.profile);
   static const uint8_t start_code_h264[] = { 0x00, 0x00, 0x01 };
   static const uint8_t start_code_h265[] = { 0x00, 0x00, 0x01 };
   static const uint8_t start_code_vc1_frame[] = { 0x00, 0x00, 0x01, 0x0d };
   static const uint8_t start_code_vc1_field[] = { 0x00, 0x00, 0x01, 0x0c };
   static const uint8_t start_code_vc1_slice[] = { 0x00, 0x00, 0x01, 0x0b };
   static const uint8_t eoi_jpeg[] = { 0xff, 0xd9 };

   if (!context->decoder)
      return VA_STATUS_ERROR_INVALID_CONTEXT;

   if (!context->desc.base.protected_playback) {
      switch (format) {
      case PIPE_VIDEO_FORMAT_MPEG4_AVC:
         if (bufHasStartcode(buf, 0x000001, 24))
            break;

         vlVaAddSliceDataBuffer(context, start_code_h264, sizeof(start_code_h264));
         break;

      case PIPE_VIDEO_FORMAT_HEVC:
         if (!bufHasStartcode(buf, 0x000001, 24))
            vlVaAddSliceDataBuffer(context, start_code_h265, sizeof(start_code_h265));

         vlVaDecoderHEVCBitstreamHeader(context, buf);
         break;

      case PIPE_VIDEO_FORMAT_VC1:
         if (bufHasStartcode(buf, 0x000001, 24))
            break;

         if (context->decoder->profile == PIPE_VIDEO_PROFILE_VC1_ADVANCED) {
            const uint8_t *start_code;
            if (context->slice_data_offset)
               start_code = start_code_vc1_slice;
            else if (context->desc.vc1.is_first_field)
               start_code = start_code_vc1_frame;
            else
               start_code = start_code_vc1_field;

            vlVaAddSliceDataBuffer(context, start_code, sizeof(start_code_vc1_frame));
         }
         break;

      case PIPE_VIDEO_FORMAT_JPEG:
         if (bufHasStartcode(buf, 0xffd8ffdb, 32))
            break;

         vlVaGetJpegSliceHeader(context);
         if (context->mjpeg.slice_header_size > 0) {
            vlVaAddSliceDataBuffer(context,
                                   context->mjpeg.slice_header,
                                   context->mjpeg.slice_header_size);
         }
         break;

      case PIPE_VIDEO_FORMAT_VP9:
         vlVaDecoderVP9BitstreamHeader(context, buf);
         break;

      case PIPE_VIDEO_FORMAT_AV1:
      default:
         break;
      }
   }

   vlVaAddSliceDataBuffer(context, buf->data, buf->size);

   if (format == PIPE_VIDEO_FORMAT_JPEG)
      vlVaAddSliceDataBuffer(context, eoi_jpeg, sizeof(eoi_jpeg));

   if (context->needs_begin_frame) {
      context->decoder->begin_frame(context->decoder, context->target, &context->desc.base);
      context->needs_begin_frame = false;
   }

   return VA_STATUS_SUCCESS;
}

VAStatus
vlVaHandleDecBufferType(vlVaDriver *drv, vlVaContext *context, vlVaBuffer *buf)
{
   VAStatus va_status = VA_STATUS_SUCCESS;

   if (!drv || !context || !buf)
      return VA_STATUS_ERROR_INVALID_CONTEXT;

   switch (buf->type) {
   case VAPictureParameterBufferType:
      va_status = handlePictureParameterBuffer(drv, context, buf);
      break;

   case VAIQMatrixBufferType:
      handleIQMatrixBuffer(context, buf);
      break;

   case VASliceParameterBufferType:
      handleSliceParameterBuffer(context, buf);
      context->have_slice_params = true;
      break;

   case VASliceDataBufferType:
      va_status = handleVASliceDataBufferType(context, buf);
      /* Workaround for apps sending single slice data buffer followed
       * by multiple slice parameter buffers.
       */
      if (context->have_slice_params) {
         if (buf->size > (unsigned)(UINT_MAX - context->slice_data_offset))
            context->slice_data_offset = UINT_MAX;
         else
            context->slice_data_offset += buf->size;
      }
      break;

   case VAHuffmanTableBufferType:
      vlVaHandleHuffmanTableBufferType(context, buf);
      break;

   case VAProtectedSliceDataBufferType:
      va_status = handleVAProtectedSliceDataBufferType(context, buf);
      break;

   default:
      break;
   }

   return va_status;
}
