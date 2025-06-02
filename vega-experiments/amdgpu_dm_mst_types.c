// SPDX-License-Identifier: MIT
// Copyright 2012-2023 Advanced Micro Devices, Inc.

#include <linux/vmalloc.h>
#include <linux/slab.h>
#include <linux/delay.h>
#include <linux/math64.h>
#include <linux/compiler.h>
#include <linux/kernel.h>
#include "core_types.h"

#include <drm/display/drm_dp_helper.h>
#include <drm/display/drm_dp_mst_helper.h>
#include <drm/drm_atomic.h>
#include <drm/drm_atomic_helper.h>
#include <drm/drm_fixed.h>
#include <drm/drm_edid.h>
#include <drm/drm_print.h>
#include <drm/drm_connector.h>

#include "dm_services.h"
#include "amdgpu.h"
#include "amdgpu_dm.h"
#include "amdgpu_dm_mst_types.h"
#include "amdgpu_dm_hdcp.h"

#include "dc.h"
#include "dm_helpers.h"

#include "ddc_service_types.h"
#include "dpcd_defs.h"

#include "dmub_cmd.h"
#if defined(CONFIG_DEBUG_FS)
#include "amdgpu_dm_debugfs.h"
#endif

#ifndef DC_CONNECTION_TYPE_MST_BRANCH
#define DC_CONNECTION_TYPE_MST_BRANCH	dc_connection_mst_branch
#endif

#ifndef DP_TOTAL_TIMESLOTS
#define DP_TOTAL_TIMESLOTS	63
#endif

#ifndef AMDGPU_DM_MST_CHECK_SLOTS_WRAPPER
#define AMDGPU_DM_MST_CHECK_SLOTS_WRAPPER
static inline int
drm_dp_mst_atomic_check_slots(struct drm_atomic_state *state,
							  struct drm_dp_mst_topology_mgr *mgr)
{
	struct drm_dp_mst_topology_state *mst_state;

	mst_state = drm_atomic_get_mst_topology_state(state, mgr);
	if (IS_ERR(mst_state))
		return PTR_ERR(mst_state);

	return drm_dp_mst_atomic_check_mgr(state, mgr, mst_state, NULL);
}
#endif

#define PEAK_FACTOR_X1000 1006
#define MAX_DPCD_ACCESS_RETRIES				3
#define MAX_MST_SIDEBAND_MSG_PROCESS_COUNT		30
#define DP_MST_MAX_DPCD_TRANSACTION_BYTES_PER_MANAGER	16
#define DP_MST_MAX_PAYLOADS_PER_MANAGER			4

#ifndef AMDGPU_DM_MAX_NUM_LINKS
#define AMDGPU_DM_MAX_NUM_LINKS			6
#endif
#ifndef AMDGPU_DM_MAX_CONNECTORS_PER_LINK
#define AMDGPU_DM_MAX_CONNECTORS_PER_LINK	4
#endif
#ifndef MST_ESI_ACK_BUFFER_MAX_LEN
#define MST_ESI_ACK_BUFFER_MAX_LEN		4
#endif

struct dsc_mst_fairness_params {
	struct dc_crtc_timing	*timing;
	struct dc_sink		*sink;
	struct dc_dsc_bw_range	 bw_range;
	bool			 compression_possible;
	struct drm_dp_mst_port	*port;
	enum dsc_clock_force_state clock_force_enable;
	u32			 num_slices_h;
	u32			 num_slices_v;
	u32			 bpp_overwrite;
	struct amdgpu_dm_connector *aconnector;
};

static const s8 aux_ret_to_errno_map[] = {
	[AUX_RET_SUCCESS]                 = 0,
	[AUX_RET_ERROR_HPD_DISCON]        = -EIO,
	[AUX_RET_ERROR_UNKNOWN]           = -EIO,
	[AUX_RET_ERROR_INVALID_OPERATION] = -EIO,
	[AUX_RET_ERROR_PROTOCOL_ERROR]    = -EIO,
	[AUX_RET_ERROR_INVALID_REPLY]     = -EBUSY,
	[AUX_RET_ERROR_ENGINE_ACQUIRE]    = -EBUSY,
	[AUX_RET_ERROR_TIMEOUT]           = -ETIMEDOUT,
};

static bool is_dsc_precompute_needed(struct drm_atomic_state *state)
{
	int i;
	struct drm_crtc *crtc;
	struct drm_crtc_state *old_st, *new_st;

	if (unlikely(!state))
		return false;

	for_each_oldnew_crtc_in_state(state, crtc, old_st, new_st, i) {
		struct dm_crtc_state *dm = to_dm_crtc_state(new_st);

		if (dm && dm->stream && dm->stream->link &&
			dm->stream->link->type == DC_CONNECTION_TYPE_MST_BRANCH)
			return true;
	}
	return false;
}

static noinline __cold ssize_t
dm_dp_aux_transfer_error_handler(enum aux_return_code_type dc_op_status,
								 ssize_t dc_bytes_io,
								 const struct drm_dp_aux_msg *__restrict msg,
								 bool is_write_op,
								 const struct drm_dp_aux *__restrict aux,
								 const struct amdgpu_device *adev)
{
	ssize_t final_errno = dc_bytes_io;

	if (dc_bytes_io >= 0 && dc_op_status != AUX_RET_SUCCESS) {
		if (dc_op_status >= 0 &&
			dc_op_status < (int)ARRAY_SIZE(aux_ret_to_errno_map) &&
			aux_ret_to_errno_map[dc_op_status])
			final_errno = aux_ret_to_errno_map[dc_op_status];
		else
			final_errno = -EIO;
	}

	if (dc_bytes_io >= 0 && dc_op_status == AUX_RET_SUCCESS) {
		if (!(msg->reply == DP_AUX_NATIVE_REPLY_ACK ||
			msg->reply == DP_AUX_I2C_REPLY_ACK)) {
			switch (msg->reply & (DP_AUX_NATIVE_REPLY_MASK | DP_AUX_I2C_REPLY_MASK)) {
				case DP_AUX_NATIVE_REPLY_NACK:
				case DP_AUX_I2C_REPLY_NACK:
					final_errno = -EIO;
					break;
				case DP_AUX_NATIVE_REPLY_DEFER:
				case DP_AUX_I2C_REPLY_DEFER:
					final_errno = -EAGAIN;
					break;
				default:
					final_errno = -EIO;
					break;
			}
			}
	}

	if (final_errno >= 0)
		final_errno = -EIO;

	if (drm_debug_enabled(DRM_UT_DP))
		drm_dbg_dp(adev_to_drm((struct amdgpu_device *)adev),
				   "AUX-xfer %s (%s 0x%05x, len %zu): dc_status=%d dc_io=%zd reply=0x%02x -> errno=%zd\n",
				   is_write_op ? "WRITE" : "READ", aux->name, msg->address,
			 msg->size, dc_op_status, dc_bytes_io, msg->reply, final_errno);

		return final_errno;
}

static ssize_t dm_dp_aux_transfer(struct drm_dp_aux *aux,
								  struct drm_dp_aux_msg *msg)
{
	struct amdgpu_dm_dp_aux *dm_aux;
	struct ddc_service *ddc;
	struct amdgpu_device *adev;
	struct aux_payload payload = { 0 };
	u8 write_buf_copy[DP_AUX_MAX_PAYLOAD_BYTES];
	enum aux_return_code_type dc_op_status;
	ssize_t dc_bytes_io;
	bool is_write_op;

	if (unlikely(!aux || !msg))
		return -EINVAL;

	dm_aux = TO_DM_AUX(aux);
	ddc    = dm_aux->ddc_service;

	if (unlikely(!ddc || !ddc->ctx || !ddc->ctx->driver_context))
		return -EINVAL;

	adev = ddc->ctx->driver_context;

	if (unlikely(msg->size > DP_AUX_MAX_PAYLOAD_BYTES))
		return -E2BIG;

	is_write_op = !(msg->request & DP_AUX_I2C_READ);
	payload.address = msg->address;
	payload.length = msg->size;
	payload.reply = &msg->reply;
	payload.i2c_over_aux = !(msg->request & DP_AUX_NATIVE_WRITE);
	payload.write = is_write_op;
	payload.mot = !!(msg->request & DP_AUX_I2C_MOT);
	payload.write_status_update = !!(msg->request & DP_AUX_I2C_WRITE_STATUS_UPDATE);
	payload.data = msg->buffer;

	if (is_write_op && msg->buffer) {
		memcpy(write_buf_copy, msg->buffer, msg->size);
		payload.data = write_buf_copy;
	}

	dc_bytes_io = dc_link_aux_transfer_raw(ddc, &payload, &dc_op_status);

	if (unlikely(adev->dm.aux_hpd_discon_quirk &&
		msg->address == DP_SIDEBAND_MSG_DOWN_REQ_BASE &&
		dc_op_status == AUX_RET_ERROR_HPD_DISCON)) {
		dc_bytes_io  = msg->size;
	dc_op_status = AUX_RET_SUCCESS;
	msg->reply   = DP_AUX_NATIVE_REPLY_ACK;
		}

		if (likely(dc_bytes_io >= 0 &&
			dc_op_status == AUX_RET_SUCCESS &&
			(msg->reply == DP_AUX_NATIVE_REPLY_ACK ||
			msg->reply == DP_AUX_I2C_REPLY_ACK))) {
			return is_write_op ? (ssize_t)msg->size : dc_bytes_io;
			}

			return dm_dp_aux_transfer_error_handler(dc_op_status, dc_bytes_io, msg,
													is_write_op, aux, adev);
}

static void
dm_dp_mst_connector_destroy(struct drm_connector *connector)
{
	struct amdgpu_dm_connector *aconnector;

	if (unlikely(!connector))
		return;

	aconnector = to_amdgpu_dm_connector(connector);

	if (aconnector->dc_sink) {
		if (likely(aconnector->dc_link &&
			aconnector->dc_link->sink_count > 0)) {
			dc_link_remove_remote_sink(aconnector->dc_link,
									   aconnector->dc_sink);
			}
			dc_sink_release(aconnector->dc_sink);
		aconnector->dc_sink = NULL;
	}

	drm_edid_free(aconnector->drm_edid);
	aconnector->drm_edid = NULL;

	drm_connector_cleanup(connector);

	if (aconnector->mst_output_port)
		drm_dp_mst_put_port_malloc(aconnector->mst_output_port);

	kfree(aconnector->dm_dp_aux.aux.name);

	kfree(aconnector);
}

static int
amdgpu_dm_mst_connector_late_register(struct drm_connector *connector)
{
	struct amdgpu_dm_connector *amd_connector;
	int ret;

	if (unlikely(!connector))
		return -EINVAL;
	amd_connector = to_amdgpu_dm_connector(connector);

	if (unlikely(!amd_connector->mst_output_port)) {
		DRM_ERROR("%s: Connector %s has no MST output port.\n", __func__, connector->name);
		return -EINVAL;
	}

	ret = drm_dp_mst_connector_late_register(connector, amd_connector->mst_output_port);
	if (unlikely(ret < 0))
		return ret;

	#if defined(CONFIG_DEBUG_FS)
	connector_debugfs_init(amd_connector);
	#endif
	return 0;
}

static __always_inline void
amdgpu_dm_mst_reset_mst_connector_setting(struct amdgpu_dm_connector *aconnector)
{
	if (unlikely(!aconnector))
		return;

	drm_edid_free(aconnector->drm_edid);
	aconnector->drm_edid = NULL;
	aconnector->dsc_aux = NULL;

	if (aconnector->mst_output_port)
		aconnector->mst_output_port->passthrough_aux = NULL;

	aconnector->mst_local_bw = 0;
	aconnector->vc_full_pbn = 0;
}

static void
amdgpu_dm_mst_connector_early_unregister(struct drm_connector *connector)
{
	struct amdgpu_dm_connector *aconnector;
	struct drm_dp_mst_port *port;
	struct amdgpu_dm_connector *root_aconnector;

	if (unlikely(!connector))
		return;
	aconnector = to_amdgpu_dm_connector(connector);
	port = aconnector->mst_output_port;
	root_aconnector = aconnector->mst_root;

	drm_dp_mst_connector_early_unregister(connector, port);

	if (unlikely(!port || !root_aconnector)) {
		if (drm_debug_enabled(DRM_UT_DP))
			drm_dbg_dp(connector->dev,
					   "%s: Connector %s missing full MST context, partial unregister.\n",
			  __func__, connector->name);
			return;
	}

	drm_modeset_lock(&root_aconnector->mst_mgr.base.lock, NULL);
	if (aconnector->dc_sink) {
		struct dc_link *dc_link = aconnector->dc_link;

		if (likely(dc_link && dc_link->sink_count > 0)) {
			dc_link_remove_remote_sink(dc_link, aconnector->dc_sink);
			if (drm_debug_enabled(DRM_UT_DP))
				drm_dbg_dp(connector->dev,
						   "DM_MST: Removed remote sink %ps for %s, %d left.\n",
			   aconnector->dc_sink, connector->name, dc_link->sink_count);
		}
		dc_sink_release(aconnector->dc_sink);
		aconnector->dc_sink = NULL;
		amdgpu_dm_mst_reset_mst_connector_setting(aconnector);
	}
	aconnector->mst_status = MST_STATUS_DEFAULT;
	drm_modeset_unlock(&root_aconnector->mst_mgr.base.lock);
}

static const struct drm_connector_funcs dm_dp_mst_connector_funcs = {
	.fill_modes = drm_helper_probe_single_connector_modes,
	.destroy = dm_dp_mst_connector_destroy,
	.reset = amdgpu_dm_connector_funcs_reset,
	.atomic_duplicate_state = amdgpu_dm_connector_atomic_duplicate_state,
	.atomic_destroy_state = drm_atomic_helper_connector_destroy_state,
	.atomic_set_property = amdgpu_dm_connector_atomic_set_property,
	.atomic_get_property = amdgpu_dm_connector_atomic_get_property,
	.late_register = amdgpu_dm_mst_connector_late_register,
	.early_unregister = amdgpu_dm_mst_connector_early_unregister,
};

bool needs_dsc_aux_workaround(struct dc_link *link)
{
	if (unlikely(!link))
		return false;

	if (link->dpcd_caps.branch_dev_id == DP_BRANCH_DEVICE_ID_90CC24 &&
		(link->dpcd_caps.dpcd_rev.raw == DPCD_REV_14 ||
		link->dpcd_caps.dpcd_rev.raw == DPCD_REV_12) &&
		link->dpcd_caps.sink_count.bits.SINK_COUNT >= 2)
		return true;
	return false;
}

#if defined(CONFIG_DRM_AMD_DC_FP)
static bool is_synaptics_cascaded_panamera(struct dc_link *link, struct drm_dp_mst_port *port)
{
	u8 branch_vendor_data[4] = {0};
	ssize_t read_len;

	if (unlikely(!link || !port || !port->mgr || !port->mgr->aux))
		return false;

	read_len = drm_dp_dpcd_read(port->mgr->aux, DP_BRANCH_VENDOR_SPECIFIC_START,
								branch_vendor_data, sizeof(branch_vendor_data));

	if (read_len == sizeof(branch_vendor_data)) {
		if (link->dpcd_caps.branch_dev_id == DP_BRANCH_DEVICE_ID_90CC24 &&
			IS_SYNAPTICS_CASCADED_PANAMERA(link->dpcd_caps.branch_dev_name, branch_vendor_data)) {
			if (drm_debug_enabled(DRM_UT_DP))
				DRM_INFO_ONCE("Synaptics Cascaded MST hub detected on link %d.\n",
							  link->link_index);
				return true;
			}
	}
	return false;
}

static bool validate_dsc_caps_on_connector(struct amdgpu_dm_connector *aconnector)
{
	struct dc_sink *dc_sink;
	struct drm_dp_mst_port *port;
	u8 dsc_caps_dpcd[DP_DSC_RECEIVER_CAP_SIZE] = {0};
	u8 dsc_branch_dec_caps_raw[3] = {0};
	u8 *dsc_branch_dec_caps_ptr = NULL;
	ssize_t read_len;

	if (unlikely(!aconnector || !aconnector->dc_sink || !aconnector->mst_output_port ||
		!aconnector->dc_link || !aconnector->dc_link->ctx || !aconnector->dc_link->ctx->dc))
		return false;

	dc_sink = aconnector->dc_sink;
	port = aconnector->mst_output_port;

	aconnector->dsc_aux = drm_dp_mst_dsc_aux_for_port(port);

	if (!aconnector->dsc_aux && port->parent && !port->parent->port_parent &&
		needs_dsc_aux_workaround(aconnector->dc_link)) {
		if (likely(aconnector->mst_root))
			aconnector->dsc_aux = &aconnector->mst_root->dm_dp_aux.aux;
		}

		if (is_synaptics_cascaded_panamera(aconnector->dc_link, port)) {
			if (likely(port->mgr && port->mgr->aux))
				aconnector->dsc_aux = port->mgr->aux;
		}

		if (unlikely(!aconnector->dsc_aux))
			return false;

	read_len = drm_dp_dpcd_read(aconnector->dsc_aux, DP_DSC_SUPPORT,
								dsc_caps_dpcd, sizeof(dsc_caps_dpcd));
	if (unlikely(read_len != sizeof(dsc_caps_dpcd))) {
		if (drm_debug_enabled(DRM_UT_DP)) {
			drm_dbg_dp(aconnector->base.dev,
					   "%s: Failed to read DSC caps for %s. Read %zd bytes.\n",
			  __func__, aconnector->base.name, read_len);
		}
		return false;
	}

	read_len = drm_dp_dpcd_read(aconnector->dsc_aux, DP_DSC_BRANCH_OVERALL_THROUGHPUT_0,
								dsc_branch_dec_caps_raw, sizeof(dsc_branch_dec_caps_raw));
	if (read_len == sizeof(dsc_branch_dec_caps_raw)) {
		dsc_branch_dec_caps_ptr = dsc_branch_dec_caps_raw;
	} else if (drm_debug_enabled(DRM_UT_DP)) {
		drm_dbg_dp(aconnector->base.dev,
				   "%s: Failed to read DSC branch caps for %s. Read %zd bytes. (Non-fatal)\n",
				   __func__, aconnector->base.name, read_len);
	}

	if (!dc_dsc_parse_dsc_dpcd(aconnector->dc_link->ctx->dc,
		dsc_caps_dpcd, dsc_branch_dec_caps_ptr,
		&dc_sink->dsc_caps.dsc_dec_caps))
		return false;

	return true;
}
#endif

static bool retrieve_downstream_port_device(struct amdgpu_dm_connector *aconnector)
{
	union dp_downstream_port_present ds_port_present = {0};
	ssize_t bytes_read;

	if (unlikely(!aconnector || !aconnector->dsc_aux))
		return false;

	bytes_read = drm_dp_dpcd_read(aconnector->dsc_aux, DP_DOWNSTREAMPORT_PRESENT,
								  &ds_port_present, sizeof(ds_port_present));

	if (unlikely(bytes_read != sizeof(ds_port_present))) {
		if (drm_debug_enabled(DRM_UT_DP)) {
			drm_dbg_dp(aconnector->base.dev,
					   "%s: Failed DPCD read 0x%x on %s. Read %zd bytes.\n",
			  __func__, DP_DOWNSTREAMPORT_PRESENT,
			  aconnector->base.name, bytes_read);
		}
		return false;
	}

	aconnector->mst_downstream_port_present = ds_port_present;
	if (drm_debug_enabled(DRM_UT_DP)) {
		drm_dbg_dp(aconnector->base.dev, "%s: %s: DS port present: %u, type: %u\n",
				   __func__, aconnector->base.name,
			 ds_port_present.fields.PORT_PRESENT, ds_port_present.fields.PORT_TYPE);
	}
	return true;
}

static int dm_dp_mst_get_modes(struct drm_connector *connector)
{
	struct amdgpu_dm_connector *aconnector;
	const struct drm_edid *current_edid = NULL;
	int num_modes_added = 0;

	if (unlikely(!connector))
		return 0;

	aconnector = to_amdgpu_dm_connector(connector);
	if (unlikely(!aconnector))
		return drm_add_edid_modes(connector, NULL);

	if (unlikely(!aconnector->mst_root || !aconnector->mst_output_port || !aconnector->dc_link))
		return 0;

	if (!aconnector->drm_edid) {
		const struct drm_edid *newly_read_edid;

		newly_read_edid = drm_dp_mst_edid_read(connector,
											   &aconnector->mst_root->mst_mgr,
										 aconnector->mst_output_port);
		if (likely(newly_read_edid)) {
			aconnector->drm_edid = newly_read_edid;
			amdgpu_dm_set_mst_status(&aconnector->mst_status, MST_REMOTE_EDID, true);
		} else {
			amdgpu_dm_set_mst_status(&aconnector->mst_status, MST_REMOTE_EDID, false);
		}
	}
	current_edid = aconnector->drm_edid;

	if (aconnector->dc_sink && aconnector->dc_sink->sink_signal == SIGNAL_TYPE_VIRTUAL && current_edid) {
		dc_sink_release(aconnector->dc_sink);
		aconnector->dc_sink = NULL;
	}

	if (!aconnector->dc_sink) {
		struct dc_sink_init_data init_params = {
			.link = aconnector->dc_link,
			.sink_signal = SIGNAL_TYPE_DISPLAY_PORT_MST
		};
		struct dc_sink *new_dc_sink;
		const u8 *raw_edid_bytes = NULL;
		int raw_edid_len = 0;

		if (current_edid) {
			const struct edid *edid_struct_ptr = drm_edid_raw(current_edid);

			if (likely(edid_struct_ptr)) {
				raw_edid_bytes = (const u8 *)edid_struct_ptr;
				raw_edid_len = (edid_struct_ptr->extensions + 1) * EDID_LENGTH;
			}
		}

		new_dc_sink = dc_link_add_remote_sink(aconnector->dc_link,
											  raw_edid_bytes, raw_edid_len, &init_params);

		if (unlikely(!new_dc_sink)) {
			DRM_ERROR("%s: Connector %s: Failed to add remote sink.\n", __func__, connector->name);
			drm_edid_connector_update(&aconnector->base, NULL);
			drm_edid_free(aconnector->drm_edid);
			aconnector->drm_edid = NULL;
			return 0;
		}

		new_dc_sink->priv = aconnector;
		aconnector->dc_sink = new_dc_sink;

		if (connector->state) {
			struct amdgpu_device *adev = drm_to_adev(connector->dev);

			if (likely(adev && adev->dm.hdcp_workqueue &&
				aconnector->dc_link->link_index < AMDGPU_DM_MAX_NUM_LINKS &&
				connector->index < AMDGPU_DM_MAX_CONNECTORS_PER_LINK)) {
				struct hdcp_workqueue *hdcp_link_data =
				&adev->dm.hdcp_workqueue[aconnector->dc_link->link_index];
			connector->state->hdcp_content_type =
			hdcp_link_data->hdcp_content_type[connector->index];
			connector->state->content_protection =
			hdcp_link_data->content_protection[connector->index];
				}
		}

		if (likely(aconnector->dc_sink)) {
			if (current_edid) {
				amdgpu_dm_update_freesync_caps(connector, current_edid);
				#if defined(CONFIG_DRM_AMD_DC_FP)
				if (unlikely(!validate_dsc_caps_on_connector(aconnector))) {
					memset(&aconnector->dc_sink->dsc_caps, 0,
						   sizeof(aconnector->dc_sink->dsc_caps));
				}
				#endif
			}
			if (unlikely(!retrieve_downstream_port_device(aconnector))) {
				memset(&aconnector->mst_downstream_port_present, 0,
					   sizeof(aconnector->mst_downstream_port_present));
			}
		}
	}

	drm_edid_connector_update(&aconnector->base, current_edid);
	num_modes_added = drm_edid_connector_add_modes(connector);
	return num_modes_added;
}


static struct drm_encoder *
dm_mst_atomic_best_encoder(struct drm_connector *connector,
						   struct drm_atomic_state *state)
{
	struct drm_connector_state *connector_state;
	struct amdgpu_device *adev;
	struct amdgpu_crtc *acrtc;

	if (unlikely(!connector || !state))
		return NULL;
	adev = drm_to_adev(connector->dev);
	if (unlikely(!adev))
		return NULL;

	connector_state = drm_atomic_get_new_connector_state(state, connector);
	if (unlikely(!connector_state || !connector_state->crtc))
		return NULL;

	acrtc = to_amdgpu_crtc(connector_state->crtc);
	if (unlikely(!acrtc || acrtc->crtc_id >= adev->dm.display_indexes_num))
		return NULL;

	return &adev->dm.mst_encoders[acrtc->crtc_id].base;
}

static int
dm_dp_mst_detect(struct drm_connector *connector,
				 struct drm_modeset_acquire_ctx *ctx, bool force)
{
	struct amdgpu_dm_connector *aconnector;
	struct amdgpu_dm_connector *master_aconnector;
	struct drm_dp_mst_port *port;
	int connection_status;

	if (unlikely(!connector))
		return connector_status_unknown;
	aconnector = to_amdgpu_dm_connector(connector);
	if (unlikely(!aconnector || !aconnector->mst_root || !aconnector->mst_output_port))
		return connector_status_unknown;

	master_aconnector = aconnector->mst_root;
	port = aconnector->mst_output_port;

	if (unlikely(drm_connector_is_unregistered(connector)))
		return connector_status_disconnected;

	connection_status = drm_dp_mst_detect_port(connector, ctx, &master_aconnector->mst_mgr, port);

	if (port->pdt != DP_PEER_DEVICE_NONE && unlikely(!port->dpcd_rev)) {
		u8 rev_val = 0;
		ssize_t bytes_read;

		bytes_read = drm_dp_dpcd_readb(&port->aux, DP_DP13_DPCD_REV, &rev_val);
		if (likely(bytes_read == 1 && rev_val != 0)) {
			port->dpcd_rev = rev_val;
		} else {
			bytes_read = drm_dp_dpcd_readb(&port->aux, DP_DPCD_REV, &rev_val);
			if (likely(bytes_read == 1))
				port->dpcd_rev = rev_val;
			else if (drm_debug_enabled(DRM_UT_KMS))
				DRM_DEBUG_KMS("%s: Failed to read DPCD rev for %s (err %zd).\n",
							  __func__, connector->name, bytes_read);
		}
	} else if (port->pdt == DP_PEER_DEVICE_NONE && port->dpcd_rev != 0) {
		port->dpcd_rev = 0;
	}

	if (connection_status == connector_status_disconnected && likely(aconnector->dc_sink)) {
		if (likely(aconnector->dc_link && aconnector->dc_link->sink_count > 0))
			dc_link_remove_remote_sink(aconnector->dc_link, aconnector->dc_sink);

		dc_sink_release(aconnector->dc_sink);
		aconnector->dc_sink = NULL;
		amdgpu_dm_mst_reset_mst_connector_setting(aconnector);
		amdgpu_dm_set_mst_status(&aconnector->mst_status,
								 MST_REMOTE_EDID | MST_ALLOCATE_NEW_PAYLOAD | MST_CLEAR_ALLOCATED_PAYLOAD,
						   false);
	}
	return connection_status;
}

static int dm_dp_mst_atomic_check(struct drm_connector * const connector,
								  struct drm_atomic_state * const state)
{
	struct amdgpu_dm_connector *aconnector;
	struct drm_dp_mst_topology_mgr *mst_mgr;
	struct drm_dp_mst_port *mst_port;

	if (unlikely(!connector || !state))
		return -EINVAL;
	aconnector = to_amdgpu_dm_connector(connector);
	if (unlikely(!aconnector || !aconnector->mst_root || !aconnector->mst_output_port))
		return -EINVAL;

	mst_mgr = &aconnector->mst_root->mst_mgr;
	mst_port = aconnector->mst_output_port;

	return drm_dp_atomic_release_time_slots(state, mst_mgr, mst_port);
}

static const struct drm_connector_helper_funcs dm_dp_mst_connector_helper_funcs = {
	.get_modes = dm_dp_mst_get_modes,
	.mode_valid = amdgpu_dm_connector_mode_valid,
	.atomic_best_encoder = dm_mst_atomic_best_encoder,
	.detect_ctx = dm_dp_mst_detect,
	.atomic_check = dm_dp_mst_atomic_check,
};

static void amdgpu_dm_encoder_destroy(struct drm_encoder *encoder)
{
	if (unlikely(!encoder))
		return;
	drm_encoder_cleanup(encoder);
}

static const struct drm_encoder_funcs amdgpu_dm_encoder_funcs = {
	.destroy = amdgpu_dm_encoder_destroy,
};

void
dm_dp_create_fake_mst_encoders(struct amdgpu_device * const adev)
{
	struct drm_device *drm_dev;
	int i;
	uint32_t possible_crtcs_mask;

	if (unlikely(!adev || !adev_to_drm(adev)))
		return;
	drm_dev = adev_to_drm(adev);

	possible_crtcs_mask = amdgpu_dm_get_encoder_crtc_mask(adev);

	for (i = 0; i < adev->dm.display_indexes_num; i++) {
		struct amdgpu_encoder *amdgpu_mst_encoder = &adev->dm.mst_encoders[i];
		struct drm_encoder *encoder = &amdgpu_mst_encoder->base;
		char encoder_name[32];

		encoder->possible_crtcs = possible_crtcs_mask;
		snprintf(encoder_name, sizeof(encoder_name), "amdgpu_mst_enc%d", i);
		drm_encoder_init(drm_dev, encoder, &amdgpu_dm_encoder_funcs,
						 DRM_MODE_ENCODER_DPMST, encoder_name);
		drm_encoder_helper_add(encoder, &amdgpu_dm_encoder_helper_funcs);
	}
}

static struct drm_connector *
dm_dp_add_mst_connector(struct drm_dp_mst_topology_mgr *mgr,
						struct drm_dp_mst_port *port,
						const char *pathprop)
{
	struct amdgpu_dm_connector *root;
	struct drm_device *ddev;
	struct amdgpu_device *adev;
	struct amdgpu_dm_connector *aconn;
	struct drm_connector *drm_conn;
	u32 i;
	int ret;

	if (unlikely(!mgr || !port || !pathprop))
		return NULL;

	root = container_of(mgr, struct amdgpu_dm_connector, mst_mgr);
	if (unlikely(!root || !root->base.dev))
		return NULL;

	ddev = root->base.dev;
	adev = drm_to_adev(ddev);
	if (unlikely(!adev))
		return NULL;

	aconn = kzalloc(sizeof(*aconn), GFP_KERNEL);
	if (!aconn)
		return NULL;

	aconn->mst_output_port = port;
	aconn->mst_root = root;
	amdgpu_dm_set_mst_status(&aconn->mst_status, MST_PROBE, true);

	mutex_init(&aconn->handle_mst_msg_ready);

	drm_conn = &aconn->base;

	ret = drm_connector_init(ddev, drm_conn, &dm_dp_mst_connector_funcs,
							 DRM_MODE_CONNECTOR_DisplayPort);
	if (ret) {
		kfree(aconn);
		return NULL;
	}

	drm_connector_helper_add(drm_conn, &dm_dp_mst_connector_helper_funcs);

	amdgpu_dm_connector_init_helper(&adev->dm, aconn,
									DRM_MODE_CONNECTOR_DisplayPort,
								 root->dc_link,
								 root->connector_id);

	for (i = 0; i < adev->dm.display_indexes_num; ++i) {
		drm_connector_attach_encoder(drm_conn,
									 &adev->dm.mst_encoders[i].base);
	}

	if (root->base.max_bpc_property) {
		drm_conn->max_bpc_property = root->base.max_bpc_property;
		drm_connector_attach_max_bpc_property(drm_conn, 8, 16);
	}

	if (root->base.vrr_capable_property) {
		drm_conn->vrr_capable_property = root->base.vrr_capable_property;
		drm_connector_attach_vrr_capable_property(drm_conn);
	}

	if (root->base.colorspace_property) {
		drm_conn->colorspace_property = root->base.colorspace_property;
		drm_connector_attach_colorspace_property(drm_conn);
	}

	drm_object_attach_property(&drm_conn->base,
							   ddev->mode_config.path_property, 0);
	drm_object_attach_property(&drm_conn->base,
							   ddev->mode_config.tile_property, 0);

	drm_connector_set_path_property(drm_conn, pathprop);
	amdgpu_dm_connector_funcs_reset(drm_conn);

	drm_dp_mst_get_port_malloc(port);

	return drm_conn;
}

void dm_handle_mst_sideband_msg_ready_event(
	struct drm_dp_mst_topology_mgr *const mgr,
	enum mst_msg_ready_type msg_rdy_type)
{
	struct amdgpu_dm_connector *root_aconn;
	const struct dc_link_status *link_st;
	struct drm_dp_aux *aux;
	u8 esi_buf[MST_ESI_ACK_BUFFER_MAX_LEN] = { 0 };
	u8 ack_buf[MST_ESI_ACK_BUFFER_MAX_LEN] = { 0 };
	int esi_base;
	u8 bytes_to_read;
	unsigned int loop;
	ssize_t ret;

	if (unlikely(!mgr))
		return;

	root_aconn = container_of(mgr, struct amdgpu_dm_connector, mst_mgr);
	if (unlikely(!root_aconn || !root_aconn->dc_link ||
		!root_aconn->dm_dp_aux.aux.dev))
		return;

	link_st = dc_link_get_status(root_aconn->dc_link);
	if (unlikely(!link_st || !link_st->dpcd_caps))
		return;

	if (link_st->dpcd_caps->dpcd_rev.raw >= DP_DPCD_REV_12) {
		bytes_to_read = (DP_PSR_ERROR_STATUS - DP_SINK_COUNT_ESI) + 1;
		esi_base = DP_SINK_COUNT_ESI;
	} else {
		bytes_to_read = (DP_LANE0_1_STATUS - DP_SINK_COUNT) + 1;
		esi_base = DP_SINK_COUNT;
	}
	bytes_to_read = min_t(u8, bytes_to_read, MST_ESI_ACK_BUFFER_MAX_LEN);
	aux = &root_aconn->dm_dp_aux.aux;

	if (!mutex_trylock(&root_aconn->handle_mst_msg_ready)) {
		if (mgr->work.func)
			schedule_work(&mgr->work);
		return;
	}

	for (loop = MAX_MST_SIDEBAND_MSG_PROCESS_COUNT; loop > 0; --loop) {
		bool irq_handled = false;
		u8 *irq_vec;
		u8 retry;

		memset(esi_buf, 0, bytes_to_read);
		memset(ack_buf, 0, bytes_to_read);

		ret = drm_dp_dpcd_read(aux, esi_base, esi_buf, bytes_to_read);
		if (ret != bytes_to_read)
			break;

		irq_vec = &esi_buf[1];
		switch (msg_rdy_type) {
			case DOWN_REP_MSG_RDY_EVENT:
				*irq_vec &= DP_DOWN_REP_MSG_RDY;
				break;
			case UP_REQ_MSG_RDY_EVENT:
				*irq_vec &= DP_UP_REQ_MSG_RDY;
				break;
			default:
				*irq_vec &= DP_DOWN_REP_MSG_RDY | DP_UP_REQ_MSG_RDY;
				break;
		}

		if (!*irq_vec)
			break;

		if (mgr->mst_state) {
			drm_dp_mst_hpd_irq_handle_event(mgr, esi_buf,
											ack_buf, &irq_handled);
		}
		if (!irq_handled)
			break;

		for (retry = 0; retry < MAX_DPCD_ACCESS_RETRIES; ++retry) {
			ret = drm_dp_dpcd_writeb(aux, esi_base + 1, ack_buf[1]);
			if (ret == 1)
				break;
		}
		if (retry == MAX_DPCD_ACCESS_RETRIES) {
			DRM_ERROR("%s: ACK write timeout on %s\n",
					  __func__, root_aconn->base.name);
			break;
		}

		drm_dp_mst_hpd_irq_send_new_request(mgr);
	}

	mutex_unlock(&root_aconn->handle_mst_msg_ready);

	if (!loop && drm_debug_enabled(DRM_UT_DRIVER)) {
		DRM_DEBUG_DRIVER("%s: loop budget exhausted on %s\n",
						 __func__, root_aconn->base.name);
	}
}

static void dm_handle_mst_down_rep_msg_ready(struct drm_dp_mst_topology_mgr * const mgr)
{
	dm_handle_mst_sideband_msg_ready_event(mgr, DOWN_REP_MSG_RDY_EVENT);
}

static const struct drm_dp_mst_topology_cbs dm_mst_cbs = {
	.add_connector = dm_dp_add_mst_connector,
	.poll_hpd_irq = dm_handle_mst_down_rep_msg_ready,
};

void amdgpu_dm_initialize_dp_connector(struct amdgpu_display_manager * const dm,
									   struct amdgpu_dm_connector *aconnector,
									   int link_index)
{
	if (unlikely(!dm || !aconnector || !aconnector->dc_link || !dm->ddev || !dm->adev))
		return;

	aconnector->dm_dp_aux.aux.name = kasprintf(GFP_KERNEL, "AMDGPU DM aux hw bus %d", link_index);
	if (unlikely(!aconnector->dm_dp_aux.aux.name))
		DRM_ERROR("%s: kasprintf failed for aux name (link %d)\n", __func__, link_index);

	aconnector->dm_dp_aux.aux.transfer = dm_dp_aux_transfer;
	aconnector->dm_dp_aux.aux.drm_dev = dm->ddev;
	aconnector->dm_dp_aux.ddc_service = aconnector->dc_link->ddc;

	drm_dp_aux_init(&aconnector->dm_dp_aux.aux);

	drm_dp_cec_register_connector(&aconnector->dm_dp_aux.aux, &aconnector->base);

	if (aconnector->base.connector_type == DRM_MODE_CONNECTOR_eDP)
		return;

	aconnector->mst_mgr.cbs = &dm_mst_cbs;
	drm_dp_mst_topology_mgr_init(&aconnector->mst_mgr, adev_to_drm(dm->adev),
								 &aconnector->dm_dp_aux.aux,
							  DP_MST_MAX_DPCD_TRANSACTION_BYTES_PER_MANAGER,
							  DP_MST_MAX_PAYLOADS_PER_MANAGER,
							  aconnector->connector_id);

	drm_connector_attach_dp_subconnector_property(&aconnector->base);
}

int dm_mst_get_pbn_divider(struct dc_link *link)
{
	u64 link_bw_kbps_total;
	const struct dc_link_settings *link_cap;

	if (unlikely(!link))
		return 0;
	link_cap = dc_link_get_link_cap(link);
	if (unlikely(!link_cap))
		return 0;

	link_bw_kbps_total = dc_link_bandwidth_kbps(link, link_cap);
	if (unlikely(link_bw_kbps_total == 0))
		return 0;

	return div_u64(link_bw_kbps_total, (54ULL * 8ULL * 1000ULL));
}


#if defined(CONFIG_DRM_AMD_DC_FP)
static __always_inline u16
get_fec_overhead_multiplier(const struct dc_link *dc_link)
{
	if (!dc_link)
		return PBN_FEC_OVERHEAD_MULTIPLIER_8B_10B;

	return dc_link_dp_mst_decide_link_encoding_format(dc_link) ==
	DP_128b_132b_ENCODING
	? PBN_FEC_OVERHEAD_MULTIPLIER_128B_132B
	: PBN_FEC_OVERHEAD_MULTIPLIER_8B_10B;
}

static __always_inline int
kbps_to_peak_pbn(int kbps, u16 fec_overhead_multiplier_x1000)
{
	u64 peak_kbps_intermediate;
	u64 result;

	if (unlikely(kbps <= 0))
		return 0;

	if (kbps > 2000000)
		kbps = 2000000;

	if (unlikely(fec_overhead_multiplier_x1000 == 0))
		fec_overhead_multiplier_x1000 = 1000;

	peak_kbps_intermediate = (u64)kbps * PEAK_FACTOR_X1000 *
	fec_overhead_multiplier_x1000;
	peak_kbps_intermediate =
	div_u64(peak_kbps_intermediate, 1000ULL * 1000ULL);

	result = DIV64_U64_ROUND_UP(peak_kbps_intermediate * 64ULL,
								(54ULL * 8ULL * 1000ULL));

	return (result > INT_MAX) ? INT_MAX : (int)result;
}

static void
set_dsc_configs_from_fairness_vars(struct dsc_mst_fairness_params *params,
								   const struct dsc_mst_fairness_vars *vars,
								   int count, int k_off)
{
	int i;
	if (count <= 0 || count > MAX_PIPES)
		return;
	for (i = 0; i < count; ++i) {
		struct dsc_mst_fairness_params *p = &params[i];
		const struct dsc_mst_fairness_vars *v = &vars[i + k_off];
		struct dc_crtc_timing *t = p->timing;
		struct dc *dc;
		struct dc_dsc_config_options opt = { };
		if (!t || !p->sink || !p->sink->ctx ||
			!(dc = p->sink->ctx->dc) ||
			!dc->res_pool || !dc->res_pool->dscs[0]) {
			if (t) {
				memset(&t->dsc_cfg, 0, sizeof(t->dsc_cfg));
				t->flags.DSC = 0;
			}
			continue;
			}
			memset(&t->dsc_cfg, 0, sizeof(t->dsc_cfg));
		t->flags.DSC = 0;
		if (!v->dsc_enabled)
			goto store_pbn;
		dc_dsc_get_default_config_option(dc, &opt);
		if (p->aconnector->base.display_info.max_dsc_bpp)
			opt.max_target_bpp_limit_override_x16 =
			p->aconnector->base.display_info.max_dsc_bpp * 16;
		if (dc_dsc_compute_config(dc->res_pool->dscs[0],
			&p->sink->dsc_caps.dsc_dec_caps,
			&opt, 0, t,
			dc_link_get_highest_encoding_format(
				p->aconnector->dc_link),
				&t->dsc_cfg)) {
			t->flags.DSC = 1;
		t->dsc_cfg.bits_per_pixel =
		p->bpp_overwrite ? p->bpp_overwrite : v->bpp_x16;
		if (p->num_slices_h) {
			t->dsc_cfg.num_slices_h = p->num_slices_h;
		}
		if (p->num_slices_v) {
			t->dsc_cfg.num_slices_v = p->num_slices_v;
		}
				}

				store_pbn:
				t->dsc_cfg.mst_pbn = v->pbn;
	}
}

static int
bpp_x16_from_pbn(struct dsc_mst_fairness_params param, int pbn)
{
	struct dc_dsc_config_options opt = { };
	struct dc_dsc_config cfg = { };
	struct drm_connector *drm_conn;
	u64 kbps;

	if (!param.timing || !param.sink || !param.sink->ctx ||
		!param.sink->ctx->dc || !param.aconnector ||
		!param.sink->ctx->dc->res_pool ||
		!param.sink->ctx->dc->res_pool->dscs[0] || pbn <= 0)
		return 0;

	kbps = div_u64((u64)pbn * 994ULL * 8ULL * 54ULL, 64ULL);
	drm_conn = &param.aconnector->base;

	dc_dsc_get_default_config_option(param.sink->ctx->dc, &opt);
	if (drm_conn->display_info.max_dsc_bpp)
		opt.max_target_bpp_limit_override_x16 =
		drm_conn->display_info.max_dsc_bpp * 16;

	if (dc_dsc_compute_config(param.sink->ctx->dc->res_pool->dscs[0],
		&param.sink->dsc_caps.dsc_dec_caps,
		&opt, (int)kbps, param.timing,
							  dc_link_get_highest_encoding_format(
								  param.aconnector->dc_link),
						   &cfg))
		return cfg.bits_per_pixel;

	return 0;
}

static int
increase_dsc_bpp(struct drm_atomic_state *state,
				 struct drm_dp_mst_topology_state *mst,
				 struct dc_link *link,
				 struct dsc_mst_fairness_params *p,
				 struct dsc_mst_fairness_vars *v,
				 int n,
				 int k_off)
{
	u16 fec_mul = get_fec_overhead_multiplier(link);
	int slack[MAX_PIPES] = { };
	unsigned long pending_mask = 0;
	int i, ret = 0, pbn_div;

	if (WARN_ON(n <= 0 || n > MAX_PIPES))
		return -EINVAL;

	pbn_div = dfixed_trunc(mst->pbn_div);
	if (!pbn_div)
		return -EINVAL;

	for (i = 0; i < n; ++i) {
		if (!p[i].timing || !v[i + k_off].dsc_enabled)
			continue;

		slack[i] = kbps_to_peak_pbn(p[i].bw_range.max_kbps,
									fec_mul) -
									v[i + k_off].pbn;
									if (slack[i] < 0)
										slack[i] = 0;

		if (slack[i])
			pending_mask |= BIT(i);
	}

	while (pending_mask) {
		int idx = -1, min_slack = INT_MAX, used_slots = 0;
		int free_slots, fair_share, add_pbn, old_pbn;
		int bit;

		for_each_set_bit(bit, &pending_mask, n) {
			if (slack[bit] < min_slack) {
				min_slack = slack[bit];
				idx = bit;
			}
		}
		if (idx == -1)
			break;

		for (i = 0; i < n; ++i)
			used_slots += DIV_ROUND_UP(v[i + k_off].pbn, pbn_div);

		free_slots = DP_TOTAL_TIMESLOTS - used_slots;
		if (free_slots <= 0)
			break;

		fair_share = (free_slots / hweight_long(pending_mask)) *
		pbn_div;
		add_pbn = min(slack[idx], fair_share);
		if (!add_pbn) {
			pending_mask &= ~BIT(idx);
			continue;
		}

		old_pbn = v[idx + k_off].pbn;
		v[idx + k_off].pbn += add_pbn;

		ret = drm_dp_atomic_find_time_slots(state,
											p[idx].port->mgr,
									  p[idx].port,
									  v[idx + k_off].pbn);
		if (ret < 0)
			goto rollback;

		ret = drm_dp_mst_atomic_check_slots(state,
											p[idx].port->mgr);
		if (ret)
			goto rollback;

		slack[idx] -= add_pbn;
		if (!slack[idx])
			pending_mask &= ~BIT(idx);

		v[idx + k_off].bpp_x16 =
		bpp_x16_from_pbn(p[idx], v[idx + k_off].pbn);
		continue;
		rollback:
		v[idx + k_off].pbn = old_pbn;
		(void)drm_dp_atomic_find_time_slots(state,
											p[idx].port->mgr,
									  p[idx].port,
									  old_pbn);
		pending_mask &= ~BIT(idx);
		if (ret)
			return ret;
	}
	return 0;
}

static int try_disable_dsc(struct drm_atomic_state *state,
						   struct dc_link *dc_link,
						   struct dsc_mst_fairness_params *params,
						   struct dsc_mst_fairness_vars *vars,
						   int count,
						   int k_offset_vars)
{
	int i, ret = 0;
	bool tried[MAX_PIPES];
	int kbps_increase_if_dsc_disabled[MAX_PIPES];
	int streams_remaining_to_try = 0;
	u16 fec_overhead_multiplier_x1000 = get_fec_overhead_multiplier(dc_link);

	if (unlikely(!state || !dc_link || !params || !vars || count <= 0 || count > MAX_PIPES))
		return -EINVAL;

	for (i = 0; i < count; i++) {
		if (vars[i + k_offset_vars].dsc_enabled &&
			vars[i + k_offset_vars].bpp_x16 == params[i].bw_range.max_target_bpp_x16 &&
			params[i].clock_force_enable == DSC_CLK_FORCE_DEFAULT) {
			kbps_increase_if_dsc_disabled[i] = params[i].bw_range.stream_kbps - params[i].bw_range.max_kbps;
		tried[i] = false;
		streams_remaining_to_try++;
			} else {
				kbps_increase_if_dsc_disabled[i] = 0;
				tried[i] = true;
			}
	}

	while (streams_remaining_to_try > 0) {
		int stream_idx_to_try = -1, max_kbps_increase_val = -1;
		int original_pbn_for_stream;

		for (i = 0; i < count; i++) {
			if (!tried[i] && (stream_idx_to_try == -1 ||
				kbps_increase_if_dsc_disabled[i] > max_kbps_increase_val)) {
				max_kbps_increase_val = kbps_increase_if_dsc_disabled[i];
			stream_idx_to_try = i;
				}
		}
		if (unlikely(stream_idx_to_try == -1))
			break;

		original_pbn_for_stream = vars[stream_idx_to_try + k_offset_vars].pbn;
		vars[stream_idx_to_try + k_offset_vars].pbn =
		kbps_to_peak_pbn(params[stream_idx_to_try].bw_range.stream_kbps, fec_overhead_multiplier_x1000);

		ret = drm_dp_atomic_find_time_slots(state, params[stream_idx_to_try].port->mgr,
											params[stream_idx_to_try].port,
									  vars[stream_idx_to_try + k_offset_vars].pbn);
		if (ret < 0) {
			vars[stream_idx_to_try + k_offset_vars].pbn = original_pbn_for_stream;
			tried[stream_idx_to_try] = true;
			streams_remaining_to_try--;
			return ret;
		}

		ret = drm_dp_mst_atomic_check_slots(state, params[stream_idx_to_try].port->mgr);
		if (ret == 0) {
			vars[stream_idx_to_try + k_offset_vars].dsc_enabled = false;
			vars[stream_idx_to_try + k_offset_vars].bpp_x16 = 0;
		} else {
			vars[stream_idx_to_try + k_offset_vars].pbn = original_pbn_for_stream;
			ret = drm_dp_atomic_find_time_slots(state, params[stream_idx_to_try].port->mgr,
												params[stream_idx_to_try].port,
									   vars[stream_idx_to_try + k_offset_vars].pbn);
			if (ret < 0)
				return ret;
		}
		tried[stream_idx_to_try] = true;
		streams_remaining_to_try--;
	}
	return 0;
}

static void __maybe_unused
log_dsc_params(int count,
			   struct dsc_mst_fairness_vars *vars,
			   int k_offset_vars)
{
	int i;

	if (!drm_debug_enabled(DRM_UT_DRIVER))
		return;

	for (i = 0; i < count; ++i) {
		const struct dsc_mst_fairness_vars *v = &vars[i + k_offset_vars];

		DRM_DEBUG_DRIVER(
			"MST-DSC  stream[%d]: dsc_on=%d  bpp*16=%d  pbn=%d\n",
			i, v->dsc_enabled, v->bpp_x16, v->pbn);
	}
}

static int
compute_mst_dsc_configs_for_link(struct drm_atomic_state *state,
								 struct dc_state *dc_state,
								 struct dc_link *dc_link,
								 struct dsc_mst_fairness_vars *vars,
								 struct drm_dp_mst_topology_mgr *mgr,
								 int *link_vars_idx)
{
	struct dsc_mst_fairness_params params[MAX_PIPES] = { };
	struct drm_dp_mst_topology_state *mst_state;
	u16 fec_mul_x1000;
	enum dc_link_encoding_format encoding;
	bool debug_force_dsc = false;
	unsigned int n = 0;
	int ret;
	int var_off;

	if (!state || !dc_state || !dc_link || !vars || !mgr || !link_vars_idx)
		return -EINVAL;

	if (*link_vars_idx < 0 || *link_vars_idx >= MAX_PIPES)
		return -EINVAL;

	mst_state = drm_atomic_get_mst_topology_state(state, mgr);
	if (IS_ERR(mst_state))
		return PTR_ERR(mst_state);

	if (dfixed_trunc(mst_state->pbn_div) <= 0)
		return -EINVAL;

	fec_mul_x1000 = get_fec_overhead_multiplier(dc_link);
	encoding = dc_link_get_highest_encoding_format(dc_link);

	for (unsigned int i = 0; i < dc_state->stream_count && n < MAX_PIPES; ++i) {
		struct dc_stream_state *s = dc_state->streams[i];
		struct amdgpu_dm_connector *aconn;
		struct dsc_mst_fairness_params *p;

		if (!s || s->link != dc_link)
			continue;

		aconn = (struct amdgpu_dm_connector *)s->dm_stream_context;
		if (!aconn || !aconn->mst_output_port)
			continue;

		if (IS_ERR_OR_NULL(drm_atomic_get_new_connector_state(state, &aconn->base)))
			continue;

		p = &params[n];
		s->timing.flags.DSC = 0;
		p->timing = &s->timing;
		p->sink = s->sink;
		p->aconnector = aconn;
		p->port = aconn->mst_output_port;
		p->clock_force_enable = aconn->dsc_settings.dsc_force_enable;
		p->num_slices_h = aconn->dsc_settings.dsc_num_slices_h;
		p->num_slices_v = aconn->dsc_settings.dsc_num_slices_v;
		p->bpp_overwrite = aconn->dsc_settings.dsc_bits_per_pixel;
		p->compression_possible = (s->sink &&
		s->sink->dsc_caps.dsc_dec_caps.is_dsc_supported);

		if (p->clock_force_enable == DSC_CLK_FORCE_ENABLE)
			debug_force_dsc = true;

		if (s->sink && s->sink->ctx && s->sink->ctx->dc &&
			s->sink->ctx->dc->res_pool && s->sink->ctx->dc->res_pool->dscs[0]) {
			struct dc_dsc_policy pol = { };

		dc_dsc_get_policy_for_timing(p->timing, 0, &pol, encoding);

		if (!dc_dsc_compute_bandwidth_range(
			s->sink->ctx->dc->res_pool->dscs[0],
			s->sink->ctx->dc->debug.dsc_min_slice_height_override,
			pol.min_target_bpp * 16,
			pol.max_target_bpp * 16,
			&s->sink->dsc_caps.dsc_dec_caps,
			&s->timing, encoding, &p->bw_range)) {
			p->bw_range.stream_kbps =
			dc_bandwidth_in_kbps_from_timing(&s->timing, encoding);
			}
			} else {
				p->bw_range.stream_kbps =
				dc_bandwidth_in_kbps_from_timing(&s->timing, encoding);
			}
			++n;
	}

	if (n == 0)
		return 0;

	var_off = *link_vars_idx;
	if (var_off + n > MAX_PIPES)
		return -EINVAL;

	*link_vars_idx += n;

	for (unsigned int i = 0; i < n; ++i) {
		int pbn = kbps_to_peak_pbn(params[i].bw_range.stream_kbps, fec_mul_x1000);

		if (var_off + i >= MAX_PIPES)
			return -EINVAL;

		vars[var_off + i].pbn = pbn;
		vars[var_off + i].dsc_enabled = false;
		vars[var_off + i].bpp_x16 = 0;

		ret = drm_dp_atomic_find_time_slots(state, params[i].port->mgr,
											params[i].port, pbn);
		if (ret < 0)
			return ret;
	}

	ret = drm_dp_mst_atomic_check_slots(state, mgr);
	if (ret == 0 && !debug_force_dsc) {
		set_dsc_configs_from_fairness_vars(params, vars, n, var_off);
		return 0;
	}
	if (ret && ret != -ENOSPC)
		return ret;

	for (unsigned int i = 0; i < n; ++i) {
		int pbn;

		if (!params[i].compression_possible ||
			params[i].clock_force_enable == DSC_CLK_FORCE_DISABLE)
			continue;

		if (var_off + i >= MAX_PIPES)
			return -EINVAL;

		pbn = kbps_to_peak_pbn(params[i].bw_range.min_kbps, fec_mul_x1000);
		vars[var_off + i].pbn = pbn;
		vars[var_off + i].dsc_enabled = true;
		vars[var_off + i].bpp_x16 = params[i].bw_range.min_target_bpp_x16;

		ret = drm_dp_atomic_find_time_slots(state, params[i].port->mgr,
											params[i].port, pbn);
		if (ret < 0)
			return ret;
	}

	ret = drm_dp_mst_atomic_check_slots(state, mgr);
	if (ret)
		return ret;

	ret = increase_dsc_bpp(state, mst_state, dc_link, params, vars, n, var_off);
	if (ret)
		return ret;

	if (!debug_force_dsc) {
		ret = try_disable_dsc(state, dc_link, params, vars, n, var_off);
		if (ret)
			return ret;
	}

	set_dsc_configs_from_fairness_vars(params, vars, n, var_off);
	return 0;
}

static bool
is_dsc_recompute_needed(struct drm_atomic_state *state,
						struct dc_state *dc_state_new,
						struct dc_link *dc_link_to_check)
{
	int i, j;
	struct amdgpu_dm_connector *streams_on_link_new[MAX_PIPES] = { NULL };
	int num_streams_on_link_new = 0;
	struct dc_stream_state *stream;
	const struct dc *dc_instance;

	if (unlikely(!state || !dc_state_new || !dc_link_to_check ||
		!dc_link_to_check->dc)) {
		return false;
		}
		dc_instance = dc_link_to_check->dc;

	if (dc_link_to_check->type != DC_CONNECTION_TYPE_MST_BRANCH)
		return false;

	if (needs_dsc_aux_workaround(dc_link_to_check) &&
		!(dc_link_to_check->dpcd_caps.dsc_caps.dsc_basic_caps.fields.
		dsc_support.DSC_SUPPORT ||
		dc_link_to_check->dpcd_caps.dsc_caps.dsc_basic_caps.fields.
		dsc_support.DSC_PASSTHROUGH_SUPPORT)) {
		return false;
		}

		for (i = 0; i < dc_state_new->stream_count && num_streams_on_link_new < MAX_PIPES; ++i) {
			struct drm_crtc_state *new_crtc_state;
			struct drm_connector_state *new_conn_state;
			struct amdgpu_dm_connector *aconn;

			stream = dc_state_new->streams[i];
			if (!stream || stream->link != dc_link_to_check)
				continue;

			aconn = (struct amdgpu_dm_connector *)stream->dm_stream_context;
			if (!aconn)
				continue;

			streams_on_link_new[num_streams_on_link_new++] = aconn;

			new_conn_state = drm_atomic_get_new_connector_state(state, &aconn->base);
			if (!new_conn_state || IS_ERR(new_conn_state) || !new_conn_state->crtc)
				continue;

			new_crtc_state = drm_atomic_get_new_crtc_state(state, new_conn_state->crtc);
			if (!new_crtc_state || IS_ERR(new_crtc_state))
				continue;

			if (new_crtc_state->enable &&
				new_crtc_state->active &&
				(new_crtc_state->mode_changed ||
				new_crtc_state->active_changed ||
				new_crtc_state->connectors_changed)) {
				return true;
				}
		}

		if (dc_instance->current_state != NULL) {
			for (i = 0; i < dc_instance->current_state->stream_count; ++i) {
				bool found_in_new_state = false;
				struct amdgpu_dm_connector *current_aconn;
				struct drm_crtc_state *new_crtc_state_for_curr = NULL;
				struct drm_connector_state *new_conn_state_for_curr = NULL;

				stream = dc_instance->current_state->streams[i];
				if (!stream || stream->link != dc_link_to_check)
					continue;

				current_aconn = (struct amdgpu_dm_connector *)stream->dm_stream_context;
				if (!current_aconn)
					continue;

				for (j = 0; j < num_streams_on_link_new; ++j) {
					if (streams_on_link_new[j] == current_aconn) {
						found_in_new_state = true;
						break;
					}
				}
				if (!found_in_new_state)
					return true;

				new_conn_state_for_curr =
				drm_atomic_get_new_connector_state(state,
												   &current_aconn->base);
				if (new_conn_state_for_curr && new_conn_state_for_curr->crtc) {
					new_crtc_state_for_curr =
					drm_atomic_get_new_crtc_state(state,
												  new_conn_state_for_curr->crtc);
					if (new_crtc_state_for_curr && !new_crtc_state_for_curr->enable)
						return true;
				} else if (new_conn_state_for_curr && !new_conn_state_for_curr->crtc) {
					return true;
				}
			}
		}

		if (num_streams_on_link_new > 0) {
			int curr_cnt = 0;

			if (dc_instance->current_state != NULL) {
				for (i = 0; i < dc_instance->current_state->stream_count; ++i) {
					if (dc_instance->current_state->streams[i] &&
						dc_instance->current_state->streams[i]->link == dc_link_to_check) {
						++curr_cnt;
						}
				}
			}
			if (num_streams_on_link_new > curr_cnt)
				return true;
		}

		return false;
}

int compute_mst_dsc_configs_for_state(struct drm_atomic_state *state,
									  struct dc_state *dc_state_new,
									  struct dsc_mst_fairness_vars *vars)
{
	int i, ret = 0;
	struct dc_stream_state *stream;
	bool computed_for_link[MAX_PIPES] = {false};
	struct amdgpu_dm_connector *aconnector;
	struct drm_dp_mst_topology_mgr *mst_mgr;
	int link_vars_start_index = 0;

	if (unlikely(!state || !dc_state_new || !vars))
		return -EINVAL;

	for (i = 0; i < dc_state_new->stream_count; i++) {
		stream = dc_state_new->streams[i];
		if (!stream || stream->signal != SIGNAL_TYPE_DISPLAY_PORT_MST || !stream->link ||
			stream->link->link_index >= MAX_PIPES)
			continue;

		aconnector = (struct amdgpu_dm_connector *)stream->dm_stream_context;
		if (!aconnector || !aconnector->dc_sink || !aconnector->mst_output_port ||
			!aconnector->dc_sink->dsc_caps.dsc_dec_caps.is_dsc_supported)
			continue;

		if (computed_for_link[stream->link->link_index])
			continue;

		if (stream->ctx && stream->ctx->dc && stream->ctx->dc->res_pool &&
			stream->ctx->dc->res_pool->funcs->remove_stream_from_ctx) {
			if (stream->ctx->dc->res_pool->funcs->remove_stream_from_ctx(
				stream->ctx->dc, dc_state_new, stream) != DC_OK)
				return -EINVAL;
			}

			if (!is_dsc_recompute_needed(state, dc_state_new, stream->link)) {
				computed_for_link[stream->link->link_index] = true;
				continue;
			}

			mst_mgr = aconnector->mst_output_port->mgr;
			if (!mst_mgr)
				continue;

		ret = compute_mst_dsc_configs_for_link(state, dc_state_new, stream->link, vars, mst_mgr,
											   &link_vars_start_index);
		if (ret != 0)
			return ret;

		computed_for_link[stream->link->link_index] = true;
	}

	for (i = 0; i < dc_state_new->stream_count; i++) {
		stream = dc_state_new->streams[i];
		if (stream && stream->timing.flags.DSC == 1) {
			if (dc_stream_add_dsc_to_resource(stream->ctx->dc, dc_state_new, stream) != DC_OK)
				return -EINVAL;
		}
	}
	return 0;
}

static int pre_compute_mst_dsc_configs_for_state(struct drm_atomic_state *state,
												 struct dc_state *dc_state_validate_copy,
												 struct dsc_mst_fairness_vars *vars)
{
	int i, ret = 0;
	struct dc_stream_state *stream;
	bool computed_for_link[MAX_PIPES] = {false};
	struct amdgpu_dm_connector *aconnector;
	struct drm_dp_mst_topology_mgr *mst_mgr;
	int link_vars_start_index = 0;

	if (unlikely(!state || !dc_state_validate_copy || !vars))
		return -EINVAL;

	for (i = 0; i < dc_state_validate_copy->stream_count; i++) {
		stream = dc_state_validate_copy->streams[i];
		if (!stream || stream->signal != SIGNAL_TYPE_DISPLAY_PORT_MST || !stream->link ||
			stream->link->link_index >= MAX_PIPES)
			continue;

		aconnector = (struct amdgpu_dm_connector *)stream->dm_stream_context;
		if (!aconnector || !aconnector->dc_sink || !aconnector->mst_output_port ||
			!aconnector->dc_sink->dsc_caps.dsc_dec_caps.is_dsc_supported)
			continue;

		if (computed_for_link[stream->link->link_index])
			continue;

		if (!is_dsc_recompute_needed(state, dc_state_validate_copy, stream->link)) {
			computed_for_link[stream->link->link_index] = true;
			continue;
		}

		mst_mgr = aconnector->mst_output_port->mgr;
		if (!mst_mgr)
			continue;

		ret = compute_mst_dsc_configs_for_link(state, dc_state_validate_copy, stream->link, vars,
											   mst_mgr, &link_vars_start_index);
		if (ret != 0)
			return ret;

		computed_for_link[stream->link->link_index] = true;
	}
	return 0;
}

static int find_crtc_index_in_state_by_stream(struct drm_atomic_state *state,
											  struct dc_stream_state *stream_to_find)
{
	int i;
	struct drm_crtc *crtc;
	struct drm_crtc_state *new_crtc_drm_state, *old_crtc_drm_state;

	if (unlikely(!state || !stream_to_find))
		return -1;

	for_each_oldnew_crtc_in_state(state, crtc, old_crtc_drm_state, new_crtc_drm_state, i) {
		struct dm_crtc_state *dm_new_crtc_state = to_dm_crtc_state(new_crtc_drm_state);

		if (dm_new_crtc_state && dm_new_crtc_state->stream == stream_to_find)
			return i;
	}
	return -1;
}

static bool __maybe_unused
is_link_to_dschub(struct dc_link *dc_link)
{
	union dpcd_dsc_basic_capabilities *caps;

	if (unlikely(!dc_link))
		return false;

	caps = &dc_link->dpcd_caps.dsc_caps.dsc_basic_caps;

	if (dc_link->type != DC_CONNECTION_TYPE_MST_BRANCH)
		return false;

	return caps->fields.dsc_support.DSC_SUPPORT ||
	caps->fields.dsc_support.DSC_PASSTHROUGH_SUPPORT;
}

int pre_validate_dsc(struct drm_atomic_state *state,
					 struct dm_atomic_state **dm_state_ptr,
					 struct dsc_mst_fairness_vars *vars)
{
	int i, ret = 0;
	struct dm_atomic_state *dm_state;
	struct dc_state *local_dc_state_copy = NULL;

	if (unlikely(!state || !dm_state_ptr || !vars))
		return -EINVAL;

	if (!is_dsc_precompute_needed(state))
		return 0;

	ret = dm_atomic_get_state(state, dm_state_ptr);
	if (ret != 0)
		return ret;
	dm_state = *dm_state_ptr;
	if (unlikely(!dm_state || !dm_state->context))
		return -EINVAL;

	local_dc_state_copy = kvmalloc(sizeof(struct dc_state), GFP_KERNEL);
	if (!local_dc_state_copy)
		return -ENOMEM;
	memcpy(local_dc_state_copy, dm_state->context, sizeof(struct dc_state));
	for (i = 0; i < MAX_PIPES; ++i)
		local_dc_state_copy->streams[i] = NULL;
	local_dc_state_copy->stream_count = 0;

	for (i = 0; i < dm_state->context->stream_count; i++) {
		struct dc_stream_state *original_stream_in_dm_ctx = dm_state->context->streams[i];
		int crtc_idx_in_atomic_state;

		if (!original_stream_in_dm_ctx)
			continue;

		crtc_idx_in_atomic_state = find_crtc_index_in_state_by_stream(state, original_stream_in_dm_ctx);
		if (crtc_idx_in_atomic_state >= 0) {
			struct drm_crtc_state *new_crtc_drm_state = state->crtcs[crtc_idx_in_atomic_state].new_state;
			struct drm_connector *conn_for_crtc;
			struct amdgpu_dm_connector *aconn_for_crtc;
			struct drm_connector_state *drm_new_conn_state;
			struct dm_connector_state *dm_new_conn_state;
			struct dm_crtc_state *dm_old_crtc_state;
			struct dc_stream_state *new_validate_stream;

			if (!new_crtc_drm_state || !new_crtc_drm_state->enable)
				continue;

			conn_for_crtc = amdgpu_dm_find_first_crtc_matching_connector(state, state->crtcs[crtc_idx_in_atomic_state].ptr);
			if (!conn_for_crtc) {
				ret = -EINVAL;
				goto clean_exit_pre_validate;
			}

			aconn_for_crtc = to_amdgpu_dm_connector(conn_for_crtc);
			drm_new_conn_state = drm_atomic_get_new_connector_state(state, &aconn_for_crtc->base);
			if (!drm_new_conn_state) {
				ret = -EINVAL;
				goto clean_exit_pre_validate;
			}

			dm_new_conn_state = to_dm_connector_state(drm_new_conn_state);
			dm_old_crtc_state = to_dm_crtc_state(state->crtcs[crtc_idx_in_atomic_state].old_state);

			new_validate_stream =
			create_validate_stream_for_sink(&aconn_for_crtc->base,
											&new_crtc_drm_state->mode,
								   dm_new_conn_state,
								   dm_old_crtc_state ? dm_old_crtc_state->stream : NULL);
			if (!new_validate_stream) {
				ret = -EINVAL;
				goto clean_exit_pre_validate;
			}
			local_dc_state_copy->streams[local_dc_state_copy->stream_count++] = new_validate_stream;
		}
	}

	ret = pre_compute_mst_dsc_configs_for_state(state, local_dc_state_copy, vars);
	if (ret != 0) {
		ret = -EINVAL;
		goto clean_exit_pre_validate;
	}

	for (i = 0; i < local_dc_state_copy->stream_count; i++) {
		struct dc_stream_state *validated_stream_with_dsc = local_dc_state_copy->streams[i];
		struct dc_stream_state *original_stream_in_dm_ctx = NULL;
		int crtc_idx_in_atomic_state;
		struct amdgpu_dm_connector *aconn_ctx;
		struct drm_connector_state *conn_state_from_atomic;
		struct drm_crtc_state *crtc_s_from_atomic;
		struct dm_crtc_state *dm_s_from_atomic;


		if (!validated_stream_with_dsc || !validated_stream_with_dsc->dm_stream_context)
			continue;

		aconn_ctx = validated_stream_with_dsc->dm_stream_context;
		conn_state_from_atomic = drm_atomic_get_new_connector_state(state, &aconn_ctx->base);

		if (conn_state_from_atomic && conn_state_from_atomic->crtc) {
			crtc_s_from_atomic = drm_atomic_get_new_crtc_state(state, conn_state_from_atomic->crtc);
			dm_s_from_atomic = to_dm_crtc_state(crtc_s_from_atomic);
			original_stream_in_dm_ctx = dm_s_from_atomic ? dm_s_from_atomic->stream : NULL;

			if (original_stream_in_dm_ctx &&
				!dc_is_timing_changed(original_stream_in_dm_ctx, validated_stream_with_dsc)) {
				crtc_idx_in_atomic_state = find_crtc_index_in_state_by_stream(state, original_stream_in_dm_ctx);
			if (crtc_idx_in_atomic_state >= 0 && state->crtcs[crtc_idx_in_atomic_state].new_state)
				state->crtcs[crtc_idx_in_atomic_state].new_state->mode_changed = 0;
				}
		}
	}

	clean_exit_pre_validate:
	if (local_dc_state_copy) {
		for (i = 0; i < local_dc_state_copy->stream_count; i++) {
			if (local_dc_state_copy->streams[i])
				dc_stream_release(local_dc_state_copy->streams[i]);
		}
		kvfree(local_dc_state_copy);
	}
	return ret;
}

static __always_inline uint32_t
kbps_from_pbn(unsigned int pbn)
{
	u64 kbps = pbn;

	kbps *= (1000000ULL / PEAK_FACTOR_X1000);
	kbps *= 8ULL;
	kbps *= 54ULL;
	kbps = div_u64(kbps, 64ULL);
	return (uint32_t)kbps;
}

static bool is_dsc_common_config_possible(struct dc_stream_state *stream,
										  struct dc_dsc_bw_range *bw_range_out)
{
	struct dc_dsc_policy dsc_policy = {0};

	if (unlikely(!stream || !stream->sink || !stream->sink->ctx || !stream->sink->ctx->dc ||
		!stream->sink->ctx->dc->res_pool || !stream->sink->ctx->dc->res_pool->dscs[0] ||
		!bw_range_out))
		return false;

	dc_dsc_get_policy_for_timing(&stream->timing, 0, &dsc_policy,
								 dc_link_get_highest_encoding_format(stream->link));
	dc_dsc_compute_bandwidth_range(
		stream->sink->ctx->dc->res_pool->dscs[0],
		stream->sink->ctx->dc->debug.dsc_min_slice_height_override,
		dsc_policy.min_target_bpp * 16, dsc_policy.max_target_bpp * 16,
		&stream->sink->dsc_caps.dsc_dec_caps,
		&stream->timing, dc_link_get_highest_encoding_format(stream->link),
								   bw_range_out);

	return bw_range_out->max_target_bpp_x16 != 0 && bw_range_out->min_target_bpp_x16 != 0;
}

static bool dp_get_link_current_set_bw(struct drm_dp_aux *aux,
									   u32 *cur_link_bw_kbps_out)
{
	u32 total_data_bw_efficiency_x10000 = 0;
	u32 link_rate_per_lane_kbps = 0;
	enum dc_link_rate link_rate_enum;
	union lane_count_set lane_count = {0};
	u8 dp_link_encoding_set = 0;
	u8 link_bw_set_dpcd = 0;

	if (unlikely(!aux || !cur_link_bw_kbps_out))
		return false;

	*cur_link_bw_kbps_out = 0;

	if (drm_dp_dpcd_readb(aux, DP_MAIN_LINK_CHANNEL_CODING_SET,
		&dp_link_encoding_set) != 1)
		return false;

	if (drm_dp_dpcd_readb(aux, DP_LANE_COUNT_SET,
		&lane_count.raw) != 1)
		return false;

	if (drm_dp_dpcd_readb(aux, DP_LINK_BW_SET,
		&link_bw_set_dpcd) != 1)
		return false;

	if (lane_count.bits.LANE_COUNT_SET == 0 ||
		lane_count.bits.LANE_COUNT_SET > 4)
		return false;

	switch (dp_link_encoding_set) {
		case DP_8b_10b_ENCODING:
			link_rate_enum = (enum dc_link_rate)link_bw_set_dpcd;

			if (link_rate_enum > LINK_RATE_HIGH3)
				return false;
		link_rate_per_lane_kbps = (u32)link_rate_enum *
		LINK_RATE_REF_FREQ_IN_KHZ *
		8;
		total_data_bw_efficiency_x10000 = 8000;
		break;

		case DP_128b_132b_ENCODING:
			switch (link_bw_set_dpcd) {
				case DP_LINK_BW_10:
					link_rate_enum = LINK_RATE_UHBR10;
					break;
				case DP_LINK_BW_13_5:
					link_rate_enum = LINK_RATE_UHBR13_5;
					break;
				case DP_LINK_BW_20:
					link_rate_enum = LINK_RATE_UHBR20;
					break;
				default:
					return false;
			}
			link_rate_per_lane_kbps = (u32)link_rate_enum * 1000000;
			total_data_bw_efficiency_x10000 = (128 * 10000) / 132;
			break;

				default:
					return false;
	}

	*cur_link_bw_kbps_out = div_u64((u64)link_rate_per_lane_kbps *
	lane_count.bits.LANE_COUNT_SET *
	total_data_bw_efficiency_x10000, 10000ULL);

	if (*cur_link_bw_kbps_out > 80000000)
		return false;

	return true;
}
#endif

enum dc_status dm_dp_mst_is_port_support_mode(
	struct amdgpu_dm_connector *aconnector,
	struct dc_stream_state *stream)
{
	#if defined(CONFIG_DRM_AMD_DC_FP)
	u32 stream_kbps_no_dsc;
	u32 root_link_bw_kbps, virtual_channel_to_port_kbps, end_to_end_bottleneck_kbps;
	struct dc_dsc_bw_range dsc_bw_range = {0};
	const struct dc_link_settings *link_settings;

	if (unlikely(!aconnector || !stream || !stream->link || !stream->sink ||
		!aconnector->dc_link || !aconnector->mst_output_port))
		return DC_ERROR_UNEXPECTED;

	link_settings = &aconnector->dc_link->cur_link_settings;

	stream_kbps_no_dsc = dc_bandwidth_in_kbps_from_timing(&stream->timing,
														  dc_link_get_highest_encoding_format(stream->link));

	root_link_bw_kbps = dc_link_bandwidth_kbps(aconnector->dc_link, link_settings);
	virtual_channel_to_port_kbps = kbps_from_pbn(aconnector->mst_output_port->full_pbn);
	end_to_end_bottleneck_kbps = min(root_link_bw_kbps, virtual_channel_to_port_kbps);

	if (stream_kbps_no_dsc <= end_to_end_bottleneck_kbps)
		return DC_OK;

	if (unlikely(!aconnector->dsc_aux))
		return DC_FAIL_BANDWIDTH_VALIDATE;

	if (is_dsc_common_config_possible(stream, &dsc_bw_range)) {
		if (aconnector->mst_output_port->passthrough_aux) {
			if (dsc_bw_range.min_kbps > end_to_end_bottleneck_kbps)
				return DC_FAIL_BANDWIDTH_VALIDATE;
		} else {
			struct drm_dp_mst_port *immediate_upstream_port = NULL;
			u32 last_hop_uncompressed_link_bw_kbps = 0;
			u32 vc_bw_to_this_branch_kbps;

			if (aconnector->mst_output_port->pdt != DP_PEER_DEVICE_DP_LEGACY_CONV &&
				aconnector->mst_output_port->pdt != DP_PEER_DEVICE_NONE) {
				if (aconnector->vc_full_pbn != aconnector->mst_output_port->full_pbn) {
					if (!dp_get_link_current_set_bw(&aconnector->mst_output_port->aux,
						&last_hop_uncompressed_link_bw_kbps))
						last_hop_uncompressed_link_bw_kbps = 0;

					aconnector->vc_full_pbn = aconnector->mst_output_port->full_pbn;
					aconnector->mst_local_bw = last_hop_uncompressed_link_bw_kbps;
				} else {
					last_hop_uncompressed_link_bw_kbps = aconnector->mst_local_bw;
				}

				if (last_hop_uncompressed_link_bw_kbps > 0 &&
					stream_kbps_no_dsc > last_hop_uncompressed_link_bw_kbps)
					return DC_FAIL_BANDWIDTH_VALIDATE;
				}

				if (aconnector->mst_output_port->parent &&
					aconnector->mst_output_port->parent->port_parent)
					immediate_upstream_port = aconnector->mst_output_port->parent->port_parent;

				if (immediate_upstream_port)
					vc_bw_to_this_branch_kbps = kbps_from_pbn(immediate_upstream_port->full_pbn);
			else
				vc_bw_to_this_branch_kbps = root_link_bw_kbps;

			vc_bw_to_this_branch_kbps = min(root_link_bw_kbps, vc_bw_to_this_branch_kbps);

			if (dsc_bw_range.min_kbps > vc_bw_to_this_branch_kbps)
				return DC_FAIL_BANDWIDTH_VALIDATE;
		}

		if (stream->link && stream->link->dc && stream->sink &&
			stream->sink->ctx && stream->sink->ctx->dc &&
			stream->sink->ctx->dc->res_pool &&
			stream->sink->ctx->dc->res_pool->dscs[0]) {
			struct dc_dsc_config_options dsc_options = {0};

		dc_dsc_get_default_config_option(stream->link->dc, &dsc_options);

		if (aconnector->base.display_info.max_dsc_bpp > 0)
			dsc_options.max_target_bpp_limit_override_x16 =
			aconnector->base.display_info.max_dsc_bpp * 16;

		if (dc_dsc_compute_config(stream->sink->ctx->dc->res_pool->dscs[0],
			&stream->sink->dsc_caps.dsc_dec_caps,
			&dsc_options,
			end_to_end_bottleneck_kbps,
			&stream->timing,
			dc_link_get_highest_encoding_format(stream->link),
								  &stream->timing.dsc_cfg)) {
			stream->timing.flags.DSC = 1;
								  } else {
									  return DC_FAIL_BANDWIDTH_VALIDATE;
								  }

								  int branch_max_throughput_mps = 0;

								  switch (stream->timing.pixel_encoding) {
									  case PIXEL_ENCODING_RGB:
									  case PIXEL_ENCODING_YCBCR444:
										  branch_max_throughput_mps =
										  aconnector->dc_sink->dsc_caps.dsc_dec_caps.branch_overall_throughput_0_mps;
										  break;
									  case PIXEL_ENCODING_YCBCR422:
									  case PIXEL_ENCODING_YCBCR420:
										  branch_max_throughput_mps =
										  aconnector->dc_sink->dsc_caps.dsc_dec_caps.branch_overall_throughput_1_mps;
										  break;
									  default:
										  break;
								  }

								  if (branch_max_throughput_mps != 0 &&
									  stream->timing.pix_clk_100hz > 0 &&
									  (div_u64(stream->timing.pix_clk_100hz, 10) > (u32)branch_max_throughput_mps * 1000))
									  return DC_FAIL_BANDWIDTH_VALIDATE;
			} else {
				return DC_FAIL_BANDWIDTH_VALIDATE;
			}
	} else {
		return DC_FAIL_BANDWIDTH_VALIDATE;
	}
	#endif
	return DC_OK;
}
