#include "amdgpu.h"
#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_fb_helper.h>
#include "amdgpu_uvd.h"
#include "amdgpu_vce.h"
#include "atom.h"

#include <linux/overflow.h>
#include <linux/vga_switcheroo.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <linux/pci.h>
#include <linux/pm_runtime.h>
#include "amdgpu_amdkfd.h"
#include "amdgpu_gem.h"
#include "amdgpu_display.h"
#include "amdgpu_ras.h"
#include "amdgpu_reset.h"
#include "amd_pcie.h"

void amdgpu_unregister_gpu_instance(struct amdgpu_device *adev)
{
	struct amdgpu_gpu_instance *gpu_instance;
	int i;

	mutex_lock(&mgpu_info.mutex);

	for (i = 0; i < mgpu_info.num_gpu; ++i) {
		gpu_instance = &mgpu_info.gpu_ins[i];
		if (gpu_instance->adev != adev)
			continue;

		mgpu_info.gpu_ins[i] =
		mgpu_info.gpu_ins[mgpu_info.num_gpu - 1];
		mgpu_info.num_gpu--;

		if (adev->flags & AMD_IS_APU)
			mgpu_info.num_apu--;
		else
			mgpu_info.num_dgpu--;
		break;
	}

	mutex_unlock(&mgpu_info.mutex);
}

void amdgpu_driver_unload_kms(struct drm_device *dev)
{
	struct amdgpu_device *adev = drm_to_adev(dev);

	if (adev == NULL)
		return;

	tbo_cache_cleanup();

	amdgpu_unregister_gpu_instance(adev);

	if (adev->rmmio == NULL)
		return;

	if (amdgpu_acpi_smart_shift_update(dev, AMDGPU_SS_DRV_UNLOAD))
		DRM_WARN("smart shift update failed\n");

	amdgpu_acpi_fini(adev);
	amdgpu_device_fini_hw(adev);
}

void amdgpu_register_gpu_instance(struct amdgpu_device *adev)
{
	struct amdgpu_gpu_instance *gpu_instance;

	mutex_lock(&mgpu_info.mutex);

	if (mgpu_info.num_gpu >= MAX_GPU_INSTANCE) {
		DRM_ERROR("Cannot register more gpu instance\n");
		mutex_unlock(&mgpu_info.mutex);
		return;
	}

	gpu_instance = &(mgpu_info.gpu_ins[mgpu_info.num_gpu]);
	gpu_instance->adev = adev;
	gpu_instance->mgpu_fan_enabled = 0;

	mgpu_info.num_gpu++;
	if (adev->flags & AMD_IS_APU)
		mgpu_info.num_apu++;
	else
		mgpu_info.num_dgpu++;

	mutex_unlock(&mgpu_info.mutex);
}

int amdgpu_driver_load_kms(struct amdgpu_device *adev, unsigned long flags)
{
	struct drm_device *dev;
	int r, acpi_status;

	dev = adev_to_drm(adev);

	r = amdgpu_device_init(adev, flags);
	if (r) {
		dev_err(dev->dev, "Fatal error during GPU init\n");
		goto out;
	}

	if (adev->asic_type == CHIP_VEGA10) {
		amdgpu_gem_static_branch_init(adev);
		amdgpu_vega_vram_thresholds_init();
		r = tbo_cache_init();
		if (r)
			dev_warn(dev->dev, "Failed to initialize TBO cache (%d)\n", r);
	}

	amdgpu_device_detect_runtime_pm_mode(adev);

	acpi_status = amdgpu_acpi_init(adev);
	if (acpi_status)
		dev_dbg(dev->dev, "Error during ACPI methods call\n");

	if (amdgpu_acpi_smart_shift_update(dev, AMDGPU_SS_DRV_LOAD))
		DRM_WARN("smart shift update failed\n");

	out:
	if (r)
		amdgpu_driver_unload_kms(dev);

	return r;
}

static enum amd_ip_block_type
amdgpu_ip_get_block_type(struct amdgpu_device *adev, uint32_t ip)
{
	enum amd_ip_block_type type;

	switch (ip) {
		case AMDGPU_HW_IP_GFX:
			type = AMD_IP_BLOCK_TYPE_GFX;
			break;
		case AMDGPU_HW_IP_COMPUTE:
			type = AMD_IP_BLOCK_TYPE_GFX;
			break;
		case AMDGPU_HW_IP_DMA:
			type = AMD_IP_BLOCK_TYPE_SDMA;
			break;
		case AMDGPU_HW_IP_UVD:
		case AMDGPU_HW_IP_UVD_ENC:
			type = AMD_IP_BLOCK_TYPE_UVD;
			break;
		case AMDGPU_HW_IP_VCE:
			type = AMD_IP_BLOCK_TYPE_VCE;
			break;
		case AMDGPU_HW_IP_VCN_DEC:
		case AMDGPU_HW_IP_VCN_ENC:
			type = AMD_IP_BLOCK_TYPE_VCN;
			break;
		case AMDGPU_HW_IP_VCN_JPEG:
			type = (amdgpu_device_ip_get_ip_block(adev, AMD_IP_BLOCK_TYPE_JPEG)) ?
			AMD_IP_BLOCK_TYPE_JPEG : AMD_IP_BLOCK_TYPE_VCN;
			break;
		default:
			type = AMD_IP_BLOCK_TYPE_NUM;
			break;
	}

	return type;
}

static int amdgpu_firmware_info(struct drm_amdgpu_info_firmware *fw_info,
								struct drm_amdgpu_query_fw *query_fw,
								struct amdgpu_device *adev)
{
	switch (query_fw->fw_type) {
		case AMDGPU_INFO_FW_VCE:
			fw_info->ver = adev->vce.fw_version;
			fw_info->feature = adev->vce.fb_version;
			break;
		case AMDGPU_INFO_FW_UVD:
			fw_info->ver = adev->uvd.fw_version;
			fw_info->feature = 0;
			break;
		case AMDGPU_INFO_FW_VCN:
			fw_info->ver = adev->vcn.fw_version;
			fw_info->feature = 0;
			break;
		case AMDGPU_INFO_FW_GMC:
			fw_info->ver = adev->gmc.fw_version;
			fw_info->feature = 0;
			break;
		case AMDGPU_INFO_FW_GFX_ME:
			fw_info->ver = adev->gfx.me_fw_version;
			fw_info->feature = adev->gfx.me_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_PFP:
			fw_info->ver = adev->gfx.pfp_fw_version;
			fw_info->feature = adev->gfx.pfp_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_CE:
			fw_info->ver = adev->gfx.ce_fw_version;
			fw_info->feature = adev->gfx.ce_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLC:
			fw_info->ver = adev->gfx.rlc_fw_version;
			fw_info->feature = adev->gfx.rlc_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_CNTL:
			fw_info->ver = adev->gfx.rlc_srlc_fw_version;
			fw_info->feature = adev->gfx.rlc_srlc_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_GPM_MEM:
			fw_info->ver = adev->gfx.rlc_srlg_fw_version;
			fw_info->feature = adev->gfx.rlc_srlg_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_SRM_MEM:
			fw_info->ver = adev->gfx.rlc_srls_fw_version;
			fw_info->feature = adev->gfx.rlc_srls_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLCP:
			fw_info->ver = adev->gfx.rlcp_ucode_version;
			fw_info->feature = adev->gfx.rlcp_ucode_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_RLCV:
			fw_info->ver = adev->gfx.rlcv_ucode_version;
			fw_info->feature = adev->gfx.rlcv_ucode_feature_version;
			break;
		case AMDGPU_INFO_FW_GFX_MEC:
			if (query_fw->index == 0) {
				fw_info->ver = adev->gfx.mec_fw_version;
				fw_info->feature = adev->gfx.mec_feature_version;
			} else if (query_fw->index == 1) {
				fw_info->ver = adev->gfx.mec2_fw_version;
				fw_info->feature = adev->gfx.mec2_feature_version;
			} else {
				return -EINVAL;
			}
			break;
		case AMDGPU_INFO_FW_SMC:
			fw_info->ver = adev->pm.fw_version;
			fw_info->feature = 0;
			break;
		case AMDGPU_INFO_FW_TA:
			switch (query_fw->index) {
				case TA_FW_TYPE_PSP_XGMI:
					fw_info->ver = adev->psp.xgmi_context.context.bin_desc.fw_version;
					fw_info->feature = adev->psp.xgmi_context.context
					.bin_desc.feature_version;
					break;
				case TA_FW_TYPE_PSP_RAS:
					fw_info->ver = adev->psp.ras_context.context.bin_desc.fw_version;
					fw_info->feature = adev->psp.ras_context.context
					.bin_desc.feature_version;
					break;
				case TA_FW_TYPE_PSP_HDCP:
					fw_info->ver = adev->psp.hdcp_context.context.bin_desc.fw_version;
					fw_info->feature = adev->psp.hdcp_context.context
					.bin_desc.feature_version;
					break;
				case TA_FW_TYPE_PSP_DTM:
					fw_info->ver = adev->psp.dtm_context.context.bin_desc.fw_version;
					fw_info->feature = adev->psp.dtm_context.context
					.bin_desc.feature_version;
					break;
				case TA_FW_TYPE_PSP_RAP:
					fw_info->ver = adev->psp.rap_context.context.bin_desc.fw_version;
					fw_info->feature = adev->psp.rap_context.context
					.bin_desc.feature_version;
					break;
				case TA_FW_TYPE_PSP_SECUREDISPLAY:
					fw_info->ver = adev->psp.securedisplay_context.context.bin_desc.fw_version;
					fw_info->feature =
					adev->psp.securedisplay_context.context.bin_desc
					.feature_version;
					break;
				default:
					return -EINVAL;
			}
			break;
				case AMDGPU_INFO_FW_SDMA:
					if (query_fw->index >= adev->sdma.num_instances)
						return -EINVAL;
		fw_info->ver = adev->sdma.instance[query_fw->index].fw_version;
		fw_info->feature = adev->sdma.instance[query_fw->index].feature_version;
		break;
				case AMDGPU_INFO_FW_SOS:
					fw_info->ver = adev->psp.sos.fw_version;
					fw_info->feature = adev->psp.sos.feature_version;
					break;
				case AMDGPU_INFO_FW_ASD:
					fw_info->ver = adev->psp.asd_context.bin_desc.fw_version;
					fw_info->feature = adev->psp.asd_context.bin_desc.feature_version;
					break;
				case AMDGPU_INFO_FW_DMCU:
					fw_info->ver = adev->dm.dmcu_fw_version;
					fw_info->feature = 0;
					break;
				case AMDGPU_INFO_FW_DMCUB:
					fw_info->ver = adev->dm.dmcub_fw_version;
					fw_info->feature = 0;
					break;
				case AMDGPU_INFO_FW_TOC:
					fw_info->ver = adev->psp.toc.fw_version;
					fw_info->feature = adev->psp.toc.feature_version;
					break;
				case AMDGPU_INFO_FW_CAP:
					fw_info->ver = adev->psp.cap_fw_version;
					fw_info->feature = adev->psp.cap_feature_version;
					break;
				case AMDGPU_INFO_FW_MES_KIQ:
					fw_info->ver = adev->mes.kiq_version & AMDGPU_MES_VERSION_MASK;
					fw_info->feature = (adev->mes.kiq_version & AMDGPU_MES_FEAT_VERSION_MASK)
					>> AMDGPU_MES_FEAT_VERSION_SHIFT;
					break;
				case AMDGPU_INFO_FW_MES:
					fw_info->ver = adev->mes.sched_version & AMDGPU_MES_VERSION_MASK;
					fw_info->feature = (adev->mes.sched_version & AMDGPU_MES_FEAT_VERSION_MASK)
					>> AMDGPU_MES_FEAT_VERSION_SHIFT;
					break;
				case AMDGPU_INFO_FW_IMU:
					fw_info->ver = adev->gfx.imu_fw_version;
					fw_info->feature = 0;
					break;
				case AMDGPU_INFO_FW_VPE:
					fw_info->ver = adev->vpe.fw_version;
					fw_info->feature = adev->vpe.feature_version;
					break;
				default:
					return -EINVAL;
	}
	return 0;
}

static int amdgpu_hw_ip_info(struct amdgpu_device *adev,
							 struct drm_amdgpu_info *info,
							 struct drm_amdgpu_info_hw_ip *result)
{
	uint32_t ib_start_alignment = 0;
	uint32_t ib_size_alignment = 0;
	enum amd_ip_block_type type;
	unsigned int num_rings = 0;
	unsigned int i, j;

	if (info->query_hw_ip.ip_instance >= AMDGPU_HW_IP_INSTANCE_MAX_COUNT)
		return -EINVAL;

	switch (info->query_hw_ip.type) {
		case AMDGPU_HW_IP_GFX:
			type = AMD_IP_BLOCK_TYPE_GFX;
			for (i = 0; i < adev->gfx.num_gfx_rings; i++) {
				if (adev->gfx.gfx_ring[i].sched.ready)
					++num_rings;
			}
			ib_start_alignment = 32;
			ib_size_alignment = 32;
			break;
		case AMDGPU_HW_IP_COMPUTE:
			type = AMD_IP_BLOCK_TYPE_GFX;
			for (i = 0; i < adev->gfx.num_compute_rings; i++) {
				if (adev->gfx.compute_ring[i].sched.ready)
					++num_rings;
			}
			ib_start_alignment = 32;
			ib_size_alignment = 32;
			break;
		case AMDGPU_HW_IP_DMA:
			type = AMD_IP_BLOCK_TYPE_SDMA;
			for (i = 0; i < adev->sdma.num_instances; i++) {
				if (adev->sdma.instance[i].ring.sched.ready)
					++num_rings;
			}
			ib_start_alignment = 256;
			ib_size_alignment = 4;
			break;
		case AMDGPU_HW_IP_UVD:
			type = AMD_IP_BLOCK_TYPE_UVD;
			for (i = 0; i < adev->uvd.num_uvd_inst; i++) {
				if (adev->uvd.harvest_config & (1 << i))
					continue;

				if (adev->uvd.inst[i].ring.sched.ready)
					++num_rings;
			}
			ib_start_alignment = 256;
			ib_size_alignment = 64;
			break;
		case AMDGPU_HW_IP_VCE:
			type = AMD_IP_BLOCK_TYPE_VCE;
			for (i = 0; i < adev->vce.num_rings; i++) {
				if (adev->vce.ring[i].sched.ready)
					++num_rings;
			}
			ib_start_alignment = 256;
			ib_size_alignment = 4;
			break;
		case AMDGPU_HW_IP_UVD_ENC:
			type = AMD_IP_BLOCK_TYPE_UVD;
			for (i = 0; i < adev->uvd.num_uvd_inst; i++) {
				if (adev->uvd.harvest_config & (1 << i))
					continue;

				for (j = 0; j < adev->uvd.num_enc_rings; j++) {
					if (adev->uvd.inst[i].ring_enc[j].sched.ready)
						++num_rings;
				}
			}
			ib_start_alignment = 256;
			ib_size_alignment = 4;
			break;
		case AMDGPU_HW_IP_VCN_DEC:
			type = AMD_IP_BLOCK_TYPE_VCN;
			for (i = 0; i < adev->vcn.num_vcn_inst; i++) {
				if (adev->vcn.harvest_config & (1 << i))
					continue;

				if (adev->vcn.inst[i].ring_dec.sched.ready)
					++num_rings;
			}
			ib_start_alignment = 256;
			ib_size_alignment = 64;
			break;
		case AMDGPU_HW_IP_VCN_ENC:
			type = AMD_IP_BLOCK_TYPE_VCN;
			for (i = 0; i < adev->vcn.num_vcn_inst; i++) {
				if (adev->vcn.harvest_config & (1 << i))
					continue;

				for (j = 0; j < adev->vcn.num_enc_rings; j++) {
					if (adev->vcn.inst[i].ring_enc[j].sched.ready)
						++num_rings;
				}
			}
			ib_start_alignment = 256;
			ib_size_alignment = 4;
			break;
		case AMDGPU_HW_IP_VCN_JPEG:
			type = (amdgpu_device_ip_get_ip_block(adev, AMD_IP_BLOCK_TYPE_JPEG)) ?
			AMD_IP_BLOCK_TYPE_JPEG : AMD_IP_BLOCK_TYPE_VCN;

			for (i = 0; i < adev->jpeg.num_jpeg_inst; i++) {
				if (adev->jpeg.harvest_config & (1 << i))
					continue;

				for (j = 0; j < adev->jpeg.num_jpeg_rings; j++) {
					if (adev->jpeg.inst[i].ring_dec[j].sched.ready)
						++num_rings;
				}
			}
			ib_start_alignment = 256;
			ib_size_alignment = 64;
			break;
		case AMDGPU_HW_IP_VPE:
			type = AMD_IP_BLOCK_TYPE_VPE;
			if (adev->vpe.ring.sched.ready)
				++num_rings;
		ib_start_alignment = 256;
		ib_size_alignment = 4;
		break;
		default:
			return -EINVAL;
	}

	for (i = 0; i < adev->num_ip_blocks; i++) {
		if (adev->ip_blocks[i].version->type == type &&
			adev->ip_blocks[i].status.valid)
			break;
	}

	if (i == adev->num_ip_blocks)
		return 0;

	num_rings = min(amdgpu_ctx_num_entities[info->query_hw_ip.type],
					num_rings);

	result->hw_ip_version_major = adev->ip_blocks[i].version->major;
	result->hw_ip_version_minor = adev->ip_blocks[i].version->minor;

	if (adev->asic_type >= CHIP_VEGA10) {
		switch (type) {
			case AMD_IP_BLOCK_TYPE_GFX:
				result->ip_discovery_version =
				IP_VERSION_MAJ_MIN_REV(amdgpu_ip_version(adev, GC_HWIP, 0));
				break;
			case AMD_IP_BLOCK_TYPE_SDMA:
				result->ip_discovery_version =
				IP_VERSION_MAJ_MIN_REV(amdgpu_ip_version(adev, SDMA0_HWIP, 0));
				break;
			case AMD_IP_BLOCK_TYPE_UVD:
			case AMD_IP_BLOCK_TYPE_VCN:
			case AMD_IP_BLOCK_TYPE_JPEG:
				result->ip_discovery_version =
				IP_VERSION_MAJ_MIN_REV(amdgpu_ip_version(adev, UVD_HWIP, 0));
				break;
			case AMD_IP_BLOCK_TYPE_VCE:
				result->ip_discovery_version =
				IP_VERSION_MAJ_MIN_REV(amdgpu_ip_version(adev, VCE_HWIP, 0));
				break;
			case AMD_IP_BLOCK_TYPE_VPE:
				result->ip_discovery_version =
				IP_VERSION_MAJ_MIN_REV(amdgpu_ip_version(adev, VPE_HWIP, 0));
				break;
			default:
				result->ip_discovery_version = 0;
				break;
		}
	} else {
		result->ip_discovery_version = 0;
	}
	result->capabilities_flags = 0;
	result->available_rings = (1 << num_rings) - 1;
	result->ib_start_alignment = ib_start_alignment;
	result->ib_size_alignment = ib_size_alignment;
	return 0;
}

static int amdgpu_ioctl_read_mmr(struct amdgpu_device      *adev,
								 struct drm_amdgpu_info    *info,
								 void __user               *out,
								 u32                        user_size)
{
	uint32_t  se_idx, sh_idx;
	size_t    bytes;
	uint32_t *regs = NULL;
	int       i, ret = 0;

	if (!info->read_mmr_reg.count ||
		info->read_mmr_reg.count > 128)
		return -EINVAL;

	if (check_mul_overflow((size_t)info->read_mmr_reg.count,
		sizeof(*regs), &bytes))
		return -E2BIG;

	if (user_size < bytes)
		return -EINVAL;

	regs = kcalloc(info->read_mmr_reg.count, sizeof(*regs), GFP_KERNEL);
	if (!regs)
		return -ENOMEM;

	se_idx = (info->read_mmr_reg.instance >>
	AMDGPU_INFO_MMR_SE_INDEX_SHIFT) &
	AMDGPU_INFO_MMR_SE_INDEX_MASK;

	sh_idx = (info->read_mmr_reg.instance >>
	AMDGPU_INFO_MMR_SH_INDEX_SHIFT) &
	AMDGPU_INFO_MMR_SH_INDEX_MASK;

	if (se_idx == AMDGPU_INFO_MMR_SE_INDEX_MASK) {
		se_idx = 0xffffffff;
	} else if (se_idx >= AMDGPU_GFX_MAX_SE) {
		ret = -EINVAL;
		goto out_free;
	}

	if (sh_idx == AMDGPU_INFO_MMR_SH_INDEX_MASK) {
		sh_idx = 0xffffffff;
	} else if (sh_idx >= AMDGPU_GFX_MAX_SH_PER_SE) {
		ret = -EINVAL;
		goto out_free;
	}

	if (!down_read_trylock(&adev->reset_domain->sem)) {
		ret = -ENOENT;
		goto out_free;
	}

	amdgpu_gfx_off_ctrl(adev, false);
	for (i = 0; i < info->read_mmr_reg.count; ++i) {
		ret = amdgpu_asic_read_register(
			adev, se_idx, sh_idx,
			info->read_mmr_reg.dword_offset + i, &regs[i]);
		if (ret) {
			DRM_DEBUG_KMS("unallowed offset %#x\n",
						  info->read_mmr_reg.dword_offset + i);
			ret = -EFAULT;
			break;
		}
	}
	amdgpu_gfx_off_ctrl(adev, true);

	up_read(&adev->reset_domain->sem);

	if (!ret && copy_to_user(out, regs, bytes))
		ret = -EFAULT;

	out_free:
	kfree(regs);
	return ret;
}

int amdgpu_info_ioctl(struct drm_device *dev, void *data, struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct drm_amdgpu_info *info = data;
	struct amdgpu_mode_info *minfo = &adev->mode_info;
	void __user *out = (void __user *)(uintptr_t)info->return_pointer;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_ip_block *ip_block;
	enum amd_ip_block_type type;
	struct amdgpu_xcp *xcp;
	u32 count, inst_mask;
	uint32_t user_ret_size = info->return_size;
	struct drm_crtc *crtc;
	uint32_t ui32 = 0;
	uint64_t ui64 = 0;
	int i, found, ret;
	int ui32_size = sizeof(ui32);

	if (!info->return_size || !info->return_pointer)
		return -EINVAL;

	switch (info->query) {
		case AMDGPU_INFO_ACCEL_WORKING:
			if (user_ret_size < sizeof(ui32))
				return -EINVAL;
		ui32 = adev->accel_working;
		return copy_to_user(out, &ui32, sizeof(ui32)) ? -EFAULT : 0;
		case AMDGPU_INFO_CRTC_FROM_ID:
			if (user_ret_size < sizeof(ui32))
				return -EINVAL;
		for (i = 0, found = 0; i < adev->mode_info.num_crtc; i++) {
			crtc = (struct drm_crtc *)minfo->crtcs[i];
			if (crtc && crtc->base.id == info->mode_crtc.id) {
				struct amdgpu_crtc *amdgpu_crtc = to_amdgpu_crtc(crtc);

				ui32 = amdgpu_crtc->crtc_id;
				found = 1;
				break;
			}
		}
		if (!found) {
			DRM_DEBUG_KMS("unknown crtc id %d\n", info->mode_crtc.id);
			return -EINVAL;
		}
		return copy_to_user(out, &ui32, sizeof(ui32)) ? -EFAULT : 0;
		case AMDGPU_INFO_HW_IP_INFO: {
			struct drm_amdgpu_info_hw_ip ip = {};

			if (user_ret_size < sizeof(ip))
				return -EINVAL;
			ret = amdgpu_hw_ip_info(adev, info, &ip);
			if (ret)
				return ret;

			ret = copy_to_user(out, &ip, sizeof(ip));
			return ret ? -EFAULT : 0;
		}
		case AMDGPU_INFO_HW_IP_COUNT: {
			if (user_ret_size < sizeof(u32))
				return -EINVAL;
			fpriv = (struct amdgpu_fpriv *)filp->driver_priv;
			type = amdgpu_ip_get_block_type(adev, info->query_hw_ip.type);
			ip_block = amdgpu_device_ip_get_ip_block(adev, type);

			if (!ip_block || !ip_block->status.valid)
				return -EINVAL;

			if (adev->xcp_mgr && adev->xcp_mgr->num_xcps > 0 &&
				fpriv->xcp_id < adev->xcp_mgr->num_xcps) {
				xcp = &adev->xcp_mgr->xcp[fpriv->xcp_id];
			switch (type) {
				case AMD_IP_BLOCK_TYPE_GFX:
					ret = amdgpu_xcp_get_inst_details(xcp, AMDGPU_XCP_GFX, &inst_mask);
					if (ret)
						return ret;
				count = hweight32(inst_mask);
				break;
				case AMD_IP_BLOCK_TYPE_SDMA:
					ret = amdgpu_xcp_get_inst_details(xcp, AMDGPU_XCP_SDMA, &inst_mask);
					if (ret)
						return ret;
				count = hweight32(inst_mask);
				break;
				case AMD_IP_BLOCK_TYPE_JPEG:
					ret = amdgpu_xcp_get_inst_details(xcp, AMDGPU_XCP_VCN, &inst_mask);
					if (ret)
						return ret;
				count = hweight32(inst_mask) * adev->jpeg.num_jpeg_rings;
				break;
				case AMD_IP_BLOCK_TYPE_VCN:
					ret = amdgpu_xcp_get_inst_details(xcp, AMDGPU_XCP_VCN, &inst_mask);
					if (ret)
						return ret;
				count = hweight32(inst_mask);
				break;
				default:
					return -EINVAL;
			}

			return copy_to_user(out, &count, sizeof(count)) ? -EFAULT : 0;
				}

				switch (type) {
					case AMD_IP_BLOCK_TYPE_GFX:
					case AMD_IP_BLOCK_TYPE_VCE:
						count = 1;
						break;
					case AMD_IP_BLOCK_TYPE_SDMA:
						count = adev->sdma.num_instances;
						break;
					case AMD_IP_BLOCK_TYPE_JPEG:
						count = adev->jpeg.num_jpeg_inst * adev->jpeg.num_jpeg_rings;
						break;
					case AMD_IP_BLOCK_TYPE_VCN:
						count = adev->vcn.num_vcn_inst;
						break;
					case AMD_IP_BLOCK_TYPE_UVD:
						count = adev->uvd.num_uvd_inst;
						break;
					default:
						count = 1;
						break;
				}
				return copy_to_user(out, &count, sizeof(count)) ? -EFAULT : 0;
		}
					case AMDGPU_INFO_TIMESTAMP:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = amdgpu_gfx_get_gpu_clock_counter(adev);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_FW_VERSION: {
						struct drm_amdgpu_info_firmware fw_info;

						if (user_ret_size < sizeof(fw_info))
							return -EINVAL;

						if (info->query_fw.ip_instance != 0)
							return -EINVAL;

						ret = amdgpu_firmware_info(&fw_info, &info->query_fw, adev);
						if (ret)
							return ret;

						return copy_to_user(out, &fw_info, sizeof(fw_info)) ? -EFAULT : 0;
					}
					case AMDGPU_INFO_NUM_BYTES_MOVED:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = atomic64_read(&adev->num_bytes_moved);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_NUM_EVICTIONS:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = atomic64_read(&adev->num_evictions);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_NUM_VRAM_CPU_PAGE_FAULTS:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = atomic64_read(&adev->num_vram_cpu_page_faults);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_VRAM_USAGE:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = ttm_resource_manager_usage(&adev->mman.vram_mgr.manager);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_VIS_VRAM_USAGE:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = amdgpu_vram_mgr_vis_usage(&adev->mman.vram_mgr);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_GTT_USAGE:
						if (user_ret_size < sizeof(ui64))
							return -EINVAL;
		ui64 = ttm_resource_manager_usage(&adev->mman.gtt_mgr.manager);
		return copy_to_user(out, &ui64, sizeof(ui64)) ? -EFAULT : 0;
					case AMDGPU_INFO_GDS_CONFIG: {
						struct drm_amdgpu_info_gds gds_info;

						if (user_ret_size < sizeof(gds_info))
							return -EINVAL;
						memset(&gds_info, 0, sizeof(gds_info));
						gds_info.compute_partition_size = adev->gds.gds_size;
						gds_info.gds_total_size = adev->gds.gds_size;
						gds_info.gws_per_compute_partition = adev->gds.gws_size;
						gds_info.oa_per_compute_partition = adev->gds.oa_size;
						return copy_to_user(out, &gds_info, sizeof(gds_info)) ? -EFAULT : 0;
					}
					case AMDGPU_INFO_VRAM_GTT: {
						struct drm_amdgpu_info_vram_gtt vram_gtt;

						if (user_ret_size < sizeof(vram_gtt))
							return -EINVAL;
						vram_gtt.vram_size = adev->gmc.real_vram_size -
						atomic64_read(&adev->vram_pin_size) -
						AMDGPU_VM_RESERVED_VRAM;
						vram_gtt.vram_cpu_accessible_size =
						min(adev->gmc.visible_vram_size -
						atomic64_read(&adev->visible_pin_size),
							vram_gtt.vram_size);
						vram_gtt.gtt_size = ttm_manager_type(&adev->mman.bdev, TTM_PL_TT)->size;
						vram_gtt.gtt_size -= atomic64_read(&adev->gart_pin_size);
						return copy_to_user(out, &vram_gtt, sizeof(vram_gtt)) ? -EFAULT : 0;
					}
					case AMDGPU_INFO_MEMORY: {
						struct drm_amdgpu_memory_info mem;
						struct ttm_resource_manager *gtt_man =
						&adev->mman.gtt_mgr.manager;
						struct ttm_resource_manager *vram_man =
						&adev->mman.vram_mgr.manager;

						if (user_ret_size < sizeof(mem))
							return -EINVAL;
						memset(&mem, 0, sizeof(mem));
						mem.vram.total_heap_size = adev->gmc.real_vram_size;
						mem.vram.usable_heap_size = adev->gmc.real_vram_size -
						atomic64_read(&adev->vram_pin_size) -
						AMDGPU_VM_RESERVED_VRAM;
						mem.vram.heap_usage =
						ttm_resource_manager_usage(vram_man);
						mem.vram.max_allocation = mem.vram.usable_heap_size * 3 / 4;

						mem.cpu_accessible_vram.total_heap_size =
						adev->gmc.visible_vram_size;
						mem.cpu_accessible_vram.usable_heap_size =
						min(adev->gmc.visible_vram_size -
						atomic64_read(&adev->visible_pin_size),
							mem.vram.usable_heap_size);
						mem.cpu_accessible_vram.heap_usage =
						amdgpu_vram_mgr_vis_usage(&adev->mman.vram_mgr);
						mem.cpu_accessible_vram.max_allocation =
						mem.cpu_accessible_vram.usable_heap_size * 3 / 4;

						mem.gtt.total_heap_size = gtt_man->size;
						mem.gtt.usable_heap_size = mem.gtt.total_heap_size -
						atomic64_read(&adev->gart_pin_size);
						mem.gtt.heap_usage = ttm_resource_manager_usage(gtt_man);
						mem.gtt.max_allocation = mem.gtt.usable_heap_size * 3 / 4;

						return copy_to_user(out, &mem, sizeof(mem)) ? -EFAULT : 0;
					}
					case AMDGPU_INFO_READ_MMR_REG:
						return amdgpu_ioctl_read_mmr(adev, info, out, user_ret_size);
					case AMDGPU_INFO_DEV_INFO: {
						struct drm_amdgpu_info_device *dev_info;
						uint64_t vm_size;
						uint32_t pcie_gen_mask, pcie_width_mask;

						if (user_ret_size < sizeof(*dev_info))
							return -EINVAL;

						dev_info = kzalloc(sizeof(*dev_info), GFP_KERNEL);
						if (!dev_info)
							return -ENOMEM;

						dev_info->device_id = adev->pdev->device;
						dev_info->chip_rev = adev->rev_id;
						dev_info->external_rev = adev->external_rev_id;
						dev_info->pci_rev = adev->pdev->revision;
						dev_info->family = adev->family;
						dev_info->num_shader_engines = adev->gfx.config.max_shader_engines;
						dev_info->num_shader_arrays_per_engine = adev->gfx.config.max_sh_per_se;
						dev_info->gpu_counter_freq = amdgpu_asic_get_xclk(adev) * 10;
						if (adev->pm.dpm_enabled) {
							dev_info->max_engine_clock = amdgpu_dpm_get_sclk(adev, false) * 10;
							dev_info->max_memory_clock = amdgpu_dpm_get_mclk(adev, false) * 10;
							dev_info->min_engine_clock = amdgpu_dpm_get_sclk(adev, true) * 10;
							dev_info->min_memory_clock = amdgpu_dpm_get_mclk(adev, true) * 10;
						} else {
							dev_info->max_engine_clock =
							dev_info->min_engine_clock =
							adev->clock.default_sclk * 10;
							dev_info->max_memory_clock =
							dev_info->min_memory_clock =
							adev->clock.default_mclk * 10;
						}
						dev_info->enabled_rb_pipes_mask = adev->gfx.config.backend_enable_mask;
						dev_info->num_rb_pipes = adev->gfx.config.max_backends_per_se *
						adev->gfx.config.max_shader_engines;
						dev_info->num_hw_gfx_contexts = adev->gfx.config.max_hw_contexts;
						dev_info->ids_flags = 0;
						if (adev->flags & AMD_IS_APU)
							dev_info->ids_flags |= AMDGPU_IDS_FLAGS_FUSION;
						if (adev->gfx.mcbp)
							dev_info->ids_flags |= AMDGPU_IDS_FLAGS_PREEMPTION;
						if (amdgpu_is_tmz(adev))
							dev_info->ids_flags |= AMDGPU_IDS_FLAGS_TMZ;
						if (adev->gfx.config.ta_cntl2_truncate_coord_mode)
							dev_info->ids_flags |= AMDGPU_IDS_FLAGS_CONFORMANT_TRUNC_COORD;

						vm_size = adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
						vm_size -= AMDGPU_VA_RESERVED_TOP;

						if (adev->vce.fw_version &&
							adev->vce.fw_version < AMDGPU_VCE_FW_53_45)
							vm_size = min(vm_size, 1ULL << 40);

						dev_info->virtual_address_offset = AMDGPU_VA_RESERVED_BOTTOM;
						dev_info->virtual_address_max =
						min(vm_size, AMDGPU_GMC_HOLE_START);

						if (vm_size > AMDGPU_GMC_HOLE_START) {
							dev_info->high_va_offset = AMDGPU_GMC_HOLE_END;
							dev_info->high_va_max = AMDGPU_GMC_HOLE_END | vm_size;
						}
						dev_info->virtual_address_alignment = max_t(u32, PAGE_SIZE, AMDGPU_GPU_PAGE_SIZE);
						dev_info->pte_fragment_size = (1 << adev->vm_manager.fragment_size) * AMDGPU_GPU_PAGE_SIZE;
						dev_info->gart_page_size = max_t(u32, PAGE_SIZE, AMDGPU_GPU_PAGE_SIZE);
						dev_info->cu_active_number = adev->gfx.cu_info.number;
						dev_info->cu_ao_mask = adev->gfx.cu_info.ao_cu_mask;
						dev_info->ce_ram_size = adev->gfx.ce_ram_size;
						memcpy(&dev_info->cu_ao_bitmap[0], &adev->gfx.cu_info.ao_cu_bitmap[0],
							   sizeof(adev->gfx.cu_info.ao_cu_bitmap));
						memcpy(&dev_info->cu_bitmap[0], &adev->gfx.cu_info.bitmap[0],
							   sizeof(dev_info->cu_bitmap));
						dev_info->vram_type = adev->gmc.vram_type;
						dev_info->vram_bit_width = adev->gmc.vram_width;
						dev_info->vce_harvest_config = adev->vce.harvest_config;
						dev_info->gc_double_offchip_lds_buf =
						adev->gfx.config.double_offchip_lds_buf;
						dev_info->wave_front_size = adev->gfx.cu_info.wave_front_size;
						dev_info->num_shader_visible_vgprs = adev->gfx.config.max_gprs;
						dev_info->num_cu_per_sh = adev->gfx.config.max_cu_per_sh;
						dev_info->num_tcc_blocks = adev->gfx.config.max_texture_channel_caches;
						dev_info->gs_vgt_table_depth = adev->gfx.config.gs_vgt_table_depth;
						dev_info->gs_prim_buffer_depth = adev->gfx.config.gs_prim_buffer_depth;
						dev_info->max_gs_waves_per_vgt = adev->gfx.config.max_gs_threads;

						if (adev->family >= AMDGPU_FAMILY_NV)
							dev_info->pa_sc_tile_steering_override =
							adev->gfx.config.pa_sc_tile_steering_override;

						dev_info->tcc_disabled_mask = adev->gfx.config.tcc_disabled_mask;

						pcie_gen_mask = adev->pm.pcie_gen_mask &
						(adev->pm.pcie_gen_mask >> CAIL_PCIE_LINK_SPEED_SUPPORT_SHIFT);
						pcie_width_mask = adev->pm.pcie_mlw_mask &
						(adev->pm.pcie_mlw_mask >> CAIL_PCIE_LINK_WIDTH_SUPPORT_SHIFT);
						dev_info->pcie_gen = fls(pcie_gen_mask);
						dev_info->pcie_num_lanes =
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X32 ? 32 :
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X16 ? 16 :
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X12 ? 12 :
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X8 ? 8 :
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X4 ? 4 :
						pcie_width_mask & CAIL_ASIC_PCIE_LINK_WIDTH_SUPPORT_X2 ? 2 : 1;

						dev_info->tcp_cache_size = adev->gfx.config.gc_tcp_l1_size;
						dev_info->num_sqc_per_wgp = adev->gfx.config.gc_num_sqc_per_wgp;
						dev_info->sqc_data_cache_size = adev->gfx.config.gc_l1_data_cache_size_per_sqc;
						dev_info->sqc_inst_cache_size = adev->gfx.config.gc_l1_instruction_cache_size_per_sqc;
						dev_info->gl1c_cache_size = adev->gfx.config.gc_gl1c_size_per_instance *
						adev->gfx.config.gc_gl1c_per_sa;
						dev_info->gl2c_cache_size = adev->gfx.config.gc_gl2c_per_gpu;
						dev_info->mall_size = adev->gmc.mall_size;


						if (adev->gfx.funcs->get_gfx_shadow_info) {
							struct amdgpu_gfx_shadow_info shadow_info;

							ret = amdgpu_gfx_get_gfx_shadow_info(adev, &shadow_info);
							if (!ret) {
								dev_info->shadow_size = shadow_info.shadow_size;
								dev_info->shadow_alignment = shadow_info.shadow_alignment;
								dev_info->csa_size = shadow_info.csa_size;
								dev_info->csa_alignment = shadow_info.csa_alignment;
							}
						}

						ret = copy_to_user(out, dev_info, sizeof(*dev_info)) ? -EFAULT : 0;
						kfree(dev_info);
						return ret;
					}
					case AMDGPU_INFO_VCE_CLOCK_TABLE: {
						struct drm_amdgpu_info_vce_clock_table vce_clk_table = {};
						struct amd_vce_state *vce_state;

						if (user_ret_size < sizeof(vce_clk_table))
							return -EINVAL;

						for (i = 0; i < AMDGPU_VCE_CLOCK_TABLE_ENTRIES; i++) {
							vce_state = amdgpu_dpm_get_vce_clock_state(adev, i);
							if (vce_state) {
								vce_clk_table.entries[i].sclk = vce_state->sclk;
								vce_clk_table.entries[i].mclk = vce_state->mclk;
								vce_clk_table.entries[i].eclk = vce_state->evclk;
								vce_clk_table.num_valid_entries++;
							}
						}

						return copy_to_user(out, &vce_clk_table, sizeof(vce_clk_table)) ? -EFAULT : 0;
					}
					case AMDGPU_INFO_VBIOS: {
						uint32_t bios_size = adev->bios_size;

						switch (info->vbios_info.type) {
							case AMDGPU_INFO_VBIOS_SIZE:
								if (user_ret_size < sizeof(bios_size))
									return -EINVAL;
							return copy_to_user(out, &bios_size, sizeof(bios_size))
							? -EFAULT : 0;
							case AMDGPU_INFO_VBIOS_IMAGE: {
								uint8_t *bios;
								uint32_t bios_offset = info->vbios_info.offset;

								if (bios_offset >= bios_size)
									return -EINVAL;
								if (user_ret_size < (bios_size - bios_offset))
									return -EINVAL;

								bios = adev->bios + bios_offset;
								return copy_to_user(out, bios, (bios_size - bios_offset))
								? -EFAULT : 0;
							}
							case AMDGPU_INFO_VBIOS_INFO: {
								struct drm_amdgpu_info_vbios vbios_info = {};
								struct atom_context *atom_context;

								if (user_ret_size < sizeof(vbios_info))
									return -EINVAL;

								atom_context = adev->mode_info.atom_context;
								if (atom_context) {
									memcpy(vbios_info.name, atom_context->name,
										   sizeof(atom_context->name));
									memcpy(vbios_info.vbios_pn, atom_context->vbios_pn,
										   sizeof(atom_context->vbios_pn));
									vbios_info.version = atom_context->version;
									memcpy(vbios_info.vbios_ver_str, atom_context->vbios_ver_str,
										   sizeof(atom_context->vbios_ver_str));
									memcpy(vbios_info.date, atom_context->date,
										   sizeof(atom_context->date));
								}

								return copy_to_user(out, &vbios_info, sizeof(vbios_info)) ? -EFAULT : 0;
							}
							default:
								DRM_DEBUG_KMS("Invalid request %d\n",
											  info->vbios_info.type);
								return -EINVAL;
						}
					}
							case AMDGPU_INFO_NUM_HANDLES: {
								struct drm_amdgpu_info_num_handles handle;

								if (user_ret_size < sizeof(handle))
									return -EINVAL;

								switch (info->query_hw_ip.type) {
									case AMDGPU_HW_IP_UVD:
										if (adev->asic_type < CHIP_POLARIS10) {
											handle.uvd_max_handles = adev->uvd.max_handles;
											handle.uvd_used_handles = amdgpu_uvd_used_handles(adev);

											return copy_to_user(out, &handle, sizeof(handle)) ? -EFAULT : 0;
										} else {
											return -ENODATA;
										}

										break;
									default:
										return -EINVAL;
								}
							}
									case AMDGPU_INFO_SENSOR: {
										if (user_ret_size < sizeof(ui32))
											return -EINVAL;
										if (!adev->pm.dpm_enabled)
											return -ENOENT;

										switch (info->sensor_info.type) {
											case AMDGPU_INFO_SENSOR_GFX_SCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GFX_SCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											case AMDGPU_INFO_SENSOR_GFX_MCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GFX_MCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											case AMDGPU_INFO_SENSOR_GPU_TEMP:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GPU_TEMP,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   break;
											case AMDGPU_INFO_SENSOR_GPU_LOAD:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GPU_LOAD,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   break;
											case AMDGPU_INFO_SENSOR_GPU_AVG_POWER:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GPU_AVG_POWER,
									   (void *)&ui32, &ui32_size)) {
													if (amdgpu_dpm_read_sensor(adev,
														AMDGPU_PP_SENSOR_GPU_INPUT_POWER,
										(void *)&ui32, &ui32_size)) {
														return -EINVAL;
										}
									   }
									   ui32 >>= 8;
									   break;
											case AMDGPU_INFO_SENSOR_GPU_INPUT_POWER:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_GPU_INPUT_POWER,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 >>= 8;
									   break;
											case AMDGPU_INFO_SENSOR_VDDNB:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_VDDNB,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   break;
											case AMDGPU_INFO_SENSOR_VDDGFX:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_VDDGFX,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   break;
											case AMDGPU_INFO_SENSOR_STABLE_PSTATE_GFX_SCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_STABLE_PSTATE_SCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											case AMDGPU_INFO_SENSOR_STABLE_PSTATE_GFX_MCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_STABLE_PSTATE_MCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											case AMDGPU_INFO_SENSOR_PEAK_PSTATE_GFX_SCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_PEAK_PSTATE_SCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											case AMDGPU_INFO_SENSOR_PEAK_PSTATE_GFX_MCLK:
												if (amdgpu_dpm_read_sensor(adev,
													AMDGPU_PP_SENSOR_PEAK_PSTATE_MCLK,
									   (void *)&ui32, &ui32_size)) {
													return -EINVAL;
									   }
									   ui32 /= 100;
									   break;
											default:
												DRM_DEBUG_KMS("Invalid request %d\n",
															  info->sensor_info.type);
												return -EINVAL;
										}
										return copy_to_user(out, &ui32, sizeof(ui32)) ? -EFAULT : 0;
									}
											case AMDGPU_INFO_VRAM_LOST_COUNTER:
												if (user_ret_size < sizeof(ui32))
													return -EINVAL;
		ui32 = atomic_read(&adev->vram_lost_counter);
		return copy_to_user(out, &ui32, sizeof(ui32)) ? -EFAULT : 0;
											case AMDGPU_INFO_RAS_ENABLED_FEATURES: {
												uint64_t ras_mask;
												struct amdgpu_ras *ras = amdgpu_ras_get_context(adev);

												if (user_ret_size < sizeof(ras_mask))
													return -EINVAL;
												if (!ras)
													return -EINVAL;
												ras_mask = (uint64_t)adev->ras_enabled << 32 | ras->features;

												return copy_to_user(out, &ras_mask, sizeof(ras_mask)) ? -EFAULT : 0;
											}
											case AMDGPU_INFO_VIDEO_CAPS: {
												const struct amdgpu_video_codecs *codecs;
												struct drm_amdgpu_info_video_caps *caps;
												int r;

												if (user_ret_size < sizeof(*caps))
													return -EINVAL;

												if (!adev->asic_funcs->query_video_codecs)
													return -EINVAL;

												switch (info->video_cap.type) {
													case AMDGPU_INFO_VIDEO_CAPS_DECODE:
														r = amdgpu_asic_query_video_codecs(adev, false, &codecs);
														if (r)
															return -EINVAL;
													break;
													case AMDGPU_INFO_VIDEO_CAPS_ENCODE:
														r = amdgpu_asic_query_video_codecs(adev, true, &codecs);
														if (r)
															return -EINVAL;
													break;
													default:
														DRM_DEBUG_KMS("Invalid request %d\n",
																	  info->video_cap.type);
														return -EINVAL;
												}

												caps = kzalloc(sizeof(*caps), GFP_KERNEL);
												if (!caps)
													return -ENOMEM;

												for (i = 0; i < codecs->codec_count; i++) {
													int idx = codecs->codec_array[i].codec_type;

													switch (idx) {
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_MPEG2:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_MPEG4:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_VC1:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_MPEG4_AVC:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_HEVC:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_JPEG:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_VP9:
														case AMDGPU_INFO_VIDEO_CAPS_CODEC_IDX_AV1:
															caps->codec_info[idx].valid = 1;
															caps->codec_info[idx].max_width =
															codecs->codec_array[i].max_width;
															caps->codec_info[idx].max_height =
															codecs->codec_array[i].max_height;
															caps->codec_info[idx].max_pixels_per_frame =
															codecs->codec_array[i].max_pixels_per_frame;
															caps->codec_info[idx].max_level =
															codecs->codec_array[i].max_level;
															break;
														default:
															/* Deliberately do nothing for unknown codecs */
															break;
													}
												}
												r = copy_to_user(out, caps, sizeof(*caps)) ? -EFAULT : 0;
												kfree(caps);
												return r;
											}
														case AMDGPU_INFO_MAX_IBS: {
															uint32_t max_ibs[AMDGPU_HW_IP_NUM];

															if (user_ret_size < sizeof(max_ibs))
																return -EINVAL;

															for (i = 0; i < AMDGPU_HW_IP_NUM; ++i)
																max_ibs[i] = amdgpu_ring_max_ibs(i);

															return copy_to_user(out, max_ibs, sizeof(max_ibs)) ? -EFAULT : 0;
														}
														case AMDGPU_INFO_GPUVM_FAULT: {
															struct amdgpu_fpriv *fpriv_vm = filp->driver_priv;
															struct amdgpu_vm *vm;
															struct drm_amdgpu_info_gpuvm_fault gpuvm_fault;
															unsigned long flags;

															if (user_ret_size < sizeof(gpuvm_fault))
																return -EINVAL;

															if (!fpriv_vm) /* Should not happen if open succeeded */
																return -EINVAL;
															vm = &fpriv_vm->vm;

															memset(&gpuvm_fault, 0, sizeof(gpuvm_fault));

															xa_lock_irqsave(&adev->vm_manager.pasids, flags);
															gpuvm_fault.addr = vm->fault_info.addr;
															gpuvm_fault.status = vm->fault_info.status;
															gpuvm_fault.vmhub = vm->fault_info.vmhub;
															xa_unlock_irqrestore(&adev->vm_manager.pasids, flags);

															return copy_to_user(out, &gpuvm_fault, sizeof(gpuvm_fault)) ? -EFAULT : 0;
														}
														default:
															DRM_DEBUG_KMS("Invalid request %d\n", info->query);
															return -EINVAL;
	}
	return 0; /* Should not be reached due to default case */
}

int amdgpu_driver_open_kms(struct drm_device *dev, struct drm_file *file_priv)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = NULL;
	bool pm_active = false;
	int pasid = 0;
	int r;

	flush_delayed_work(&adev->delayed_init_work);

	if (amdgpu_ras_intr_triggered()) {
		DRM_ERROR("RAS Intr triggered, device disabled!!");
		return -EHWPOISON;
	}

	r = pm_runtime_get_sync(dev->dev);
	if (r < 0)
		goto err_pm;
	pm_active = true;

	fpriv = kzalloc(sizeof(*fpriv), GFP_KERNEL);
	if (!fpriv) {
		r = -ENOMEM;
		goto err_pm;
	}

	pasid = amdgpu_pasid_alloc(16);
	if (pasid < 0) {
		dev_warn(adev->dev, "No more PASIDs available!");
		pasid = 0;
	}

	r = amdgpu_xcp_open_device(adev, fpriv, file_priv);
	if (r)
		goto err_free_fpriv;

	r = amdgpu_vm_init(adev, &fpriv->vm, fpriv->xcp_id);
	if (r)
		goto err_xcp;

	r = amdgpu_vm_set_pasid(adev, &fpriv->vm, pasid);
	if (r)
		goto err_vm;

	fpriv->prt_va = amdgpu_vm_bo_add(adev, &fpriv->vm, NULL);
	if (!fpriv->prt_va) {
		r = -ENOMEM;
		goto err_vm;
	}

	if (adev->gfx.mcbp) {
		uint64_t csa_addr = amdgpu_csa_vaddr(adev) & AMDGPU_GMC_HOLE_MASK;

		r = amdgpu_map_static_csa(adev, &fpriv->vm,
								  adev->virt.csa_obj,
							&fpriv->csa_va,
							csa_addr,
							AMDGPU_CSA_SIZE);
		if (r)
			goto err_vm;
	}

	r = amdgpu_seq64_map(adev, &fpriv->vm, &fpriv->seq64_va);
	if (r)
		goto err_vm;

	mutex_init(&fpriv->bo_list_lock);
	idr_init_base(&fpriv->bo_list_handles, 1);
	amdgpu_ctx_mgr_init(&fpriv->ctx_mgr, adev);

	file_priv->driver_priv = fpriv;

	pm_runtime_mark_last_busy(dev->dev);
	pm_runtime_put_autosuspend(dev->dev);
	return 0;

	err_vm:
	amdgpu_vm_fini(adev, &fpriv->vm);
	err_xcp:
	if (pasid)
		amdgpu_pasid_free(pasid);
	err_free_fpriv:
	kfree(fpriv);
	err_pm:
	if (pm_active)
		pm_runtime_put_autosuspend(dev->dev);
	return r;
}

void amdgpu_driver_postclose_kms(struct drm_device *dev,
								 struct drm_file *file_priv)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = file_priv->driver_priv;
	struct amdgpu_bo_list *list;
	struct amdgpu_bo *pd;
	u32 pasid;
	int handle;

	if (!fpriv)
		return;

	pm_runtime_get_sync(dev->dev);

	if (amdgpu_device_ip_get_ip_block(adev, AMD_IP_BLOCK_TYPE_UVD) != NULL)
		amdgpu_uvd_free_handles(adev, file_priv);
	if (amdgpu_device_ip_get_ip_block(adev, AMD_IP_BLOCK_TYPE_VCE) != NULL)
		amdgpu_vce_free_handles(adev, file_priv);

	if (fpriv->csa_va) {
		uint64_t csa_addr = amdgpu_csa_vaddr(adev) & AMDGPU_GMC_HOLE_MASK;

		WARN_ON(amdgpu_unmap_static_csa(adev, &fpriv->vm, adev->virt.csa_obj,
										fpriv->csa_va, csa_addr));
		fpriv->csa_va = NULL;
	}

	amdgpu_seq64_unmap(adev, &fpriv->vm, fpriv->seq64_va);

	pasid = fpriv->vm.pasid;
	pd = amdgpu_bo_ref(fpriv->vm.root.bo);
	if (!WARN_ON(amdgpu_bo_reserve(pd, true))) {
		amdgpu_vm_bo_del(adev, fpriv->prt_va);
		amdgpu_bo_unreserve(pd);
	}

	amdgpu_ctx_mgr_fini(&fpriv->ctx_mgr);
	amdgpu_vm_fini(adev, &fpriv->vm);

	if (pasid)
		amdgpu_pasid_free_delayed(pd->tbo.base.resv, pasid);
	amdgpu_bo_unref(&pd);

	idr_for_each_entry(&fpriv->bo_list_handles, list, handle)
	amdgpu_bo_list_put(list);

	idr_destroy(&fpriv->bo_list_handles);
	mutex_destroy(&fpriv->bo_list_lock);

	kfree(fpriv);
	file_priv->driver_priv = NULL;

	pm_runtime_mark_last_busy(dev->dev);
	pm_runtime_put_autosuspend(dev->dev);
}


void amdgpu_driver_release_kms(struct drm_device *dev)
{
	struct amdgpu_device *adev = drm_to_adev(dev);

	amdgpu_device_fini_sw(adev);
	pci_set_drvdata(adev->pdev, NULL);
}

u32 amdgpu_get_vblank_counter_kms(struct drm_crtc *crtc)
{
	struct drm_device        *dev  = crtc->dev;
	struct amdgpu_device     *adev = drm_to_adev(dev);
	unsigned int              pipe = crtc->index;
	int                       vpos, hpos, stat;
	u32                       count;

	if (pipe >= adev->mode_info.num_crtc) {
		DRM_ERROR("Invalid crtc %u\n", pipe);
		return 0;
	}

	if (adev->mode_info.crtcs[pipe]) {
		do {
			count = amdgpu_display_vblank_get_counter(adev, pipe);
			stat  = amdgpu_display_get_crtc_scanoutpos(
				dev, pipe, GET_DISTANCE_TO_VBLANKSTART,
				&vpos, &hpos, NULL, NULL,
				&adev->mode_info.crtcs[pipe]->base.hwmode);
		} while (count != amdgpu_display_vblank_get_counter(adev, pipe));

		if ((stat & (DRM_SCANOUTPOS_VALID | DRM_SCANOUTPOS_ACCURATE)) !=
			(DRM_SCANOUTPOS_VALID | DRM_SCANOUTPOS_ACCURATE)) {
			DRM_DEBUG_VBL("Query failed! stat %d\n", stat);
			} else if (vpos >= 0) {
				count++;
			}
	} else {
		count = amdgpu_display_vblank_get_counter(adev, pipe);
		DRM_DEBUG_VBL("NULL mode info! Returned count may be wrong.\n");
	}

	return count;
}

int amdgpu_enable_vblank_kms(struct drm_crtc *crtc)
{
	struct drm_device *dev = crtc->dev;
	unsigned int pipe = crtc->index;
	struct amdgpu_device *adev = drm_to_adev(dev);
	int idx = amdgpu_display_crtc_idx_to_irq_type(adev, pipe);

	return amdgpu_irq_get(adev, &adev->crtc_irq, idx);
}

void amdgpu_disable_vblank_kms(struct drm_crtc *crtc)
{
	struct drm_device *dev = crtc->dev;
	unsigned int pipe = crtc->index;
	struct amdgpu_device *adev = drm_to_adev(dev);
	int idx = amdgpu_display_crtc_idx_to_irq_type(adev, pipe);

	amdgpu_irq_put(adev, &adev->crtc_irq, idx);
}

#if defined(CONFIG_DEBUG_FS)

static int amdgpu_debugfs_firmware_info_show(struct seq_file *m, void *unused)
{
	struct amdgpu_device *adev = m->private;
	struct drm_amdgpu_info_firmware fw_info;
	struct drm_amdgpu_query_fw query_fw;
	struct atom_context *ctx = adev->mode_info.atom_context;
	uint8_t smu_program, smu_major, smu_minor, smu_debug;
	int ret, i;

	static const char *ta_fw_name[TA_FW_TYPE_MAX_INDEX] = {
		#define TA_FW_NAME(type)[TA_FW_TYPE_PSP_##type] = #type
		TA_FW_NAME(XGMI),
		TA_FW_NAME(RAS),
		TA_FW_NAME(HDCP),
		TA_FW_NAME(DTM),
		TA_FW_NAME(RAP),
		TA_FW_NAME(SECUREDISPLAY),
		#undef TA_FW_NAME
	};

	query_fw.fw_type = AMDGPU_INFO_FW_VCE;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "VCE feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_UVD;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "UVD feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GMC;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "MC feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_ME;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "ME feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_PFP;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "PFP feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_CE;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "CE feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLC;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLC feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_CNTL;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLC SRLC feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_GPM_MEM;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLC SRLG feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLC_RESTORE_LIST_SRM_MEM;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLC SRLS feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLCP;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLCP feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_RLCV;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "RLCV feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_GFX_MEC;
	query_fw.index = 0;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "MEC feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	if (adev->gfx.mec2_fw) {
		query_fw.index = 1;
		ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
		if (ret)
			return ret;
		seq_printf(m, "MEC2 feature version: %u, firmware version: 0x%08x\n",
				   fw_info.feature, fw_info.ver);
	}

	query_fw.fw_type = AMDGPU_INFO_FW_IMU;
	query_fw.index = 0;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "IMU feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_SOS;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "SOS feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);


	query_fw.fw_type = AMDGPU_INFO_FW_ASD;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "ASD feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_TA;
	for (i = TA_FW_TYPE_PSP_XGMI; i < TA_FW_TYPE_MAX_INDEX; i++) {
		query_fw.index = i;
		ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
		if (ret)
			continue;

		seq_printf(m, "TA %s feature version: 0x%08x, firmware version: 0x%08x\n",
				   ta_fw_name[i], fw_info.feature, fw_info.ver);
	}

	query_fw.fw_type = AMDGPU_INFO_FW_SMC;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	smu_program = (fw_info.ver >> 24) & 0xff;
	smu_major = (fw_info.ver >> 16) & 0xff;
	smu_minor = (fw_info.ver >> 8) & 0xff;
	smu_debug = (fw_info.ver >> 0) & 0xff;
	seq_printf(m, "SMC feature version: %u, program: %d, firmware version: 0x%08x (%d.%d.%d)\n",
			   fw_info.feature, smu_program, fw_info.ver, smu_major, smu_minor, smu_debug);

	query_fw.fw_type = AMDGPU_INFO_FW_SDMA;
	for (i = 0; i < adev->sdma.num_instances; i++) {
		query_fw.index = i;
		ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
		if (ret)
			return ret;
		seq_printf(m, "SDMA%d feature version: %u, firmware version: 0x%08x\n",
				   i, fw_info.feature, fw_info.ver);
	}

	query_fw.fw_type = AMDGPU_INFO_FW_VCN;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "VCN feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_DMCU;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "DMCU feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_DMCUB;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "DMCUB feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_TOC;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "TOC feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	if (adev->psp.cap_fw) {
		query_fw.fw_type = AMDGPU_INFO_FW_CAP;
		ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
		if (ret)
			return ret;
		seq_printf(m, "CAP feature version: %u, firmware version: 0x%08x\n",
				   fw_info.feature, fw_info.ver);
	}

	query_fw.fw_type = AMDGPU_INFO_FW_MES_KIQ;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "MES_KIQ feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_MES;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "MES feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	query_fw.fw_type = AMDGPU_INFO_FW_VPE;
	ret = amdgpu_firmware_info(&fw_info, &query_fw, adev);
	if (ret)
		return ret;
	seq_printf(m, "VPE feature version: %u, firmware version: 0x%08x\n",
			   fw_info.feature, fw_info.ver);

	seq_printf(m, "VBIOS version: %s\n", ctx->vbios_pn);

	return 0;
}

DEFINE_SHOW_ATTRIBUTE(amdgpu_debugfs_firmware_info);

#endif

void amdgpu_debugfs_firmware_init(struct amdgpu_device *adev)
{
	#if defined(CONFIG_DEBUG_FS)
	struct drm_minor *minor = adev_to_drm(adev)->primary;
	struct dentry *root = minor->debugfs_root;

	debugfs_create_file("amdgpu_firmware_info", 0444, root,
						adev, &amdgpu_debugfs_firmware_info_fops);

	#endif
}
