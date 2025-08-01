// SPDX-License-Identifier: GPL-2.0
/* Copyright(c) 1999 - 2024 Intel Corporation. */

#include <linux/types.h>
#include <linux/module.h>
#include <linux/pci.h>
#include <linux/netdevice.h>
#include <linux/vmalloc.h>
#include <linux/string.h>
#include <linux/in.h>
#include <linux/interrupt.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <linux/sctp.h>
#include <linux/pkt_sched.h>
#include <linux/ipv6.h>
#include <linux/slab.h>
#include <net/checksum.h>
#include <net/ip6_checksum.h>
#include <linux/etherdevice.h>
#include <linux/ethtool.h>
#include <linux/if.h>
#include <linux/if_vlan.h>
#include <linux/if_macvlan.h>
#include <linux/if_bridge.h>
#include <linux/prefetch.h>
#include <linux/reciprocal_div.h>
#include <linux/bpf.h>
#include <linux/bpf_trace.h>
#include <linux/atomic.h>
#include <linux/numa.h>
#include <generated/utsrelease.h>
#include <scsi/fc/fc_fcoe.h>
#include <net/udp_tunnel.h>
#include <net/pkt_cls.h>
#include <net/tc_act/tc_gact.h>
#include <net/tc_act/tc_mirred.h>
#include <net/vxlan.h>
#include <net/mpls.h>
#include <net/netdev_queues.h>
#include <net/xdp_sock_drv.h>
#include <net/xfrm.h>

#include "ixgbe.h"
#include "ixgbe_common.h"
#include "ixgbe_e610.h"
#include "ixgbe_dcb_82599.h"
#include "ixgbe_mbx.h"
#include "ixgbe_phy.h"
#include "ixgbe_sriov.h"
#include "ixgbe_model.h"
#include "ixgbe_txrx_common.h"

static char *gaming_atr_ranges = "27000-27050,7777-7999,6112-6119,9960-9969,3074-3075,5000-5500,8393-8400";
module_param(gaming_atr_ranges, charp, 0644);
MODULE_PARM_DESC(gaming_atr_ranges, "Comma-separated UDP port ranges for gaming ATR (e.g., 27000-27050,7777-7999)");

#define IXGBE_RX_PREFETCH_OFFSET 8
#define IXGBE_TX_PREFETCH_OFFSET 8
#ifndef IXGBE_SET_FLAG_FIXED
#undef IXGBE_SET_FLAG

#define IXGBE_SET_FLAG(_input, _flag, _result) \
(((_input) & (_flag)) ? (_result) : 0)

#define IXGBE_SET_FLAG_FIXED
#endif

static inline dma_addr_t ixgbe_dma_map_single(struct device *dev, void *ptr,
                                              size_t len,
                                              enum dma_data_direction dir)
{
    #ifdef DMA_ATTR_RELAXED_ORDERING
    return dma_map_single_attrs(dev, ptr, len, dir,
                                DMA_ATTR_RELAXED_ORDERING);
    #else
    return dma_map_single(dev, ptr, len, dir);
    #endif
}

static inline dma_addr_t ixgbe_dma_map_page(struct device *dev, struct page *pg,
                                            unsigned long off, size_t len,
                                            enum dma_data_direction dir)
{
    #ifdef DMA_ATTR_RELAXED_ORDERING
    return dma_map_page_attrs(dev, pg, off, len, dir,
                              DMA_ATTR_RELAXED_ORDERING);
    #else
    return dma_map_page(dev, pg, off, len, dir);
    #endif
}

static inline void ixgbe_dma_unmap_single(struct device *dev, dma_addr_t addr,
                                          size_t len,
                                          enum dma_data_direction dir)
{
    #ifdef DMA_ATTR_RELAXED_ORDERING
    dma_unmap_single_attrs(dev, addr, len, dir,
                           DMA_ATTR_RELAXED_ORDERING);
    #else
    dma_unmap_single(dev, addr, len, dir);
    #endif
}

static inline void ixgbe_dma_unmap_page(struct device *dev, dma_addr_t addr,
                                        size_t len,
                                        enum dma_data_direction dir)
{
    #ifdef DMA_ATTR_RELAXED_ORDERING
    dma_unmap_page_attrs(dev, addr, len, dir,
                         DMA_ATTR_RELAXED_ORDERING);
    #else
    dma_unmap_page(dev, addr, len, dir);
    #endif
}

/* EITR bits[15:2] — one count = 2 µs when ITRGRAN = 0             */
#define IXGBE_ITR_TO_REG(u2us)  ((u2us) << 2)   /* abstract -> hw  */
#define IXGBE_REG_TO_ITR(reg)   ((reg)  >> 2)   /* hw -> abstract  */

/* 4 µs in abstract 2-µs units.  4 << 2 => 16 written to register  */
#define IXGBE_ITR_4US           4u

char ixgbe_driver_name[] = "ixgbe";
static const char ixgbe_driver_string[] =
			      "Intel(R) 10 Gigabit PCI Express Network Driver";
#ifdef IXGBE_FCOE
char ixgbe_default_device_descr[] =
			      "Intel(R) 10 Gigabit Network Connection";
#else
static char ixgbe_default_device_descr[] =
			      "Intel(R) 10 Gigabit Network Connection";
#endif
static const char ixgbe_copyright[] =
				"Copyright (c) 1999-2016 Intel Corporation.";

static const char ixgbe_overheat_msg[] = "Network adapter has been stopped because it has over heated. Restart the computer. If the problem persists, power off the system and replace the adapter";

static const struct ixgbe_info *ixgbe_info_tbl[] = {
	[board_82598]		= &ixgbe_82598_info,
	[board_82599]		= &ixgbe_82599_info,
	[board_X540]		= &ixgbe_X540_info,
	[board_X550]		= &ixgbe_X550_info,
	[board_X550EM_x]	= &ixgbe_X550EM_x_info,
	[board_x550em_x_fw]	= &ixgbe_x550em_x_fw_info,
	[board_x550em_a]	= &ixgbe_x550em_a_info,
	[board_x550em_a_fw]	= &ixgbe_x550em_a_fw_info,
	[board_e610]		= &ixgbe_e610_info,
};

/* ixgbe_pci_tbl - PCI Device ID Table
 *
 * Wildcard entries (PCI_ANY_ID) should come last
 * Last entry must be all 0s
 *
 * { Vendor ID, Device ID, SubVendor ID, SubDevice ID,
 *   Class, Class Mask, private data (not used) }
 */
static const struct pci_device_id ixgbe_pci_tbl[] = {
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598AF_DUAL_PORT), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598AF_SINGLE_PORT), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598AT), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598AT2), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598EB_CX4), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598_CX4_DUAL_PORT), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598_DA_DUAL_PORT), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598_SR_DUAL_PORT_EM), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598EB_XF_LR), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598EB_SFP_LOM), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82598_BX), board_82598 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_KX4), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_XAUI_LOM), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_KR), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_SFP), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_SFP_EM), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_KX4_MEZZ), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_CX4), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_BACKPLANE_FCOE), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_SFP_FCOE), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_T3_LOM), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_COMBO_BACKPLANE), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X540T), board_X540 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_SFP_SF2), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_LS), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_QSFP_SF_QP), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599EN_SFP), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_82599_SFP_SF_QP), board_82599 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X540T1), board_X540 },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550T), board_X550},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550T1), board_X550},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_KX4), board_X550EM_x},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_XFI), board_X550EM_x},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_KR), board_X550EM_x},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_10G_T), board_X550EM_x},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_SFP), board_X550EM_x},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_X_1G_T), board_x550em_x_fw},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_KR), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_KR_L), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_SFP_N), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_SGMII), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_SGMII_L), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_10G_T), board_x550em_a},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_SFP), board_x550em_a },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_1G_T), board_x550em_a_fw },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_X550EM_A_1G_T_L), board_x550em_a_fw },
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_E610_BACKPLANE), board_e610},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_E610_SFP), board_e610},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_E610_10G_T), board_e610},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_E610_2_5G_T), board_e610},
	{PCI_VDEVICE(INTEL, IXGBE_DEV_ID_E610_SGMII), board_e610},
	/* required last entry */
	{0, }
};
MODULE_DEVICE_TABLE(pci, ixgbe_pci_tbl);

#ifdef CONFIG_IXGBE_DCA
static int ixgbe_notify_dca(struct notifier_block *, unsigned long event,
			    void *p);
static struct notifier_block dca_notifier = {
	.notifier_call = ixgbe_notify_dca,
	.next          = NULL,
	.priority      = 0
};
#endif

#ifdef CONFIG_PCI_IOV
static unsigned int max_vfs;
module_param(max_vfs, uint, 0);
MODULE_PARM_DESC(max_vfs,
		 "Maximum number of virtual functions to allocate per physical function - default is zero and maximum value is 63. (Deprecated)");
#endif /* CONFIG_PCI_IOV */

static bool allow_unsupported_sfp;
module_param(allow_unsupported_sfp, bool, 0444);
MODULE_PARM_DESC(allow_unsupported_sfp,
		 "Allow unsupported and untested SFP+ modules on 82599-based adapters");

#define DEFAULT_MSG_ENABLE (NETIF_MSG_DRV|NETIF_MSG_PROBE|NETIF_MSG_LINK)
static int debug = -1;
module_param(debug, int, 0);
MODULE_PARM_DESC(debug, "Debug level (0=none,...,16=all)");

MODULE_DESCRIPTION("Intel(R) 10 Gigabit PCI Express Network Driver");
MODULE_LICENSE("GPL v2");

DEFINE_STATIC_KEY_FALSE(ixgbe_xdp_locking_key);
EXPORT_SYMBOL(ixgbe_xdp_locking_key);

static struct workqueue_struct *ixgbe_wq;

static bool ixgbe_check_cfg_remove(struct ixgbe_hw *hw, struct pci_dev *pdev);
static void ixgbe_watchdog_link_is_down(struct ixgbe_adapter *);
static void ixgbe_watchdog_link_is_up(struct ixgbe_adapter *);
static void ixgbe_watchdog_update_link(struct ixgbe_adapter *);

static const struct net_device_ops ixgbe_netdev_ops;

static bool netif_is_ixgbe(struct net_device *dev)
{
	return dev && (dev->netdev_ops == &ixgbe_netdev_ops);
}

static int ixgbe_read_pci_cfg_word_parent(struct ixgbe_adapter *adapter,
					  u32 reg, u16 *value)
{
	struct pci_dev *parent_dev;
	struct pci_bus *parent_bus;

	parent_bus = adapter->pdev->bus->parent;
	if (!parent_bus)
		return -1;

	parent_dev = parent_bus->self;
	if (!parent_dev)
		return -1;

	if (!pci_is_pcie(parent_dev))
		return -1;

	pcie_capability_read_word(parent_dev, reg, value);
	if (*value == IXGBE_FAILED_READ_CFG_WORD &&
	    ixgbe_check_cfg_remove(&adapter->hw, parent_dev))
		return -1;
	return 0;
}

static int ixgbe_get_parent_bus_info(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u16 link_status = 0;
	int err;

	hw->bus.type = ixgbe_bus_type_pci_express;

	/* Get the negotiated link width and speed from PCI config space of the
	 * parent, as this device is behind a switch
	 */
	err = ixgbe_read_pci_cfg_word_parent(adapter, 18, &link_status);

	/* assume caller will handle error case */
	if (err)
		return err;

	hw->bus.width = ixgbe_convert_bus_width(link_status);
	hw->bus.speed = ixgbe_convert_bus_speed(link_status);

	return 0;
}

/**
 * ixgbe_pcie_from_parent - Determine whether PCIe info should come from parent
 * @hw: hw specific details
 *
 * This function is used by probe to determine whether a device's PCI-Express
 * bandwidth details should be gathered from the parent bus instead of from the
 * device. Used to ensure that various locations all have the correct device ID
 * checks.
 *
 * Return: true if information should be collected from the parent bus, false
 *         otherwise
 */
static bool ixgbe_pcie_from_parent(struct ixgbe_hw *hw)
{
	switch (hw->device_id) {
	case IXGBE_DEV_ID_82599_SFP_SF_QP:
	case IXGBE_DEV_ID_82599_QSFP_SF_QP:
		return true;
	default:
		return false;
	}
}

static void ixgbe_check_minimum_link(struct ixgbe_adapter *adapter,
				     int expected_gts)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct pci_dev *pdev;

	/* Some devices are not connected over PCIe and thus do not negotiate
	 * speed. These devices do not have valid bus info, and thus any report
	 * we generate may not be correct.
	 */
	if (hw->bus.type == ixgbe_bus_type_internal)
		return;

	/* determine whether to use the parent device */
	if (ixgbe_pcie_from_parent(&adapter->hw))
		pdev = adapter->pdev->bus->parent->self;
	else
		pdev = adapter->pdev;

	pcie_print_link_status(pdev);
}

static void ixgbe_service_event_schedule(struct ixgbe_adapter *adapter)
{
	if (!test_bit(__IXGBE_DOWN, &adapter->state) &&
	    !test_bit(__IXGBE_REMOVING, &adapter->state) &&
	    !test_and_set_bit(__IXGBE_SERVICE_SCHED, &adapter->state))
		queue_work(ixgbe_wq, &adapter->service_task);
}

static void ixgbe_remove_adapter(struct ixgbe_hw *hw)
{
	struct ixgbe_adapter *adapter = hw->back;

	if (!hw->hw_addr)
		return;
	hw->hw_addr = NULL;
	e_dev_err("Adapter removed\n");
	if (test_bit(__IXGBE_SERVICE_INITED, &adapter->state))
		ixgbe_service_event_schedule(adapter);
}

static u32 ixgbe_check_remove(struct ixgbe_hw *hw, u32 reg)
{
	u8 __iomem *reg_addr;
	u32 value;
	int i;

	reg_addr = READ_ONCE(hw->hw_addr);
	if (ixgbe_removed(reg_addr))
		return IXGBE_FAILED_READ_REG;

	/* Register read of 0xFFFFFFF can indicate the adapter has been removed,
	 * so perform several status register reads to determine if the adapter
	 * has been removed.
	 */
	for (i = 0; i < IXGBE_FAILED_READ_RETRIES; i++) {
		value = readl(reg_addr + IXGBE_STATUS);
		if (value != IXGBE_FAILED_READ_REG)
			break;
		mdelay(3);
	}

	if (value == IXGBE_FAILED_READ_REG)
		ixgbe_remove_adapter(hw);
	else
		value = readl(reg_addr + reg);
	return value;
}

/**
 * ixgbe_read_reg - Read from device register
 * @hw: hw specific details
 * @reg: offset of register to read
 *
 * Returns : value read or IXGBE_FAILED_READ_REG if removed
 *
 * This function is used to read device registers. It checks for device
 * removal by confirming any read that returns all ones by checking the
 * status register value for all ones. This function avoids reading from
 * the hardware if a removal was previously detected in which case it
 * returns IXGBE_FAILED_READ_REG (all ones).
 */
u32 ixgbe_read_reg(struct ixgbe_hw *hw, u32 reg)
{
	u8 __iomem *reg_addr = READ_ONCE(hw->hw_addr);
	u32 value;

	if (ixgbe_removed(reg_addr))
		return IXGBE_FAILED_READ_REG;
	if (unlikely(hw->phy.nw_mng_if_sel &
		     IXGBE_NW_MNG_IF_SEL_SGMII_ENABLE)) {
		struct ixgbe_adapter *adapter;
		int i;

		for (i = 0; i < 200; ++i) {
			value = readl(reg_addr + IXGBE_MAC_SGMII_BUSY);
			if (likely(!value))
				goto writes_completed;
			if (value == IXGBE_FAILED_READ_REG) {
				ixgbe_remove_adapter(hw);
				return IXGBE_FAILED_READ_REG;
			}
			udelay(5);
		}

		adapter = hw->back;
		e_warn(hw, "register writes incomplete %08x\n", value);
	}

writes_completed:
	value = readl(reg_addr + reg);
	if (unlikely(value == IXGBE_FAILED_READ_REG))
		value = ixgbe_check_remove(hw, reg);
	return value;
}

static bool ixgbe_check_cfg_remove(struct ixgbe_hw *hw, struct pci_dev *pdev)
{
	u16 value;

	pci_read_config_word(pdev, PCI_VENDOR_ID, &value);
	if (value == IXGBE_FAILED_READ_CFG_WORD) {
		ixgbe_remove_adapter(hw);
		return true;
	}
	return false;
}

u16 ixgbe_read_pci_cfg_word(struct ixgbe_hw *hw, u32 reg)
{
	struct ixgbe_adapter *adapter = hw->back;
	u16 value;

	if (ixgbe_removed(hw->hw_addr))
		return IXGBE_FAILED_READ_CFG_WORD;
	pci_read_config_word(adapter->pdev, reg, &value);
	if (value == IXGBE_FAILED_READ_CFG_WORD &&
	    ixgbe_check_cfg_remove(hw, adapter->pdev))
		return IXGBE_FAILED_READ_CFG_WORD;
	return value;
}

#ifdef CONFIG_PCI_IOV
static u32 ixgbe_read_pci_cfg_dword(struct ixgbe_hw *hw, u32 reg)
{
	struct ixgbe_adapter *adapter = hw->back;
	u32 value;

	if (ixgbe_removed(hw->hw_addr))
		return IXGBE_FAILED_READ_CFG_DWORD;
	pci_read_config_dword(adapter->pdev, reg, &value);
	if (value == IXGBE_FAILED_READ_CFG_DWORD &&
	    ixgbe_check_cfg_remove(hw, adapter->pdev))
		return IXGBE_FAILED_READ_CFG_DWORD;
	return value;
}
#endif /* CONFIG_PCI_IOV */

void ixgbe_write_pci_cfg_word(struct ixgbe_hw *hw, u32 reg, u16 value)
{
	struct ixgbe_adapter *adapter = hw->back;

	if (ixgbe_removed(hw->hw_addr))
		return;
	pci_write_config_word(adapter->pdev, reg, value);
}

static void ixgbe_service_event_complete(struct ixgbe_adapter *adapter)
{
	BUG_ON(!test_bit(__IXGBE_SERVICE_SCHED, &adapter->state));

	/* flush memory to make sure state is correct before next watchdog */
	smp_mb__before_atomic();
	clear_bit(__IXGBE_SERVICE_SCHED, &adapter->state);
}

struct ixgbe_reg_info {
	u32 ofs;
	char *name;
};

static const struct ixgbe_reg_info ixgbe_reg_info_tbl[] = {

	/* General Registers */
	{IXGBE_CTRL, "CTRL"},
	{IXGBE_STATUS, "STATUS"},
	{IXGBE_CTRL_EXT, "CTRL_EXT"},

	/* Interrupt Registers */
	{IXGBE_EICR, "EICR"},

	/* RX Registers */
	{IXGBE_SRRCTL(0), "SRRCTL"},
	{IXGBE_DCA_RXCTRL(0), "DRXCTL"},
	{IXGBE_RDLEN(0), "RDLEN"},
	{IXGBE_RDH(0), "RDH"},
	{IXGBE_RDT(0), "RDT"},
	{IXGBE_RXDCTL(0), "RXDCTL"},
	{IXGBE_RDBAL(0), "RDBAL"},
	{IXGBE_RDBAH(0), "RDBAH"},

	/* TX Registers */
	{IXGBE_TDBAL(0), "TDBAL"},
	{IXGBE_TDBAH(0), "TDBAH"},
	{IXGBE_TDLEN(0), "TDLEN"},
	{IXGBE_TDH(0), "TDH"},
	{IXGBE_TDT(0), "TDT"},
	{IXGBE_TXDCTL(0), "TXDCTL"},

	/* List Terminator */
	{ .name = NULL }
};


/*
 * ixgbe_regdump - register printout routine
 */
static void ixgbe_regdump(struct ixgbe_hw *hw, struct ixgbe_reg_info *reginfo)
{
	int i;
	char rname[16];
	u32 regs[64];

	switch (reginfo->ofs) {
	case IXGBE_SRRCTL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_SRRCTL(i));
		break;
	case IXGBE_DCA_RXCTRL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_DCA_RXCTRL(i));
		break;
	case IXGBE_RDLEN(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RDLEN(i));
		break;
	case IXGBE_RDH(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RDH(i));
		break;
	case IXGBE_RDT(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RDT(i));
		break;
	case IXGBE_RXDCTL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RXDCTL(i));
		break;
	case IXGBE_RDBAL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RDBAL(i));
		break;
	case IXGBE_RDBAH(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_RDBAH(i));
		break;
	case IXGBE_TDBAL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TDBAL(i));
		break;
	case IXGBE_TDBAH(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TDBAH(i));
		break;
	case IXGBE_TDLEN(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TDLEN(i));
		break;
	case IXGBE_TDH(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TDH(i));
		break;
	case IXGBE_TDT(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TDT(i));
		break;
	case IXGBE_TXDCTL(0):
		for (i = 0; i < 64; i++)
			regs[i] = IXGBE_READ_REG(hw, IXGBE_TXDCTL(i));
		break;
	default:
		pr_info("%-15s %08x\n",
			reginfo->name, IXGBE_READ_REG(hw, reginfo->ofs));
		return;
	}

	i = 0;
	while (i < 64) {
		int j;
		char buf[9 * 8 + 1];
		char *p = buf;

		snprintf(rname, 16, "%s[%d-%d]", reginfo->name, i, i + 7);
		for (j = 0; j < 8; j++)
			p += sprintf(p, " %08x", regs[i++]);
		pr_err("%-15s%s\n", rname, buf);
	}

}

static void ixgbe_print_buffer(struct ixgbe_ring *ring, int n)
{
	struct ixgbe_tx_buffer *tx_buffer;

	tx_buffer = &ring->tx_buffer_info[ring->next_to_clean];
	pr_info(" %5d %5X %5X %016llX %08X %p %016llX\n",
		n, ring->next_to_use, ring->next_to_clean,
		(u64)dma_unmap_addr(tx_buffer, dma),
		dma_unmap_len(tx_buffer, len),
		tx_buffer->next_to_watch,
		(u64)tx_buffer->time_stamp);
}

/*
 * ixgbe_dump - Print registers, tx-rings and rx-rings
 */
static void ixgbe_dump(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_reg_info *reginfo;
	int n = 0;
	struct ixgbe_ring *ring;
	struct ixgbe_tx_buffer *tx_buffer;
	union ixgbe_adv_tx_desc *tx_desc;
	struct my_u0 { u64 a; u64 b; } *u0;
	struct ixgbe_ring *rx_ring;
	union ixgbe_adv_rx_desc *rx_desc;
	struct ixgbe_rx_buffer *rx_buffer_info;
	int i = 0;

	if (!netif_msg_hw(adapter))
		return;

	/* Print netdevice Info */
	if (netdev) {
		dev_info(&adapter->pdev->dev, "Net device Info\n");
		pr_info("Device Name     state            "
			"trans_start\n");
		pr_info("%-15s %016lX %016lX\n",
			netdev->name,
			netdev->state,
			dev_trans_start(netdev));
	}

	/* Print Registers */
	dev_info(&adapter->pdev->dev, "Register Dump\n");
	pr_info(" Register Name   Value\n");
	for (reginfo = (struct ixgbe_reg_info *)ixgbe_reg_info_tbl;
	     reginfo->name; reginfo++) {
		ixgbe_regdump(hw, reginfo);
	}

	/* Print TX Ring Summary */
	if (!netdev || !netif_running(netdev))
		return;

	dev_info(&adapter->pdev->dev, "TX Rings Summary\n");
	pr_info(" %s     %s              %s        %s\n",
		"Queue [NTU] [NTC] [bi(ntc)->dma  ]",
		"leng", "ntw", "timestamp");
	for (n = 0; n < adapter->num_tx_queues; n++) {
		ring = adapter->tx_ring[n];
		ixgbe_print_buffer(ring, n);
	}

	for (n = 0; n < adapter->num_xdp_queues; n++) {
		ring = adapter->xdp_ring[n];
		ixgbe_print_buffer(ring, n);
	}

	/* Print TX Rings */
	if (!netif_msg_tx_done(adapter))
		goto rx_ring_summary;

	dev_info(&adapter->pdev->dev, "TX Rings Dump\n");

	/* Transmit Descriptor Formats
	 *
	 * 82598 Advanced Transmit Descriptor
	 *   +--------------------------------------------------------------+
	 * 0 |         Buffer Address [63:0]                                |
	 *   +--------------------------------------------------------------+
	 * 8 |  PAYLEN  | POPTS  | IDX | STA | DCMD  |DTYP |  RSV |  DTALEN |
	 *   +--------------------------------------------------------------+
	 *   63       46 45    40 39 36 35 32 31   24 23 20 19              0
	 *
	 * 82598 Advanced Transmit Descriptor (Write-Back Format)
	 *   +--------------------------------------------------------------+
	 * 0 |                          RSV [63:0]                          |
	 *   +--------------------------------------------------------------+
	 * 8 |            RSV           |  STA  |          NXTSEQ           |
	 *   +--------------------------------------------------------------+
	 *   63                       36 35   32 31                         0
	 *
	 * 82599+ Advanced Transmit Descriptor
	 *   +--------------------------------------------------------------+
	 * 0 |         Buffer Address [63:0]                                |
	 *   +--------------------------------------------------------------+
	 * 8 |PAYLEN  |POPTS|CC|IDX  |STA  |DCMD  |DTYP |MAC  |RSV  |DTALEN |
	 *   +--------------------------------------------------------------+
	 *   63     46 45 40 39 38 36 35 32 31  24 23 20 19 18 17 16 15     0
	 *
	 * 82599+ Advanced Transmit Descriptor (Write-Back Format)
	 *   +--------------------------------------------------------------+
	 * 0 |                          RSV [63:0]                          |
	 *   +--------------------------------------------------------------+
	 * 8 |            RSV           |  STA  |           RSV             |
	 *   +--------------------------------------------------------------+
	 *   63                       36 35   32 31                         0
	 */

	for (n = 0; n < adapter->num_tx_queues; n++) {
		ring = adapter->tx_ring[n];
		pr_info("------------------------------------\n");
		pr_info("TX QUEUE INDEX = %d\n", ring->queue_index);
		pr_info("------------------------------------\n");
		pr_info("%s%s    %s              %s        %s          %s\n",
			"T [desc]     [address 63:0  ] ",
			"[PlPOIdStDDt Ln] [bi->dma       ] ",
			"leng", "ntw", "timestamp", "bi->skb");

		for (i = 0; ring->desc && (i < ring->count); i++) {
			tx_desc = IXGBE_TX_DESC(ring, i);
			tx_buffer = &ring->tx_buffer_info[i];
			u0 = (struct my_u0 *)tx_desc;
			if (dma_unmap_len(tx_buffer, len) > 0) {
				const char *ring_desc;

				if (i == ring->next_to_use &&
				    i == ring->next_to_clean)
					ring_desc = " NTC/U";
				else if (i == ring->next_to_use)
					ring_desc = " NTU";
				else if (i == ring->next_to_clean)
					ring_desc = " NTC";
				else
					ring_desc = "";
				pr_info("T [0x%03X]    %016llX %016llX %016llX %08X %p %016llX %p%s",
					i,
					le64_to_cpu((__force __le64)u0->a),
					le64_to_cpu((__force __le64)u0->b),
					(u64)dma_unmap_addr(tx_buffer, dma),
					dma_unmap_len(tx_buffer, len),
					tx_buffer->next_to_watch,
					(u64)tx_buffer->time_stamp,
					tx_buffer->skb,
					ring_desc);

				if (netif_msg_pktdata(adapter) &&
				    tx_buffer->skb)
					print_hex_dump(KERN_INFO, "",
						DUMP_PREFIX_ADDRESS, 16, 1,
						tx_buffer->skb->data,
						dma_unmap_len(tx_buffer, len),
						true);
			}
		}
	}

	/* Print RX Rings Summary */
rx_ring_summary:
	dev_info(&adapter->pdev->dev, "RX Rings Summary\n");
	pr_info("Queue [NTU] [NTC]\n");
	for (n = 0; n < adapter->num_rx_queues; n++) {
		rx_ring = adapter->rx_ring[n];
		pr_info("%5d %5X %5X\n",
			n, rx_ring->next_to_use, rx_ring->next_to_clean);
	}

	/* Print RX Rings */
	if (!netif_msg_rx_status(adapter))
		return;

	dev_info(&adapter->pdev->dev, "RX Rings Dump\n");

	/* Receive Descriptor Formats
	 *
	 * 82598 Advanced Receive Descriptor (Read) Format
	 *    63                                           1        0
	 *    +-----------------------------------------------------+
	 *  0 |       Packet Buffer Address [63:1]           |A0/NSE|
	 *    +----------------------------------------------+------+
	 *  8 |       Header Buffer Address [63:1]           |  DD  |
	 *    +-----------------------------------------------------+
	 *
	 *
	 * 82598 Advanced Receive Descriptor (Write-Back) Format
	 *
	 *   63       48 47    32 31  30      21 20 16 15   4 3     0
	 *   +------------------------------------------------------+
	 * 0 |       RSS Hash /  |SPH| HDR_LEN  | RSV |Packet|  RSS |
	 *   | Packet   | IP     |   |          |     | Type | Type |
	 *   | Checksum | Ident  |   |          |     |      |      |
	 *   +------------------------------------------------------+
	 * 8 | VLAN Tag | Length | Extended Error | Extended Status |
	 *   +------------------------------------------------------+
	 *   63       48 47    32 31            20 19               0
	 *
	 * 82599+ Advanced Receive Descriptor (Read) Format
	 *    63                                           1        0
	 *    +-----------------------------------------------------+
	 *  0 |       Packet Buffer Address [63:1]           |A0/NSE|
	 *    +----------------------------------------------+------+
	 *  8 |       Header Buffer Address [63:1]           |  DD  |
	 *    +-----------------------------------------------------+
	 *
	 *
	 * 82599+ Advanced Receive Descriptor (Write-Back) Format
	 *
	 *   63       48 47    32 31  30      21 20 17 16   4 3     0
	 *   +------------------------------------------------------+
	 * 0 |RSS / Frag Checksum|SPH| HDR_LEN  |RSC- |Packet|  RSS |
	 *   |/ RTT / PCoE_PARAM |   |          | CNT | Type | Type |
	 *   |/ Flow Dir Flt ID  |   |          |     |      |      |
	 *   +------------------------------------------------------+
	 * 8 | VLAN Tag | Length |Extended Error| Xtnd Status/NEXTP |
	 *   +------------------------------------------------------+
	 *   63       48 47    32 31          20 19                 0
	 */

	for (n = 0; n < adapter->num_rx_queues; n++) {
		rx_ring = adapter->rx_ring[n];
		pr_info("------------------------------------\n");
		pr_info("RX QUEUE INDEX = %d\n", rx_ring->queue_index);
		pr_info("------------------------------------\n");
		pr_info("%s%s%s\n",
			"R  [desc]      [ PktBuf     A0] ",
			"[  HeadBuf   DD] [bi->dma       ] [bi->skb       ] ",
			"<-- Adv Rx Read format");
		pr_info("%s%s%s\n",
			"RWB[desc]      [PcsmIpSHl PtRs] ",
			"[vl er S cks ln] ---------------- [bi->skb       ] ",
			"<-- Adv Rx Write-Back format");

		for (i = 0; i < rx_ring->count; i++) {
			const char *ring_desc;

			if (i == rx_ring->next_to_use)
				ring_desc = " NTU";
			else if (i == rx_ring->next_to_clean)
				ring_desc = " NTC";
			else
				ring_desc = "";

			rx_buffer_info = &rx_ring->rx_buffer_info[i];
			rx_desc = IXGBE_RX_DESC(rx_ring, i);
			u0 = (struct my_u0 *)rx_desc;
			if (rx_desc->wb.upper.length) {
				/* Descriptor Done */
				pr_info("RWB[0x%03X]     %016llX %016llX ---------------- %p%s\n",
					i,
					le64_to_cpu((__force __le64)u0->a),
					le64_to_cpu((__force __le64)u0->b),
					rx_buffer_info->skb,
					ring_desc);
			} else {
				pr_info("R  [0x%03X]     %016llX %016llX %016llX %p%s\n",
					i,
					le64_to_cpu((__force __le64)u0->a),
					le64_to_cpu((__force __le64)u0->b),
					(u64)rx_buffer_info->dma,
					rx_buffer_info->skb,
					ring_desc);

				if (netif_msg_pktdata(adapter) &&
				    rx_buffer_info->dma) {
					print_hex_dump(KERN_INFO, "",
					   DUMP_PREFIX_ADDRESS, 16, 1,
					   page_address(rx_buffer_info->page) +
						    rx_buffer_info->page_offset,
					   ixgbe_rx_bufsz(rx_ring), true);
				}
			}
		}
	}
}

static void ixgbe_release_hw_control(struct ixgbe_adapter *adapter)
{
	u32 ctrl_ext;

	/* Let firmware take over control of h/w */
	ctrl_ext = IXGBE_READ_REG(&adapter->hw, IXGBE_CTRL_EXT);
	IXGBE_WRITE_REG(&adapter->hw, IXGBE_CTRL_EXT,
			ctrl_ext & ~IXGBE_CTRL_EXT_DRV_LOAD);
}

static void ixgbe_get_hw_control(struct ixgbe_adapter *adapter)
{
	u32 ctrl_ext;

	/* Let firmware know the driver has taken over */
	ctrl_ext = IXGBE_READ_REG(&adapter->hw, IXGBE_CTRL_EXT);
	IXGBE_WRITE_REG(&adapter->hw, IXGBE_CTRL_EXT,
			ctrl_ext | IXGBE_CTRL_EXT_DRV_LOAD);
}

/**
 * ixgbe_set_ivar - set the IVAR registers, mapping interrupt causes to vectors
 * @adapter: pointer to adapter struct
 * @direction: 0 for Rx, 1 for Tx, -1 for other causes
 * @queue: queue to map the corresponding interrupt to
 * @msix_vector: the vector to map to the corresponding queue
 *
 */
static void ixgbe_set_ivar(struct ixgbe_adapter *adapter, s8 direction,
			   u8 queue, u8 msix_vector)
{
	u32 ivar, index;
	struct ixgbe_hw *hw = &adapter->hw;
	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		msix_vector |= IXGBE_IVAR_ALLOC_VAL;
		if (direction == -1)
			direction = 0;
		index = (((direction * 64) + queue) >> 2) & 0x1F;
		ivar = IXGBE_READ_REG(hw, IXGBE_IVAR(index));
		ivar &= ~(0xFF << (8 * (queue & 0x3)));
		ivar |= (msix_vector << (8 * (queue & 0x3)));
		IXGBE_WRITE_REG(hw, IXGBE_IVAR(index), ivar);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		if (direction == -1) {
			/* other causes */
			msix_vector |= IXGBE_IVAR_ALLOC_VAL;
			index = ((queue & 1) * 8);
			ivar = IXGBE_READ_REG(&adapter->hw, IXGBE_IVAR_MISC);
			ivar &= ~(0xFF << index);
			ivar |= (msix_vector << index);
			IXGBE_WRITE_REG(&adapter->hw, IXGBE_IVAR_MISC, ivar);
			break;
		} else {
			/* tx or rx causes */
			msix_vector |= IXGBE_IVAR_ALLOC_VAL;
			index = ((16 * (queue & 1)) + (8 * direction));
			ivar = IXGBE_READ_REG(hw, IXGBE_IVAR(queue >> 1));
			ivar &= ~(0xFF << index);
			ivar |= (msix_vector << index);
			IXGBE_WRITE_REG(hw, IXGBE_IVAR(queue >> 1), ivar);
			break;
		}
	default:
		break;
	}
}

void ixgbe_irq_rearm_queues(struct ixgbe_adapter *adapter,
			    u64 qmask)
{
	u32 mask;

	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82598EB:
		mask = (IXGBE_EIMS_RTX_QUEUE & qmask);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EICS, mask);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		mask = (qmask & 0xFFFFFFFF);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EICS_EX(0), mask);
		mask = (qmask >> 32);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EICS_EX(1), mask);
		break;
	default:
		break;
	}
}

static void ixgbe_update_xoff_rx_lfc(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_hw_stats *hwstats = &adapter->stats;
	int i;
	u32 data;

	if ((hw->fc.current_mode != ixgbe_fc_full) &&
	    (hw->fc.current_mode != ixgbe_fc_rx_pause))
		return;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		data = IXGBE_READ_REG(hw, IXGBE_LXOFFRXC);
		break;
	default:
		data = IXGBE_READ_REG(hw, IXGBE_LXOFFRXCNT);
	}
	hwstats->lxoffrxc += data;

	/* refill credits (no tx hang) if we received xoff */
	if (!data)
		return;

	for (i = 0; i < adapter->num_tx_queues; i++)
		clear_bit(__IXGBE_HANG_CHECK_ARMED,
			  &adapter->tx_ring[i]->state);

	for (i = 0; i < adapter->num_xdp_queues; i++)
		clear_bit(__IXGBE_HANG_CHECK_ARMED,
			  &adapter->xdp_ring[i]->state);
}

static void ixgbe_update_xoff_received(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_hw_stats *hwstats = &adapter->stats;
	u32 xoff[8] = {0};
	u8 tc;
	int i;
	bool pfc_en = adapter->dcb_cfg.pfc_mode_enable;

	if (adapter->ixgbe_ieee_pfc)
		pfc_en |= !!(adapter->ixgbe_ieee_pfc->pfc_en);

	if (!(adapter->flags & IXGBE_FLAG_DCB_ENABLED) || !pfc_en) {
		ixgbe_update_xoff_rx_lfc(adapter);
		return;
	}

	/* update stats for each tc, only valid with PFC enabled */
	for (i = 0; i < MAX_TX_PACKET_BUFFERS; i++) {
		u32 pxoffrxc;

		switch (hw->mac.type) {
		case ixgbe_mac_82598EB:
			pxoffrxc = IXGBE_READ_REG(hw, IXGBE_PXOFFRXC(i));
			break;
		default:
			pxoffrxc = IXGBE_READ_REG(hw, IXGBE_PXOFFRXCNT(i));
		}
		hwstats->pxoffrxc[i] += pxoffrxc;
		/* Get the TC for given UP */
		tc = netdev_get_prio_tc_map(adapter->netdev, i);
		xoff[tc] += pxoffrxc;
	}

	/* disarm tx queues that have received xoff frames */
	for (i = 0; i < adapter->num_tx_queues; i++) {
		struct ixgbe_ring *tx_ring = adapter->tx_ring[i];

		tc = tx_ring->dcb_tc;
		if (xoff[tc])
			clear_bit(__IXGBE_HANG_CHECK_ARMED, &tx_ring->state);
	}

	for (i = 0; i < adapter->num_xdp_queues; i++) {
		struct ixgbe_ring *xdp_ring = adapter->xdp_ring[i];

		tc = xdp_ring->dcb_tc;
		if (xoff[tc])
			clear_bit(__IXGBE_HANG_CHECK_ARMED, &xdp_ring->state);
	}
}

static u64 ixgbe_get_tx_completed(struct ixgbe_ring *ring)
{
	return ring->stats.packets;
}

static u64 ixgbe_get_tx_pending(struct ixgbe_ring *ring)
{
	unsigned int head, tail;

	head = ring->next_to_clean;
	tail = ring->next_to_use;

	return ((head <= tail) ? tail : tail + ring->count) - head;
}

static bool ixgbe_check_tx_hang(struct ixgbe_ring *tx_ring)
{
	u32 tx_done = ixgbe_get_tx_completed(tx_ring);
	u32 tx_done_old = tx_ring->tx_stats.tx_done_old;
	u32 tx_pending = ixgbe_get_tx_pending(tx_ring);

	clear_check_for_tx_hang(tx_ring);

	/*
	 * Check for a hung queue, but be thorough. This verifies
	 * that a transmit has been completed since the previous
	 * check AND there is at least one packet pending. The
	 * ARMED bit is set to indicate a potential hang. The
	 * bit is cleared if a pause frame is received to remove
	 * false hang detection due to PFC or 802.3x frames. By
	 * requiring this to fail twice we avoid races with
	 * pfc clearing the ARMED bit and conditions where we
	 * run the check_tx_hang logic with a transmit completion
	 * pending but without time to complete it yet.
	 */
	if (tx_done_old == tx_done && tx_pending)
		/* make sure it is true for two checks in a row */
		return test_and_set_bit(__IXGBE_HANG_CHECK_ARMED,
					&tx_ring->state);
	/* update completed stats and continue */
	tx_ring->tx_stats.tx_done_old = tx_done;
	/* reset the countdown */
	clear_bit(__IXGBE_HANG_CHECK_ARMED, &tx_ring->state);

	return false;
}

/**
 * ixgbe_tx_timeout_reset - initiate reset due to Tx timeout
 * @adapter: driver private struct
 **/
static void ixgbe_tx_timeout_reset(struct ixgbe_adapter *adapter)
{

	/* Do the reset outside of interrupt context */
	if (!test_bit(__IXGBE_DOWN, &adapter->state)) {
		set_bit(__IXGBE_RESET_REQUESTED, &adapter->state);
		e_warn(drv, "initiating reset due to tx timeout\n");
		ixgbe_service_event_schedule(adapter);
	}
}

/**
 * ixgbe_tx_maxrate - callback to set the maximum per-queue bitrate
 * @netdev: network interface device structure
 * @queue_index: Tx queue to set
 * @maxrate: desired maximum transmit bitrate
 **/
static int ixgbe_tx_maxrate(struct net_device *netdev,
			    int queue_index, u32 maxrate)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;
	u32 bcnrc_val = ixgbe_link_mbps(adapter);

	if (!maxrate)
		return 0;

	/* Calculate the rate factor values to set */
	bcnrc_val <<= IXGBE_RTTBCNRC_RF_INT_SHIFT;
	bcnrc_val /= maxrate;

	/* clear everything but the rate factor */
	bcnrc_val &= IXGBE_RTTBCNRC_RF_INT_MASK |
	IXGBE_RTTBCNRC_RF_DEC_MASK;

	/* enable the rate scheduler */
	bcnrc_val |= IXGBE_RTTBCNRC_RS_ENA;

	IXGBE_WRITE_REG(hw, IXGBE_RTTDQSEL, queue_index);
	IXGBE_WRITE_REG(hw, IXGBE_RTTBCNRC, bcnrc_val);

	return 0;
}

/**
 * ixgbe_update_tx_ring_stats - Update Tx ring specific counters
 * @tx_ring: ring to update
 * @q_vector: queue vector ring belongs to
 * @pkts: number of processed packets
 * @bytes: number of processed bytes
 */
void ixgbe_update_tx_ring_stats(struct ixgbe_ring *tx_ring,
				struct ixgbe_q_vector *q_vector, u64 pkts,
				u64 bytes)
{
	u64_stats_update_begin(&tx_ring->syncp);
	tx_ring->stats.bytes += bytes;
	tx_ring->stats.packets += pkts;
	u64_stats_update_end(&tx_ring->syncp);
	q_vector->tx.total_bytes += bytes;
	q_vector->tx.total_packets += pkts;
}

/**
 * ixgbe_update_rx_ring_stats - Update Rx ring specific counters
 * @rx_ring: ring to update
 * @q_vector: queue vector ring belongs to
 * @pkts: number of processed packets
 * @bytes: number of processed bytes
 */
void ixgbe_update_rx_ring_stats(struct ixgbe_ring *rx_ring,
				struct ixgbe_q_vector *q_vector, u64 pkts,
				u64 bytes)
{
	u64_stats_update_begin(&rx_ring->syncp);
	rx_ring->stats.bytes += bytes;
	rx_ring->stats.packets += pkts;
	u64_stats_update_end(&rx_ring->syncp);
	q_vector->rx.total_bytes += bytes;
	q_vector->rx.total_packets += pkts;
}

/**
 * ixgbe_clean_tx_irq - Reclaim resources after transmit completes
 * @q_vector: structure containing interrupt and ring information
 * @tx_ring: tx ring to clean
 * @napi_budget: Used to determine if we are in netpoll
 **/
static bool ixgbe_clean_tx_irq(struct ixgbe_q_vector *q_vector,
			       struct ixgbe_ring *tx_ring, int napi_budget)
{
	struct ixgbe_adapter *adapter = q_vector->adapter;
	struct ixgbe_tx_buffer *tx_buffer;
	union ixgbe_adv_tx_desc *tx_desc;
	unsigned int total_bytes = 0, total_packets = 0, total_ipsec = 0;
	unsigned int budget = q_vector->tx.work_limit;
	unsigned int i = tx_ring->next_to_clean;
	struct netdev_queue *txq;

	if (test_bit(__IXGBE_DOWN, &adapter->state))
		return true;

	tx_buffer = &tx_ring->tx_buffer_info[i];
	tx_desc = IXGBE_TX_DESC(tx_ring, i);
	i -= tx_ring->count;

	do {
		union ixgbe_adv_tx_desc *eop_desc = tx_buffer->next_to_watch;

		/* if next_to_watch is not set then there is no work pending */
		if (!eop_desc)
			break;

		/* prevent any other reads prior to eop_desc */
		smp_rmb();

		/* if DD is not set pending work has not been completed */
		if (!(eop_desc->wb.status & cpu_to_le32(IXGBE_TXD_STAT_DD)))
			break;

		/* clear next_to_watch to prevent false hangs */
		tx_buffer->next_to_watch = NULL;

		/* update the statistics for this packet */
		total_bytes += tx_buffer->bytecount;
		total_packets += tx_buffer->gso_segs;
		if (tx_buffer->tx_flags & IXGBE_TX_FLAGS_IPSEC)
			total_ipsec++;

		/* free the skb */
		if (ring_is_xdp(tx_ring))
			xdp_return_frame(tx_buffer->xdpf);
		else
			napi_consume_skb(tx_buffer->skb, napi_budget);

		/* unmap skb header data */
		dma_unmap_single(tx_ring->dev,
				 dma_unmap_addr(tx_buffer, dma),
				 dma_unmap_len(tx_buffer, len),
				 DMA_TO_DEVICE);

		/* clear tx_buffer data */
		dma_unmap_len_set(tx_buffer, len, 0);

		/* unmap remaining buffers */
		while (tx_desc != eop_desc) {
			tx_buffer++;
			tx_desc++;
			i++;
			if (unlikely(!i)) {
				i -= tx_ring->count;
				tx_buffer = tx_ring->tx_buffer_info;
				tx_desc = IXGBE_TX_DESC(tx_ring, 0);
			}

			/* unmap any remaining paged data */
			if (dma_unmap_len(tx_buffer, len)) {
				dma_unmap_page(tx_ring->dev,
					       dma_unmap_addr(tx_buffer, dma),
					       dma_unmap_len(tx_buffer, len),
					       DMA_TO_DEVICE);
				dma_unmap_len_set(tx_buffer, len, 0);
			}
		}

		/* move us one more past the eop_desc for start of next pkt */
		tx_buffer++;
		tx_desc++;
		i++;
		if (unlikely(!i)) {
			i -= tx_ring->count;
			tx_buffer = tx_ring->tx_buffer_info;
			tx_desc = IXGBE_TX_DESC(tx_ring, 0);
		}

		/* issue prefetch for next Tx descriptor */
		prefetch(tx_desc);

		/* update budget accounting */
		budget--;
	} while (likely(budget));

	i += tx_ring->count;
	tx_ring->next_to_clean = i;
	ixgbe_update_tx_ring_stats(tx_ring, q_vector, total_packets,
				   total_bytes);
	adapter->tx_ipsec += total_ipsec;

	if (check_for_tx_hang(tx_ring) && ixgbe_check_tx_hang(tx_ring)) {
		/* schedule immediate reset if we believe we hung */
		struct ixgbe_hw *hw = &adapter->hw;
		e_err(drv, "Detected Tx Unit Hang %s\n"
			"  Tx Queue             <%d>\n"
			"  TDH, TDT             <%x>, <%x>\n"
			"  next_to_use          <%x>\n"
			"  next_to_clean        <%x>\n"
			"tx_buffer_info[next_to_clean]\n"
			"  time_stamp           <%lx>\n"
			"  jiffies              <%lx>\n",
			ring_is_xdp(tx_ring) ? "(XDP)" : "",
			tx_ring->queue_index,
			IXGBE_READ_REG(hw, IXGBE_TDH(tx_ring->reg_idx)),
			IXGBE_READ_REG(hw, IXGBE_TDT(tx_ring->reg_idx)),
			tx_ring->next_to_use, i,
			tx_ring->tx_buffer_info[i].time_stamp, jiffies);

		if (!ring_is_xdp(tx_ring))
			netif_stop_subqueue(tx_ring->netdev,
					    tx_ring->queue_index);

		e_info(probe,
		       "tx hang %d detected on queue %d, resetting adapter\n",
			adapter->tx_timeout_count + 1, tx_ring->queue_index);

		/* schedule immediate reset if we believe we hung */
		ixgbe_tx_timeout_reset(adapter);

		/* the adapter is about to reset, no point in enabling stuff */
		return true;
	}

	if (ring_is_xdp(tx_ring))
		return !!budget;

#define TX_WAKE_THRESHOLD (DESC_NEEDED * 2)
	txq = netdev_get_tx_queue(tx_ring->netdev, tx_ring->queue_index);
	if (!__netif_txq_completed_wake(txq, total_packets, total_bytes,
					ixgbe_desc_unused(tx_ring),
					TX_WAKE_THRESHOLD,
					!netif_carrier_ok(tx_ring->netdev) ||
					test_bit(__IXGBE_DOWN, &adapter->state)))
		++tx_ring->tx_stats.restart_queue;

	return !!budget;
}

#ifdef CONFIG_IXGBE_DCA
static void ixgbe_update_tx_dca(struct ixgbe_adapter *adapter,
				struct ixgbe_ring *tx_ring,
				int cpu)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 txctrl = 0;
	u16 reg_offset;

	if (adapter->flags & IXGBE_FLAG_DCA_ENABLED)
		txctrl = dca3_get_tag(tx_ring->dev, cpu);

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		reg_offset = IXGBE_DCA_TXCTRL(tx_ring->reg_idx);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
		reg_offset = IXGBE_DCA_TXCTRL_82599(tx_ring->reg_idx);
		txctrl <<= IXGBE_DCA_TXCTRL_CPUID_SHIFT_82599;
		break;
	default:
		/* for unknown hardware do not write register */
		return;
	}

	/*
	 * We can enable relaxed ordering for reads, but not writes when
	 * DCA is enabled.  This is due to a known issue in some chipsets
	 * which will cause the DCA tag to be cleared.
	 */
	txctrl |= IXGBE_DCA_TXCTRL_DESC_RRO_EN |
		  IXGBE_DCA_TXCTRL_DATA_RRO_EN |
		  IXGBE_DCA_TXCTRL_DESC_DCA_EN;

	IXGBE_WRITE_REG(hw, reg_offset, txctrl);
}

static void ixgbe_update_rx_dca(struct ixgbe_adapter *adapter,
				struct ixgbe_ring *rx_ring,
				int cpu)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 rxctrl = 0;
	u8 reg_idx = rx_ring->reg_idx;

	if (adapter->flags & IXGBE_FLAG_DCA_ENABLED)
		rxctrl = dca3_get_tag(rx_ring->dev, cpu);

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
		rxctrl <<= IXGBE_DCA_RXCTRL_CPUID_SHIFT_82599;
		break;
	default:
		break;
	}

	/*
	 * We can enable relaxed ordering for reads, but not writes when
	 * DCA is enabled.  This is due to a known issue in some chipsets
	 * which will cause the DCA tag to be cleared.
	 */
	rxctrl |= IXGBE_DCA_RXCTRL_DESC_RRO_EN |
		  IXGBE_DCA_RXCTRL_DATA_DCA_EN |
		  IXGBE_DCA_RXCTRL_DESC_DCA_EN;

	IXGBE_WRITE_REG(hw, IXGBE_DCA_RXCTRL(reg_idx), rxctrl);
}

static void ixgbe_update_dca(struct ixgbe_q_vector *q_vector)
{
	struct ixgbe_adapter *adapter = q_vector->adapter;
	struct ixgbe_ring *ring;
	int cpu = get_cpu();

	if (q_vector->cpu == cpu)
		goto out_no_update;

	ixgbe_for_each_ring(ring, q_vector->tx)
		ixgbe_update_tx_dca(adapter, ring, cpu);

	ixgbe_for_each_ring(ring, q_vector->rx)
		ixgbe_update_rx_dca(adapter, ring, cpu);

	q_vector->cpu = cpu;
out_no_update:
	put_cpu();
}

static void ixgbe_setup_dca(struct ixgbe_adapter *adapter)
{
	int i;

	/* always use CB2 mode, difference is masked in the CB driver */
	if (adapter->flags & IXGBE_FLAG_DCA_ENABLED)
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_DCA_CTRL,
				IXGBE_DCA_CTRL_DCA_MODE_CB2);
	else
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_DCA_CTRL,
				IXGBE_DCA_CTRL_DCA_DISABLE);

	for (i = 0; i < adapter->num_q_vectors; i++) {
		adapter->q_vector[i]->cpu = -1;
		ixgbe_update_dca(adapter->q_vector[i]);
	}
}

static int __ixgbe_notify_dca(struct device *dev, void *data)
{
	struct ixgbe_adapter *adapter = dev_get_drvdata(dev);
	unsigned long event = *(unsigned long *)data;

	if (!(adapter->flags & IXGBE_FLAG_DCA_CAPABLE))
		return 0;

	switch (event) {
	case DCA_PROVIDER_ADD:
		/* if we're already enabled, don't do it again */
		if (adapter->flags & IXGBE_FLAG_DCA_ENABLED)
			break;
		if (dca_add_requester(dev) == 0) {
			adapter->flags |= IXGBE_FLAG_DCA_ENABLED;
			IXGBE_WRITE_REG(&adapter->hw, IXGBE_DCA_CTRL,
					IXGBE_DCA_CTRL_DCA_MODE_CB2);
			break;
		}
		fallthrough; /* DCA is disabled. */
	case DCA_PROVIDER_REMOVE:
		if (adapter->flags & IXGBE_FLAG_DCA_ENABLED) {
			dca_remove_requester(dev);
			adapter->flags &= ~IXGBE_FLAG_DCA_ENABLED;
			IXGBE_WRITE_REG(&adapter->hw, IXGBE_DCA_CTRL,
					IXGBE_DCA_CTRL_DCA_DISABLE);
		}
		break;
	}

	return 0;
}

#endif /* CONFIG_IXGBE_DCA */

#define IXGBE_RSS_L4_TYPES_MASK \
	((1ul << IXGBE_RXDADV_RSSTYPE_IPV4_TCP) | \
	 (1ul << IXGBE_RXDADV_RSSTYPE_IPV4_UDP) | \
	 (1ul << IXGBE_RXDADV_RSSTYPE_IPV6_TCP) | \
	 (1ul << IXGBE_RXDADV_RSSTYPE_IPV6_UDP))

static inline void ixgbe_rx_hash(struct ixgbe_ring *ring,
				 union ixgbe_adv_rx_desc *rx_desc,
				 struct sk_buff *skb)
{
	u16 rss_type;

	if (!(ring->netdev->features & NETIF_F_RXHASH))
		return;

	rss_type = le16_to_cpu(rx_desc->wb.lower.lo_dword.hs_rss.pkt_info) &
		   IXGBE_RXDADV_RSSTYPE_MASK;

	if (!rss_type)
		return;

	skb_set_hash(skb, le32_to_cpu(rx_desc->wb.lower.hi_dword.rss),
		     (IXGBE_RSS_L4_TYPES_MASK & (1ul << rss_type)) ?
		     PKT_HASH_TYPE_L4 : PKT_HASH_TYPE_L3);
}

#ifdef IXGBE_FCOE
/**
 * ixgbe_rx_is_fcoe - check the rx desc for incoming pkt type
 * @ring: structure containing ring specific data
 * @rx_desc: advanced rx descriptor
 *
 * Returns : true if it is FCoE pkt
 */
static inline bool ixgbe_rx_is_fcoe(struct ixgbe_ring *ring,
				    union ixgbe_adv_rx_desc *rx_desc)
{
	__le16 pkt_info = rx_desc->wb.lower.lo_dword.hs_rss.pkt_info;

	return test_bit(__IXGBE_RX_FCOE, &ring->state) &&
	       ((pkt_info & cpu_to_le16(IXGBE_RXDADV_PKTTYPE_ETQF_MASK)) ==
		(cpu_to_le16(IXGBE_ETQF_FILTER_FCOE <<
			     IXGBE_RXDADV_PKTTYPE_ETQF_SHIFT)));
}

#endif /* IXGBE_FCOE */
/**
 * ixgbe_rx_checksum - indicate in skb if hw indicated a good cksum
 * @ring: structure containing ring specific data
 * @rx_desc: current Rx descriptor being processed
 * @skb: skb currently being received and modified
 **/
static inline void ixgbe_rx_checksum(struct ixgbe_ring *ring,
				     union ixgbe_adv_rx_desc *rx_desc,
				     struct sk_buff *skb)
{
	__le16 pkt_info = rx_desc->wb.lower.lo_dword.hs_rss.pkt_info;
	bool encap_pkt = false;

	skb_checksum_none_assert(skb);

	/* Rx csum disabled */
	if (!(ring->netdev->features & NETIF_F_RXCSUM))
		return;

	/* check for VXLAN and Geneve packets */
	if (pkt_info & cpu_to_le16(IXGBE_RXDADV_PKTTYPE_VXLAN)) {
		encap_pkt = true;
		skb->encapsulation = 1;
	}

	/* if IP and error */
	if (ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_IPCS) &&
	    ixgbe_test_staterr(rx_desc, IXGBE_RXDADV_ERR_IPE)) {
		ring->rx_stats.csum_err++;
		return;
	}

	if (!ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_L4CS))
		return;

	if (ixgbe_test_staterr(rx_desc, IXGBE_RXDADV_ERR_TCPE)) {
		/*
		 * 82599 errata, UDP frames with a 0 checksum can be marked as
		 * checksum errors.
		 */
		if ((pkt_info & cpu_to_le16(IXGBE_RXDADV_PKTTYPE_UDP)) &&
		    test_bit(__IXGBE_RX_CSUM_UDP_ZERO_ERR, &ring->state))
			return;

		ring->rx_stats.csum_err++;
		return;
	}

	/* It must be a TCP or UDP packet with a valid checksum */
	skb->ip_summed = CHECKSUM_UNNECESSARY;
	if (encap_pkt) {
		if (!ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_OUTERIPCS))
			return;

		if (ixgbe_test_staterr(rx_desc, IXGBE_RXDADV_ERR_OUTERIPER)) {
			skb->ip_summed = CHECKSUM_NONE;
			return;
		}
		/* If we checked the outer header let the stack know */
		skb->csum_level = 1;
	}
}

static unsigned int ixgbe_rx_offset(struct ixgbe_ring *rx_ring)
{
	return ring_uses_build_skb(rx_ring) ? IXGBE_SKB_PAD : 0;
}

static bool ixgbe_alloc_mapped_page(struct ixgbe_ring *rx_ring,
				    struct ixgbe_rx_buffer *bi)
{
	struct page *page = bi->page;
	dma_addr_t dma;

	/* since we are recycling buffers we should seldom need to alloc */
	if (likely(page))
		return true;

	/* alloc new page for storage */
	page = dev_alloc_pages(ixgbe_rx_pg_order(rx_ring));
	if (unlikely(!page)) {
		rx_ring->rx_stats.alloc_rx_page_failed++;
		return false;
	}

	/* map page for use */
	dma = dma_map_page_attrs(rx_ring->dev, page, 0,
				 ixgbe_rx_pg_size(rx_ring),
				 DMA_FROM_DEVICE,
				 IXGBE_RX_DMA_ATTR);

	/*
	 * if mapping failed free memory back to system since
	 * there isn't much point in holding memory we can't use
	 */
	if (dma_mapping_error(rx_ring->dev, dma)) {
		__free_pages(page, ixgbe_rx_pg_order(rx_ring));

		rx_ring->rx_stats.alloc_rx_page_failed++;
		return false;
	}

	bi->dma = dma;
	bi->page = page;
	bi->page_offset = rx_ring->rx_offset;
	page_ref_add(page, USHRT_MAX - 1);
	bi->pagecnt_bias = USHRT_MAX;
	rx_ring->rx_stats.alloc_rx_page++;

	return true;
}

/**
 * ixgbe_alloc_rx_buffers - refill free RX descriptors
 * @rx_ring : target RX ring
 * @cleaned : number of descriptors released in the current poll cycle
 *
 * Guarantees that the NIC never sees an un-initialised descriptor and
 * that CPU modifications on recycled pages are flushed (DMA_TO_DEVICE)
 * before the adapter DMAs into them.
 */
void ixgbe_alloc_rx_buffers(struct ixgbe_ring *rx_ring, u16 cleaned) {
    union ixgbe_adv_rx_desc *rxd;
    struct ixgbe_rx_buffer *bi;
    u16 ntu = rx_ring->next_to_use;
    u16 buf_sz = ixgbe_rx_bufsz(rx_ring);

    if (unlikely(!cleaned)) {
        return;
    }

    rxd = IXGBE_RX_DESC(rx_ring, ntu);
    bi = &rx_ring->rx_buffer_info[ntu];

    prefetchw(bi);
    prefetch(rxd);

    while (cleaned--) {
        dma_addr_t old_dma = bi->dma;

        if (unlikely(!ixgbe_alloc_mapped_page(rx_ring, bi))) {
            rx_ring->rx_stats.alloc_rx_buff_failed++;
            break;
        }

        /* flush CPU writes on recycled pages (no sync_for_cpu needed pre-NIC-DMA) */
        if (old_dma && old_dma == bi->dma) {
            dma_sync_single_range_for_device(rx_ring->dev, bi->dma,
                                             bi->page_offset, buf_sz,
                                             DMA_TO_DEVICE);
        }

        rxd->read.pkt_addr = cpu_to_le64(bi->dma + bi->page_offset);
        rxd->wb.upper.length = 0;

        if (unlikely(++ntu == rx_ring->count)) {
            ntu = 0;
            rxd = IXGBE_RX_DESC(rx_ring, 0);
            bi = rx_ring->rx_buffer_info;
        } else {
            ++rxd;
            ++bi;
        }
    }

    if (rx_ring->next_to_use == ntu) {
        return;
    }

    rx_ring->next_to_use = ntu;
    rx_ring->next_to_alloc = ntu;

    wmb();  /* descriptors first */
    writel(ntu, rx_ring->tail);
}

/**
 * ixgbe_set_rsc_gso_size - compute GSO fields for an RSC frame
 * @ring:    owning ring (current stats not used)
 * @skb:     coalesced skb
 * @rx_desc: EOP descriptor that completed the RSC frame
 *
 * 82599 family reports MSS in mss_l4_len_idx; X540 does not.
 */
static void ixgbe_set_rsc_gso_size(struct ixgbe_ring *ring,
                                    struct sk_buff *skb,
                                    union ixgbe_adv_rx_desc *rx_desc) {
    u16 hdr = skb_transport_offset(skb);
    u16 append = IXGBE_CB(skb)->append_cnt ?: 1; /* avoid div/0 */
    u16 mss = 0;

#ifdef IXGBE_RXDADV_RSCCMSS_MASK  /* only 82599 has this */
    {
        u32 mss_idx = le32_to_cpu(rx_desc->wb.lower.lo_dword.mss_l4_len_idx);

        mss = (mss_idx & IXGBE_RXDADV_RSCCMSS_MASK) >> IXGBE_RXDADV_RSCCMSS_SHIFT;
    }
#endif

    if (unlikely(!mss)) {  /* X540 or value missing */
        mss = DIV_ROUND_UP(skb->len - hdr, append);
    }

    skb_shinfo(skb)->gso_size = mss;
    skb_shinfo(skb)->gso_segs = DIV_ROUND_UP(skb->len - hdr, mss);
    skb_shinfo(skb)->gso_type = SKB_GSO_TCPV4; /* RSC does IPv4 TCP */
}

static void ixgbe_update_rsc_stats(struct ixgbe_ring *rx_ring,
                                    union ixgbe_adv_rx_desc *rx_desc,
                                    struct sk_buff *skb) {
    if (!IXGBE_CB(skb)->append_cnt) {  /* not RSC */
        /* wipe stale meta if skb recycled */
        if (skb_is_gso(skb)) {
            struct skb_shared_info *sh = skb_shinfo(skb);

            sh->gso_size = 0;
            sh->gso_type = 0;
            sh->gso_segs = 0;
        }
        return;
    }

    rx_ring->rx_stats.rsc_count += IXGBE_CB(skb)->append_cnt;
    rx_ring->rx_stats.rsc_flush++;

    ixgbe_set_rsc_gso_size(rx_ring, skb, rx_desc);

    IXGBE_CB(skb)->append_cnt = 0;  /* always clear last */
}

/**
 * ixgbe_process_skb_fields - Populate skb header fields from Rx descriptor
 * @rx_ring: rx descriptor ring packet is being transacted on
 * @rx_desc: pointer to the EOP Rx descriptor
 * @skb: pointer to current skb being populated
 *
 * This function checks the ring, descriptor, and packet information in
 * order to populate the hash, checksum, VLAN, timestamp, protocol, and
 * other fields within the skb.
 */
void ixgbe_process_skb_fields(struct ixgbe_ring *rx_ring,
                              union ixgbe_adv_rx_desc *rx_desc,
                              struct sk_buff *skb) {
    struct net_device *dev = rx_ring->netdev;
    u32 flags = rx_ring->q_vector->adapter->flags;

    ixgbe_update_rsc_stats(rx_ring, rx_desc, skb);

    ixgbe_rx_hash(rx_ring, rx_desc, skb);
    ixgbe_rx_checksum(rx_ring, rx_desc, skb);

    if (unlikely(flags & IXGBE_FLAG_RX_HWTSTAMP_ENABLED)) {
        ixgbe_ptp_rx_hwtstamp(rx_ring, rx_desc, skb);
    }

    if ((dev->features & NETIF_F_HW_VLAN_CTAG_RX) &&
        ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_VP)) {
        u16 vid = le16_to_cpu(rx_desc->wb.upper.vlan);
        __vlan_hwaccel_put_tag(skb, htons(ETH_P_8021Q), vid);
    }

    if (ixgbe_test_staterr(rx_desc, IXGBE_RXDADV_STAT_SECP)) {
        ixgbe_ipsec_rx(rx_ring, rx_desc, skb);
    }

    if (unlikely(!netif_is_ixgbe(dev))) {
        macvlan_count_rx(netdev_priv(dev), skb->len + ETH_HLEN, true, false);
    } else {
        skb_record_rx_queue(skb, rx_ring->queue_index);
    }

    skb->protocol = eth_type_trans(skb, dev);
}

void ixgbe_rx_skb(struct ixgbe_q_vector *q_vector,
                  struct sk_buff *skb) {
    napi_gro_receive(&q_vector->napi, skb);
}

/**
 * ixgbe_is_non_eop - process handling of non-EOP buffers
 * @rx_ring: Rx ring being processed
 * @rx_desc: Rx descriptor for current buffer
 * @skb: Current socket buffer containing buffer in progress
 *
 * This function updates next to clean.  If the buffer is an EOP buffer
 * this function exits returning false, otherwise it will place the
 * sk_buff in the next buffer to be chained and return true indicating
 * that this is in fact a non-EOP buffer.
 */
static bool ixgbe_is_non_eop(struct ixgbe_ring *rx_ring,
                             union ixgbe_adv_rx_desc *rx_desc,
                             struct sk_buff *skb) {
    u32 ntc = rx_ring->next_to_clean + 1;

    /* fetch, update, and store next to clean */
    ntc = (ntc < rx_ring->count) ? ntc : 0;
    rx_ring->next_to_clean = ntc;

    prefetch(IXGBE_RX_DESC(rx_ring, ntc));

    /* update RSC append count if present */
    if (unlikely(ring_is_rsc_enabled(rx_ring))) {
        __le32 rsc_enabled = rx_desc->wb.lower.lo_dword.data &
                             cpu_to_le32(IXGBE_RXDADV_RSCCNT_MASK);

        if (unlikely(rsc_enabled)) {
            u32 rsc_cnt = le32_to_cpu(rsc_enabled);

            rsc_cnt >>= IXGBE_RXDADV_RSCCNT_SHIFT;
            IXGBE_CB(skb)->append_cnt += rsc_cnt - 1;

            /* update ntc based on RSC value */
            ntc = le32_to_cpu(rx_desc->wb.upper.status_error);
            ntc &= IXGBE_RXDADV_NEXTP_MASK;
            ntc >>= IXGBE_RXDADV_NEXTP_SHIFT;
        }
    }

    /* if we are the last buffer then there is nothing else to do */
    if (likely(ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_EOP))) {
        return false;
    }

    /* place skb in next buffer to be received */
    rx_ring->rx_buffer_info[ntc].skb = skb;
    rx_ring->rx_stats.non_eop_descs++;

    return true;
}

/**
 * ixgbe_pull_tail - ixgbe specific version of skb_pull_tail
 * @rx_ring: rx descriptor ring packet is being transacted on
 * @skb: pointer to current skb being adjusted
 *
 * This function is an ixgbe specific version of __pskb_pull_tail.  The
 * main difference between this version and the original function is that
 * this function can make several assumptions about the state of things
 * that allow for significant optimizations versus the standard function.
 * As a result we can do things like drop a frag and maintain an accurate
 * truesize for the skb.
 */
static void ixgbe_pull_tail(struct ixgbe_ring *rx_ring,
                            struct sk_buff *skb) {
    skb_frag_t *frag = &skb_shinfo(skb)->frags[0];
    unsigned char *va;
    unsigned int pull_len;

    /*
     * it is valid to use page_address instead of kmap since we are
     * working with pages allocated out of the lomem pool per
     * alloc_page(GFP_ATOMIC)
     */
    va = skb_frag_address(frag);

    /*
     * we need the header to contain the greater of either ETH_HLEN or
     * 60 bytes if the skb->len is less than 60 for skb_pad.
     */
    pull_len = eth_get_headlen(skb->dev, va, IXGBE_RX_HDR_SIZE);

    /* align pull length to size of long to optimize memcpy performance */
    pull_len = ALIGN(pull_len, sizeof(long));

    /* Clamp pull_len to avoid underflow (rare, but possible if headlen > frag) */
    pull_len = min(pull_len, skb_frag_size(frag));

    skb_copy_to_linear_data(skb, va, pull_len);

    /* update all of the pointers */
    skb_frag_size_sub(frag, pull_len);
    skb_frag_off_add(frag, pull_len);
    skb->data_len -= pull_len;
    skb->tail += pull_len;
}

/**
 * ixgbe_dma_sync_frag - perform DMA sync for first frag of SKB
 * @rx_ring: rx descriptor ring packet is being transacted on
 * @skb: pointer to current skb being updated
 *
 * This function provides a basic DMA sync up for the first fragment of an
 * skb.  The reason for doing this is that the first fragment cannot be
 * unmapped until we have reached the end of packet descriptor for a buffer
 * chain.
 */
static void ixgbe_dma_sync_frag(struct ixgbe_ring *rx_ring,
                                struct sk_buff *skb) {
    if (ring_uses_build_skb(rx_ring)) {
        unsigned long mask = (unsigned long)ixgbe_rx_pg_size(rx_ring) - 1;
        unsigned long offset = (unsigned long)(skb->data) & mask;

        dma_sync_single_range_for_cpu(rx_ring->dev, IXGBE_CB(skb)->dma,
                                      offset, skb_headlen(skb),
                                      DMA_FROM_DEVICE);
    } else {
        skb_frag_t *frag = &skb_shinfo(skb)->frags[0];

        dma_sync_single_range_for_cpu(rx_ring->dev, IXGBE_CB(skb)->dma,
                                      skb_frag_off(frag), skb_frag_size(frag),
                                      DMA_FROM_DEVICE);
    }

    /* If the page was released, just unmap it (with relaxed attrs for x540 perf). */
    if (unlikely(IXGBE_CB(skb)->page_released)) {
#ifdef DMA_ATTR_RELAXED_ORDERING
        dma_unmap_page_attrs(rx_ring->dev, IXGBE_CB(skb)->dma,
                             ixgbe_rx_pg_size(rx_ring), DMA_FROM_DEVICE,
                             DMA_ATTR_RELAXED_ORDERING);
#else
        dma_unmap_page(rx_ring->dev, IXGBE_CB(skb)->dma,
                       ixgbe_rx_pg_size(rx_ring), DMA_FROM_DEVICE);
#endif
    }
}

/**
 * ixgbe_cleanup_headers - Correct corrupted or empty headers
 * @rx_ring: rx descriptor ring packet is being transacted on
 * @rx_desc: pointer to the EOP Rx descriptor
 * @skb: pointer to current skb being fixed
 *
 * Check if the skb is valid in the XDP case it will be an error pointer.
 * Return true in this case to abort processing and advance to next
 * descriptor.
 *
 * Check for corrupted packet headers caused by senders on the local L2
 * embedded NIC switch not setting up their Tx Descriptors right.  These
 * should be very rare.
 *
 * Also address the case where we are pulling data in on pages only
 * and as such no data is present in the skb header.
 *
 * In addition if skb is not at least 60 bytes we need to pad it so that
 * it is large enough to qualify as a valid Ethernet frame.
 *
 * Returns true if an error was encountered and skb was freed.
 **/
bool ixgbe_cleanup_headers(struct ixgbe_ring *rx_ring,
			   union ixgbe_adv_rx_desc *rx_desc,
			   struct sk_buff *skb)
{
	struct net_device *netdev = rx_ring->netdev;

	/* Verify netdev is present, and that packet does not have any
	 * errors that would be unacceptable to the netdev.
	 */
	if (!netdev ||
	    (unlikely(ixgbe_test_staterr(rx_desc,
					 IXGBE_RXDADV_ERR_FRAME_ERR_MASK) &&
	     !(netdev->features & NETIF_F_RXALL)))) {
		dev_kfree_skb_any(skb);
		return true;
	}

	/* place header in linear portion of buffer */
	if (!skb_headlen(skb))
		ixgbe_pull_tail(rx_ring, skb);

#ifdef IXGBE_FCOE
	/* do not attempt to pad FCoE Frames as this will disrupt DDP */
	if (ixgbe_rx_is_fcoe(rx_ring, rx_desc))
		return false;

#endif
	/* if eth_skb_pad returns an error the skb was freed */
	if (eth_skb_pad(skb))
		return true;

	return false;
}

/**
 * ixgbe_reuse_rx_page - page flip buffer and store it back on the ring
 * @rx_ring: rx descriptor ring to store buffers on
 * @old_buff: donor buffer to have page reused
 *
 * Synchronizes page for reuse by the adapter
 **/
static void ixgbe_reuse_rx_page(struct ixgbe_ring *rx_ring,
				struct ixgbe_rx_buffer *old_buff)
{
	struct ixgbe_rx_buffer *new_buff;
	u16 nta = rx_ring->next_to_alloc;

	new_buff = &rx_ring->rx_buffer_info[nta];

	/* update, and store next to alloc */
	nta++;
	rx_ring->next_to_alloc = (nta < rx_ring->count) ? nta : 0;

	/* Transfer page from old buffer to new buffer.
	 * Move each member individually to avoid possible store
	 * forwarding stalls and unnecessary copy of skb.
	 */
	new_buff->dma		= old_buff->dma;
	new_buff->page		= old_buff->page;
	new_buff->page_offset	= old_buff->page_offset;
	new_buff->pagecnt_bias	= old_buff->pagecnt_bias;
}

static bool ixgbe_can_reuse_rx_page(struct ixgbe_rx_buffer *rx_buffer,
				    int rx_buffer_pgcnt)
{
	unsigned int pagecnt_bias = rx_buffer->pagecnt_bias;
	struct page *page = rx_buffer->page;

	/* avoid re-using remote and pfmemalloc pages */
	if (!dev_page_is_reusable(page))
		return false;

#if (PAGE_SIZE < 8192)
	/* if we are only owner of page we can reuse it */
	if (unlikely((rx_buffer_pgcnt - pagecnt_bias) > 1))
		return false;
#else
	/* The last offset is a bit aggressive in that we assume the
	 * worst case of FCoE being enabled and using a 3K buffer.
	 * However this should have minimal impact as the 1K extra is
	 * still less than one buffer in size.
	 */
#define IXGBE_LAST_OFFSET \
	(SKB_WITH_OVERHEAD(PAGE_SIZE) - IXGBE_RXBUFFER_3K)
	if (rx_buffer->page_offset > IXGBE_LAST_OFFSET)
		return false;
#endif

	/* If we have drained the page fragment pool we need to update
	 * the pagecnt_bias and page count so that we fully restock the
	 * number of references the driver holds.
	 */
	if (unlikely(pagecnt_bias == 1)) {
		page_ref_add(page, USHRT_MAX - 1);
		rx_buffer->pagecnt_bias = USHRT_MAX;
	}

	return true;
}

/**
 * ixgbe_add_rx_frag - Add contents of Rx buffer to sk_buff
 * @rx_ring: rx descriptor ring to transact packets on
 * @rx_buffer: buffer containing page to add
 * @skb: sk_buff to place the data into
 * @size: size of data in rx_buffer
 *
 * This function will add the data contained in rx_buffer->page to the skb.
 * This is done either through a direct copy if the data in the buffer is
 * less than the skb header size, otherwise it will just attach the page as
 * a frag to the skb.
 *
 * The function will then update the page offset if necessary and return
 * true if the buffer can be reused by the adapter.
 **/
static void ixgbe_add_rx_frag(struct ixgbe_ring *rx_ring,
			      struct ixgbe_rx_buffer *rx_buffer,
			      struct sk_buff *skb,
			      unsigned int size)
{
#if (PAGE_SIZE < 8192)
	unsigned int truesize = ixgbe_rx_pg_size(rx_ring) / 2;
#else
	unsigned int truesize = rx_ring->rx_offset ?
				SKB_DATA_ALIGN(rx_ring->rx_offset + size) :
				SKB_DATA_ALIGN(size);
#endif
	skb_add_rx_frag(skb, skb_shinfo(skb)->nr_frags, rx_buffer->page,
			rx_buffer->page_offset, size, truesize);
#if (PAGE_SIZE < 8192)
	rx_buffer->page_offset ^= truesize;
#else
	rx_buffer->page_offset += truesize;
#endif
}

static struct ixgbe_rx_buffer *ixgbe_get_rx_buffer(struct ixgbe_ring *rx_ring,
						   union ixgbe_adv_rx_desc *rx_desc,
						   struct sk_buff **skb,
						   const unsigned int size,
						   int *rx_buffer_pgcnt)
{
	struct ixgbe_rx_buffer *rx_buffer;

	rx_buffer = &rx_ring->rx_buffer_info[rx_ring->next_to_clean];
	*rx_buffer_pgcnt =
#if (PAGE_SIZE < 8192)
		page_count(rx_buffer->page);
#else
		0;
#endif
	prefetchw(rx_buffer->page);
	*skb = rx_buffer->skb;

	/* Delay unmapping of the first packet. It carries the header
	 * information, HW may still access the header after the writeback.
	 * Only unmap it when EOP is reached
	 */
	if (!ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_EOP)) {
		if (!*skb)
			goto skip_sync;
	} else {
		if (*skb)
			ixgbe_dma_sync_frag(rx_ring, *skb);
	}

	/* we are reusing so sync this buffer for CPU use */
	dma_sync_single_range_for_cpu(rx_ring->dev,
				      rx_buffer->dma,
				      rx_buffer->page_offset,
				      size,
				      DMA_FROM_DEVICE);
skip_sync:
	rx_buffer->pagecnt_bias--;

	return rx_buffer;
}

static void ixgbe_put_rx_buffer(struct ixgbe_ring *rx_ring,
				struct ixgbe_rx_buffer *rx_buffer,
				struct sk_buff *skb,
				int rx_buffer_pgcnt)
{
	if (ixgbe_can_reuse_rx_page(rx_buffer, rx_buffer_pgcnt)) {
		/* hand second half of page back to the ring */
		ixgbe_reuse_rx_page(rx_ring, rx_buffer);
	} else {
		if (skb && IXGBE_CB(skb)->dma == rx_buffer->dma) {
			/* the page has been released from the ring */
			IXGBE_CB(skb)->page_released = true;
		} else {
			/* we are not reusing the buffer so unmap it */
			dma_unmap_page_attrs(rx_ring->dev, rx_buffer->dma,
					     ixgbe_rx_pg_size(rx_ring),
					     DMA_FROM_DEVICE,
					     IXGBE_RX_DMA_ATTR);
		}
		__page_frag_cache_drain(rx_buffer->page,
					rx_buffer->pagecnt_bias);
	}

	/* clear contents of rx_buffer */
	rx_buffer->page = NULL;
	rx_buffer->skb = NULL;
}

static struct sk_buff *ixgbe_construct_skb(struct ixgbe_ring *rx_ring,
					   struct ixgbe_rx_buffer *rx_buffer,
					   struct xdp_buff *xdp,
					   union ixgbe_adv_rx_desc *rx_desc)
{
	unsigned int size = xdp->data_end - xdp->data;
#if (PAGE_SIZE < 8192)
	unsigned int truesize = ixgbe_rx_pg_size(rx_ring) / 2;
#else
	unsigned int truesize = SKB_DATA_ALIGN(xdp->data_end -
					       xdp->data_hard_start);
#endif
	struct sk_buff *skb;

	/* prefetch first cache line of first page */
	net_prefetch(xdp->data);

	/* Note, we get here by enabling legacy-rx via:
	 *
	 *    ethtool --set-priv-flags <dev> legacy-rx on
	 *
	 * In this mode, we currently get 0 extra XDP headroom as
	 * opposed to having legacy-rx off, where we process XDP
	 * packets going to stack via ixgbe_build_skb(). The latter
	 * provides us currently with 192 bytes of headroom.
	 *
	 * For ixgbe_construct_skb() mode it means that the
	 * xdp->data_meta will always point to xdp->data, since
	 * the helper cannot expand the head. Should this ever
	 * change in future for legacy-rx mode on, then lets also
	 * add xdp->data_meta handling here.
	 */

	/* allocate a skb to store the frags */
	skb = napi_alloc_skb(&rx_ring->q_vector->napi, IXGBE_RX_HDR_SIZE);
	if (unlikely(!skb))
		return NULL;

	if (size > IXGBE_RX_HDR_SIZE) {
		if (!ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_EOP))
			IXGBE_CB(skb)->dma = rx_buffer->dma;

		skb_add_rx_frag(skb, 0, rx_buffer->page,
				xdp->data - page_address(rx_buffer->page),
				size, truesize);
#if (PAGE_SIZE < 8192)
		rx_buffer->page_offset ^= truesize;
#else
		rx_buffer->page_offset += truesize;
#endif
	} else {
		memcpy(__skb_put(skb, size),
		       xdp->data, ALIGN(size, sizeof(long)));
		rx_buffer->pagecnt_bias++;
	}

	return skb;
}

static struct sk_buff *ixgbe_build_skb(struct ixgbe_ring *rx_ring,
				       struct ixgbe_rx_buffer *rx_buffer,
				       struct xdp_buff *xdp,
				       union ixgbe_adv_rx_desc *rx_desc)
{
	unsigned int metasize = xdp->data - xdp->data_meta;
#if (PAGE_SIZE < 8192)
	unsigned int truesize = ixgbe_rx_pg_size(rx_ring) / 2;
#else
	unsigned int truesize = SKB_DATA_ALIGN(sizeof(struct skb_shared_info)) +
				SKB_DATA_ALIGN(xdp->data_end -
					       xdp->data_hard_start);
#endif
	struct sk_buff *skb;

	/* Prefetch first cache line of first page. If xdp->data_meta
	 * is unused, this points extactly as xdp->data, otherwise we
	 * likely have a consumer accessing first few bytes of meta
	 * data, and then actual data.
	 */
	net_prefetch(xdp->data_meta);

	/* build an skb to around the page buffer */
	skb = napi_build_skb(xdp->data_hard_start, truesize);
	if (unlikely(!skb))
		return NULL;

	/* update pointers within the skb to store the data */
	skb_reserve(skb, xdp->data - xdp->data_hard_start);
	__skb_put(skb, xdp->data_end - xdp->data);
	if (metasize)
		skb_metadata_set(skb, metasize);

	/* record DMA address if this is the start of a chain of buffers */
	if (!ixgbe_test_staterr(rx_desc, IXGBE_RXD_STAT_EOP))
		IXGBE_CB(skb)->dma = rx_buffer->dma;

	/* update buffer offset */
#if (PAGE_SIZE < 8192)
	rx_buffer->page_offset ^= truesize;
#else
	rx_buffer->page_offset += truesize;
#endif

	return skb;
}

static int ixgbe_run_xdp(struct ixgbe_adapter *adapter,
			 struct ixgbe_ring *rx_ring,
			 struct xdp_buff *xdp)
{
	int err, result = IXGBE_XDP_PASS;
	struct bpf_prog *xdp_prog;
	struct ixgbe_ring *ring;
	struct xdp_frame *xdpf;
	u32 act;

	xdp_prog = READ_ONCE(rx_ring->xdp_prog);

	if (!xdp_prog)
		goto xdp_out;

	prefetchw(xdp->data_hard_start); /* xdp_frame write */

	act = bpf_prog_run_xdp(xdp_prog, xdp);
	switch (act) {
	case XDP_PASS:
		break;
	case XDP_TX:
		xdpf = xdp_convert_buff_to_frame(xdp);
		if (unlikely(!xdpf))
			goto out_failure;
		ring = ixgbe_determine_xdp_ring(adapter);
		if (static_branch_unlikely(&ixgbe_xdp_locking_key))
			spin_lock(&ring->tx_lock);
		result = ixgbe_xmit_xdp_ring(ring, xdpf);
		if (static_branch_unlikely(&ixgbe_xdp_locking_key))
			spin_unlock(&ring->tx_lock);
		if (result == IXGBE_XDP_CONSUMED)
			goto out_failure;
		break;
	case XDP_REDIRECT:
		err = xdp_do_redirect(adapter->netdev, xdp, xdp_prog);
		if (err)
			goto out_failure;
		result = IXGBE_XDP_REDIR;
		break;
	default:
		bpf_warn_invalid_xdp_action(rx_ring->netdev, xdp_prog, act);
		fallthrough;
	case XDP_ABORTED:
out_failure:
		trace_xdp_exception(rx_ring->netdev, xdp_prog, act);
		fallthrough; /* handle aborts by dropping packet */
	case XDP_DROP:
		result = IXGBE_XDP_CONSUMED;
		break;
	}
xdp_out:
	return result;
}

static unsigned int ixgbe_rx_frame_truesize(struct ixgbe_ring *rx_ring,
					    unsigned int size)
{
	unsigned int truesize;

#if (PAGE_SIZE < 8192)
	truesize = ixgbe_rx_pg_size(rx_ring) / 2; /* Must be power-of-2 */
#else
	truesize = rx_ring->rx_offset ?
		SKB_DATA_ALIGN(rx_ring->rx_offset + size) +
		SKB_DATA_ALIGN(sizeof(struct skb_shared_info)) :
		SKB_DATA_ALIGN(size);
#endif
	return truesize;
}

static void ixgbe_rx_buffer_flip(struct ixgbe_ring *rx_ring,
				 struct ixgbe_rx_buffer *rx_buffer,
				 unsigned int size)
{
	unsigned int truesize = ixgbe_rx_frame_truesize(rx_ring, size);
#if (PAGE_SIZE < 8192)
	rx_buffer->page_offset ^= truesize;
#else
	rx_buffer->page_offset += truesize;
#endif
}

/**
 * ixgbe_clean_rx_irq - Clean completed descriptors from Rx ring - bounce buf
 * @q_vector: structure containing interrupt and ring information
 * @rx_ring: rx descriptor ring to transact packets on
 * @budget: Total limit on number of packets to process
 *
 * This function provides a "bounce buffer" approach to Rx interrupt
 * processing.  The advantage to this is that on systems that have
 * expensive overhead for IOMMU access this provides a means of avoiding
 * it by maintaining the mapping of the page to the syste.
 *
 * Returns amount of work completed
 */
static int ixgbe_clean_rx_irq(struct ixgbe_q_vector *q_vector,
                              struct ixgbe_ring *rx_ring,
                              const int budget) {
    struct ixgbe_adapter *adapter = q_vector->adapter;
    unsigned int pkts = 0, bytes = 0, xdp_xmit = 0;
    u16 cleaned = 0;
    const unsigned int offset = rx_ring->rx_offset;
    struct xdp_buff xdp;
    unsigned int frame_sz = ixgbe_rx_frame_truesize(rx_ring, 0);

    xdp_init_buff(&xdp, frame_sz, &rx_ring->xdp_rxq);

    while (likely(pkts < budget)) {
        union ixgbe_adv_rx_desc *rxd;
        struct ixgbe_rx_buffer *bi;
        struct sk_buff *skb;
        int bias;
        unsigned int size;
        int xdp_res;

        /* Refill in small bursts to avoid starvation */
        if (cleaned >= IXGBE_RX_BUFFER_WRITE) {
            ixgbe_alloc_rx_buffers(rx_ring, cleaned);
            cleaned = 0;
        }

        /* look-ahead prefetch */
        {
            u16 la = rx_ring->next_to_clean + IXGBE_RX_PREFETCH_OFFSET;
            if (la >= rx_ring->count) {
                la -= rx_ring->count;
            }
            prefetch(IXGBE_RX_DESC(rx_ring, la));
            prefetch(&rx_ring->rx_buffer_info[la]);
        }

        rxd = IXGBE_RX_DESC(rx_ring, rx_ring->next_to_clean);
        size = le16_to_cpu(rxd->wb.upper.length);
        if (!size) {
            break;  /* no more completed descriptors */
        }

        dma_rmb();  /* descriptor is now coherent */

        bi = &rx_ring->rx_buffer_info[rx_ring->next_to_clean];
        prefetch(page_address(bi->page) + bi->page_offset);

        bi = ixgbe_get_rx_buffer(rx_ring, rxd, &skb, size, &bias);
        xdp_res = 0;

        if (!skb) {  /* prepare XDP view */
            void *hard_start = page_address(bi->page) + bi->page_offset - offset;
            xdp_prepare_buff(&xdp, hard_start, offset, size, true);
#if (PAGE_SIZE > 4096)
            xdp.frame_sz = ixgbe_rx_frame_truesize(rx_ring, size);
#endif
            xdp_buff_clear_frags_flag(&xdp);
            xdp_res = ixgbe_run_xdp(adapter, rx_ring, &xdp);
        }

        if (xdp_res) {
            if (xdp_res & (IXGBE_XDP_TX | IXGBE_XDP_REDIR)) {
                xdp_xmit |= xdp_res;
                ixgbe_rx_buffer_flip(rx_ring, bi, size);
            } else {
                bi->pagecnt_bias++;
            }
            pkts++;
            bytes += size;
        } else if (skb) {
            ixgbe_add_rx_frag(rx_ring, bi, skb, size);
        } else if (ring_uses_build_skb(rx_ring)) {
            skb = ixgbe_build_skb(rx_ring, bi, &xdp, rxd);
        } else {
            skb = ixgbe_construct_skb(rx_ring, bi, &xdp, rxd);
        }

        if (!xdp_res && !skb) {  /* allocation failure */
            rx_ring->rx_stats.alloc_rx_buff_failed++;
            bi->pagecnt_bias++;
            break;
        }

        ixgbe_put_rx_buffer(rx_ring, bi, skb, bias);
        cleaned++;

        if (unlikely(ixgbe_is_non_eop(rx_ring, rxd, skb))) {
            continue;
        }

        if (xdp_res || ixgbe_cleanup_headers(rx_ring, rxd, skb)) {
            continue;
        }

        bytes += skb->len;
        ixgbe_process_skb_fields(rx_ring, rxd, skb);
        ixgbe_rx_skb(q_vector, skb);

        pkts++;
    }

    if (xdp_xmit & IXGBE_XDP_REDIR) {
        xdp_do_flush();
    }
    if (xdp_xmit & IXGBE_XDP_TX) {
        ixgbe_xdp_ring_update_tail_locked(ixgbe_determine_xdp_ring(adapter));
    }
    smp_rmb();  /* Ensure XDP flush visibility on multi-core */

    if (cleaned) {
        ixgbe_alloc_rx_buffers(rx_ring, cleaned);
    }

    ixgbe_update_rx_ring_stats(rx_ring, q_vector, pkts, bytes);
    return pkts;
}

/**
 * ixgbe_configure_msix - Configure MSI-X hardware
 * @adapter: board private structure
 *
 * ixgbe_configure_msix sets up the hardware to properly generate MSI-X
 * interrupts.
 */
static void ixgbe_configure_msix(struct ixgbe_adapter *adapter) {
    struct ixgbe_q_vector *q_vector;
    int v_idx;
    u32 mask;

    /* Populate MSIX to EITR Select */
    if (adapter->num_vfs > 32) {
        u32 eitrsel = BIT(adapter->num_vfs - 32) - 1;
        IXGBE_WRITE_REG(&adapter->hw, IXGBE_EITRSEL, eitrsel);
    }

    /*
     * Populate the IVAR table and set the ITR values to the
     * corresponding register.
     */
    for (v_idx = 0; v_idx < adapter->num_q_vectors; v_idx++) {
        struct ixgbe_ring *ring;
        q_vector = adapter->q_vector[v_idx];

        ixgbe_for_each_ring(ring, q_vector->rx)
            ixgbe_set_ivar(adapter, 0, ring->reg_idx, v_idx);

        ixgbe_for_each_ring(ring, q_vector->tx)
            ixgbe_set_ivar(adapter, 1, ring->reg_idx, v_idx);

        ixgbe_write_eitr(q_vector);
    }

    switch (adapter->hw.mac.type) {
    case ixgbe_mac_82598EB:
        ixgbe_set_ivar(adapter, -1, IXGBE_IVAR_OTHER_CAUSES_INDEX, v_idx);
        break;
    case ixgbe_mac_82599EB:
    case ixgbe_mac_X540:
    case ixgbe_mac_X550:
    case ixgbe_mac_X550EM_x:
    case ixgbe_mac_x550em_a:
    case ixgbe_mac_e610:
        ixgbe_set_ivar(adapter, -1, 1, v_idx);
        break;
    default:
        break;
    }
    IXGBE_WRITE_REG(&adapter->hw, IXGBE_EITR(v_idx), 1950);

    /* set up to autoclear timer, and the vectors */
    mask = IXGBE_EIMS_ENABLE_MASK;
    mask &= ~(IXGBE_EIMS_OTHER |
              IXGBE_EIMS_MAILBOX |
              IXGBE_EIMS_LSC);

    if (adapter->hw.mac.type == ixgbe_mac_e610) {
        mask &= ~IXGBE_EIMS_FW_EVENT;
    }

    IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIAC, mask);
    IXGBE_WRITE_FLUSH(&adapter->hw);  /* Ensure register writes complete on x540 */
}

static __always_inline unsigned int ixgbe_itr_ll(unsigned int pkts, unsigned int bytes) {
    /* <= 64 pkts and mean size <= 256 B -> gaming / VoIP burst */
    return (pkts && pkts <= 64 && bytes <= (pkts << 8)) ? IXGBE_ITR_4US : 0;
}

/**
 * ixgbe_update_itr - update the dynamic ITR value based on statistics
 * @q_vector: structure containing interrupt and ring information
 * @ring_container: structure containing ring performance data
 *
 *      Stores a new ITR value based on packets and byte
 *      counts during the last interrupt.  The advantage of per interrupt
 *      computation is faster updates and more accurate ITR for the current
 *      traffic pattern.  Constants in this function were computed
 *      based on theoretical maximum wire speed and thresholds were set based
 *      on testing data as well as attempting to minimize response time
 *      while increasing bulk throughput.
 */
static void ixgbe_update_itr(struct ixgbe_q_vector *qv,
                              struct ixgbe_ring_container *rc) {
    unsigned int pkts = rc->total_packets;
    unsigned int bytes = rc->total_bytes;
    unsigned int mode = IXGBE_ITR_ADAPTIVE_LATENCY;
    unsigned int val = IXGBE_ITR_ADAPTIVE_MIN_USECS;
    unsigned long now = jiffies;

    prefetchw(rc);

    if (unlikely(!rc->ring)) {
        goto store;
    }

    if (time_after(now, rc->next_update)) {
        goto clear_store;
    }

    /* ---------- ultra-low-latency short-circuit --------------- */
    if ((val = ixgbe_itr_ll(pkts, bytes))) {
        goto store;
    }

    /* ---------- no packets ----------------------------------- */
    if (!pkts) {
        unsigned int cur = IXGBE_REG_TO_ITR(qv->itr);

        val = min(cur + IXGBE_ITR_ADAPTIVE_MIN_INC, IXGBE_ITR_ADAPTIVE_MAX_USECS);
        mode = rc->itr & IXGBE_ITR_ADAPTIVE_LATENCY;
        goto store;
    }

    /* ---------- classify by packet count --------------------- */
    if (pkts >= 256) {  /* heavy bulk */
        mode = IXGBE_ITR_ADAPTIVE_BULK;
        goto by_size;
    }

    {
        unsigned int cur = IXGBE_REG_TO_ITR(qv->itr);

        if (pkts >= 96) {
            val = max(cur >> 1, IXGBE_ITR_ADAPTIVE_MIN_USECS);
            goto store;
        }
        if (pkts >= 48) {
            val = max(cur, IXGBE_ITR_ADAPTIVE_MIN_USECS);
            goto store;
        }
        if (pkts >= 4) {
            val = min(cur + IXGBE_ITR_ADAPTIVE_MIN_INC, IXGBE_ITR_ADAPTIVE_MAX_USECS);
            goto store;
        }
    }

    /* ---------- 1-3 packets: check size ---------------------- */
    if (bytes < 9000) {
        goto by_size;  /* small – latency */
    }
    val = min(IXGBE_REG_TO_ITR(qv->itr) + IXGBE_ITR_ADAPTIVE_MIN_INC,
              IXGBE_ITR_ADAPTIVE_MAX_USECS);
    goto store;

    /* ---------- average-size driven branch ------------------------- */
by_size: {
        unsigned int avg = reciprocal_divide(bytes, reciprocal_value(pkts));

        if (avg <= 60) {
            val = 5120;
        } else if (avg <= 316) {
            val = avg * 40 + 2720;
        } else if (avg <= 1084) {
            val = avg * 15 + 11452;
        } else if (avg < 1968) {
            val = avg * 5 + 22420;
        } else {
            val = 32256;
        }

        if (mode & IXGBE_ITR_ADAPTIVE_LATENCY) {
            val >>= 1;
        }

        if (qv->adapter->link_speed >= IXGBE_LINK_SPEED_10GB_FULL) {
            val = DIV_ROUND_UP(val, IXGBE_ITR_ADAPTIVE_MIN_INC * 256) *
                  IXGBE_ITR_ADAPTIVE_MIN_INC;
        } else {
            val = min(val, 8064u);
            val = DIV_ROUND_UP(val, IXGBE_ITR_ADAPTIVE_MIN_INC * 64) *
                  IXGBE_ITR_ADAPTIVE_MIN_INC;
        }
        val = max(val, IXGBE_ITR_ADAPTIVE_MIN_USECS);  /* Clamp to hardware min */
    }

    /* ---------- commit result and reset counters ------------------ */
store:
    rc->itr = val | mode;

clear_store:
    rc->next_update = now + 1;
    rc->total_packets = 0;
    rc->total_bytes = 0;
}

/**
 * ixgbe_write_eitr - write EITR register in hardware specific way
 * @q_vector: structure containing interrupt and ring information
 *
 * This function is made to be called by ethtool and by the driver
 * when it needs to update EITR registers at runtime.  Hardware
 * specific quirks/differences are taken care of here.
 */
void ixgbe_write_eitr(struct ixgbe_q_vector *qv) {
    struct ixgbe_adapter *adap = qv->adapter;
    struct ixgbe_hw *hw = &adap->hw;
    u32 idx = qv->v_idx;
    u32 reg = qv->itr & IXGBE_MAX_EITR;

    switch (adap->hw.mac.type) {
    case ixgbe_mac_82598EB:
        reg |= reg << 16;
        break;
    case ixgbe_mac_82599EB:
    case ixgbe_mac_X540:
    case ixgbe_mac_X550:
    case ixgbe_mac_X550EM_x:
    case ixgbe_mac_x550em_a:
    case ixgbe_mac_e610:
        reg |= IXGBE_EITR_CNT_WDIS;  /* Prevent glitch on x540+ */
        break;
    default:
        break;
    }
    writel_relaxed(reg, hw->hw_addr + IXGBE_EITR(idx));
}

static void ixgbe_set_itr(struct ixgbe_q_vector *qv) {
    ixgbe_update_itr(qv, &qv->tx);
    ixgbe_update_itr(qv, &qv->rx);

    u32 rx_num = qv->rx.itr & ~IXGBE_ITR_ADAPTIVE_LATENCY;
    u32 tx_num = qv->tx.itr & ~IXGBE_ITR_ADAPTIVE_LATENCY;
    u32 flag = (rx_num <= tx_num) ? (qv->rx.itr & IXGBE_ITR_ADAPTIVE_LATENCY) :
               (qv->tx.itr & IXGBE_ITR_ADAPTIVE_LATENCY);
    u32 numeric = min(rx_num, tx_num);
    u32 hw_val = IXGBE_ITR_TO_REG(numeric) | flag;

    if (unlikely(hw_val == qv->itr)) {
        return;
    }

    qv->itr = hw_val;
    ixgbe_write_eitr(qv);
}

/**
 * ixgbe_check_overtemp_subtask - check for over temperature
 * @adapter: pointer to adapter
 **/
static void ixgbe_check_overtemp_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 eicr            = adapter->interrupt_event;

	if (test_bit(__IXGBE_DOWN, &adapter->state))
		return;
	if (!(adapter->flags2 & IXGBE_FLAG2_TEMP_SENSOR_EVENT))
		return;
	adapter->flags2 &= ~IXGBE_FLAG2_TEMP_SENSOR_EVENT;

	switch (hw->device_id) {
		case IXGBE_DEV_ID_82599_T3_LOM:
			if (!(eicr & (IXGBE_EICR_GPI_SDP0_8259X | IXGBE_EICR_LSC)))
				return;
		if (!(eicr & IXGBE_EICR_LSC) && hw->mac.ops.check_link) {
			u32 sp; bool up = false;
			hw->mac.ops.check_link(hw, &sp, &up, false);
			if (up)
				return;
		}
		break;
		case IXGBE_DEV_ID_X550EM_A_1G_T:
		case IXGBE_DEV_ID_X550EM_A_1G_T_L:
			break;
		default:
			if (hw->mac.type >= ixgbe_mac_X540)
				return;
		if (!(eicr & IXGBE_EICR_GPI_SDP0(hw)))
			return;
		break;
	}

	if (!hw->phy.ops.check_overtemp || !hw->phy.ops.check_overtemp(hw))
		return;

	e_crit(drv, "%s\n", ixgbe_overheat_msg);
	adapter->interrupt_event = 0;
}

static void ixgbe_check_fan_failure(struct ixgbe_adapter *adapter, u32 eicr)
{
	struct ixgbe_hw *hw = &adapter->hw;

	if ((adapter->flags & IXGBE_FLAG_FAN_FAIL_CAPABLE) &&
	    (eicr & IXGBE_EICR_GPI_SDP1(hw))) {
		e_crit(probe, "Fan has stopped, replace the adapter\n");
		/* write to clear the interrupt */
		IXGBE_WRITE_REG(hw, IXGBE_EICR, IXGBE_EICR_GPI_SDP1(hw));
	}
}

static void ixgbe_check_overtemp_event(struct ixgbe_adapter *adapter, u32 eicr)
{
	struct ixgbe_hw *hw = &adapter->hw;

	if (!(adapter->flags2 & IXGBE_FLAG2_TEMP_SENSOR_CAPABLE))
		return;

	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82599EB:
		/*
		 * Need to check link state so complete overtemp check
		 * on service task
		 */
		if (((eicr & IXGBE_EICR_GPI_SDP0(hw)) ||
		     (eicr & IXGBE_EICR_LSC)) &&
		    (!test_bit(__IXGBE_DOWN, &adapter->state))) {
			adapter->interrupt_event = eicr;
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_EVENT;
			ixgbe_service_event_schedule(adapter);
			return;
		}
		return;
	case ixgbe_mac_x550em_a:
		if (eicr & IXGBE_EICR_GPI_SDP0_X550EM_a) {
			adapter->interrupt_event = eicr;
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_EVENT;
			ixgbe_service_event_schedule(adapter);
			IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC,
					IXGBE_EICR_GPI_SDP0_X550EM_a);
			IXGBE_WRITE_REG(&adapter->hw, IXGBE_EICR,
					IXGBE_EICR_GPI_SDP0_X550EM_a);
		}
		return;
	case ixgbe_mac_X550:
	case ixgbe_mac_X540:
		if (!(eicr & IXGBE_EICR_TS))
			return;
		break;
	default:
		return;
	}

	e_crit(drv, "%s\n", ixgbe_overheat_msg);
}

static inline bool ixgbe_is_sfp(struct ixgbe_hw *hw)
{
	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		if (hw->phy.type == ixgbe_phy_nl)
			return true;
		return false;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
		switch (hw->mac.ops.get_media_type(hw)) {
		case ixgbe_media_type_fiber:
		case ixgbe_media_type_fiber_qsfp:
			return true;
		default:
			return false;
		}
	default:
		return false;
	}
}

static void ixgbe_check_sfp_event(struct ixgbe_adapter *adapter, u32 eicr)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 eicr_mask = IXGBE_EICR_GPI_SDP2(hw);

	if (!ixgbe_is_sfp(hw))
		return;

	/* Later MAC's use different SDP */
	if (hw->mac.type >= ixgbe_mac_X540)
		eicr_mask = IXGBE_EICR_GPI_SDP0_X540;

	if (eicr & eicr_mask) {
		/* Clear the interrupt */
		IXGBE_WRITE_REG(hw, IXGBE_EICR, eicr_mask);
		if (!test_bit(__IXGBE_DOWN, &adapter->state)) {
			adapter->flags2 |= IXGBE_FLAG2_SFP_NEEDS_RESET;
			adapter->sfp_poll_time = 0;
			ixgbe_service_event_schedule(adapter);
		}
	}

	if (adapter->hw.mac.type == ixgbe_mac_82599EB &&
	    (eicr & IXGBE_EICR_GPI_SDP1(hw))) {
		/* Clear the interrupt */
		IXGBE_WRITE_REG(hw, IXGBE_EICR, IXGBE_EICR_GPI_SDP1(hw));
		if (!test_bit(__IXGBE_DOWN, &adapter->state)) {
			adapter->flags |= IXGBE_FLAG_NEED_LINK_CONFIG;
			ixgbe_service_event_schedule(adapter);
		}
	}
}

static void ixgbe_check_lsc(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;

	adapter->lsc_int++;
	adapter->flags |= IXGBE_FLAG_NEED_LINK_UPDATE;
	adapter->link_check_timeout = jiffies;
	if (!test_bit(__IXGBE_DOWN, &adapter->state)) {
		IXGBE_WRITE_REG(hw, IXGBE_EIMC, IXGBE_EIMC_LSC);
		IXGBE_WRITE_FLUSH(hw);
		ixgbe_service_event_schedule(adapter);
	}
}

/**
 * ixgbe_check_phy_fw_load - check if PHY FW load failed
 * @adapter: pointer to adapter structure
 * @link_cfg_err: bitmap from the link info structure
 *
 * Check if external PHY FW load failed and print an error message if it did.
 */
static void ixgbe_check_phy_fw_load(struct ixgbe_adapter *adapter,
				    u8 link_cfg_err)
{
	if (!(link_cfg_err & IXGBE_ACI_LINK_EXTERNAL_PHY_LOAD_FAILURE)) {
		adapter->flags2 &= ~IXGBE_FLAG2_PHY_FW_LOAD_FAILED;
		return;
	}

	if (adapter->flags2 & IXGBE_FLAG2_PHY_FW_LOAD_FAILED)
		return;

	if (link_cfg_err & IXGBE_ACI_LINK_EXTERNAL_PHY_LOAD_FAILURE) {
		netdev_err(adapter->netdev, "Device failed to load the FW for the external PHY. Please download and install the latest NVM for your device and try again\n");
		adapter->flags2 |= IXGBE_FLAG2_PHY_FW_LOAD_FAILED;
	}
}

/**
 * ixgbe_check_module_power - check module power level
 * @adapter: pointer to adapter structure
 * @link_cfg_err: bitmap from the link info structure
 *
 * Check module power level returned by a previous call to aci_get_link_info
 * and print error messages if module power level is not supported.
 */
static void ixgbe_check_module_power(struct ixgbe_adapter *adapter,
				     u8 link_cfg_err)
{
	/* If module power level is supported, clear the flag. */
	if (!(link_cfg_err & (IXGBE_ACI_LINK_INVAL_MAX_POWER_LIMIT |
			      IXGBE_ACI_LINK_MODULE_POWER_UNSUPPORTED))) {
		adapter->flags2 &= ~IXGBE_FLAG2_MOD_POWER_UNSUPPORTED;
		return;
	}

	/* If IXGBE_FLAG2_MOD_POWER_UNSUPPORTED was previously set and the
	 * above block didn't clear this bit, there's nothing to do.
	 */
	if (adapter->flags2 & IXGBE_FLAG2_MOD_POWER_UNSUPPORTED)
		return;

	if (link_cfg_err & IXGBE_ACI_LINK_INVAL_MAX_POWER_LIMIT) {
		netdev_err(adapter->netdev, "The installed module is incompatible with the device's NVM image. Cannot start link.\n");
		adapter->flags2 |= IXGBE_FLAG2_MOD_POWER_UNSUPPORTED;
	} else if (link_cfg_err & IXGBE_ACI_LINK_MODULE_POWER_UNSUPPORTED) {
		netdev_err(adapter->netdev, "The module's power requirements exceed the device's power supply. Cannot start link.\n");
		adapter->flags2 |= IXGBE_FLAG2_MOD_POWER_UNSUPPORTED;
	}
}

/**
 * ixgbe_check_link_cfg_err - check if link configuration failed
 * @adapter: pointer to adapter structure
 * @link_cfg_err: bitmap from the link info structure
 *
 * Print if any link configuration failure happens due to the value in the
 * link_cfg_err parameter in the link info structure.
 */
static void ixgbe_check_link_cfg_err(struct ixgbe_adapter *adapter,
				     u8 link_cfg_err)
{
	ixgbe_check_module_power(adapter, link_cfg_err);
	ixgbe_check_phy_fw_load(adapter, link_cfg_err);
}

/**
 * ixgbe_process_link_status_event - process the link event
 * @adapter: pointer to adapter structure
 * @link_up: true if the physical link is up and false if it is down
 * @link_speed: current link speed received from the link event
 *
 * Return: 0 on success or negative value on failure.
 */
static int
ixgbe_process_link_status_event(struct ixgbe_adapter *adapter, bool link_up,
				u16 link_speed)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int status;

	/* Update the link info structures and re-enable link events,
	 * don't bail on failure due to other book keeping needed.
	 */
	status = ixgbe_update_link_info(hw);
	if (status)
		e_dev_err("Failed to update link status, err %d aq_err %d\n",
			  status, hw->aci.last_status);

	ixgbe_check_link_cfg_err(adapter, hw->link.link_info.link_cfg_err);

	/* Check if the link state is up after updating link info, and treat
	 * this event as an UP event since the link is actually UP now.
	 */
	if (hw->link.link_info.link_info & IXGBE_ACI_LINK_UP)
		link_up = true;

	/* Turn off PHY if media was removed. */
	if (!(adapter->flags2 & IXGBE_FLAG2_NO_MEDIA) &&
	    !(hw->link.link_info.link_info & IXGBE_ACI_MEDIA_AVAILABLE))
		adapter->flags2 |= IXGBE_FLAG2_NO_MEDIA;

	if (link_up == adapter->link_up &&
	    link_up == netif_carrier_ok(adapter->netdev) &&
	    link_speed == adapter->link_speed)
		return 0;

	adapter->flags |= IXGBE_FLAG_NEED_LINK_UPDATE;
	adapter->link_check_timeout = jiffies;
	ixgbe_watchdog_update_link(adapter);

	if (link_up)
		ixgbe_watchdog_link_is_up(adapter);
	else
		ixgbe_watchdog_link_is_down(adapter);

	return 0;
}

/**
 * ixgbe_handle_link_status_event - handle link status event via ACI
 * @adapter: pointer to adapter structure
 * @e: event structure containing link status info
 */
static void
ixgbe_handle_link_status_event(struct ixgbe_adapter *adapter,
			       struct ixgbe_aci_event *e)
{
	struct ixgbe_aci_cmd_get_link_status_data *link_data;
	u16 link_speed;
	bool link_up;

	link_data = (struct ixgbe_aci_cmd_get_link_status_data *)e->msg_buf;

	link_up = !!(link_data->link_info & IXGBE_ACI_LINK_UP);
	link_speed = le16_to_cpu(link_data->link_speed);

	if (ixgbe_process_link_status_event(adapter, link_up, link_speed))
		e_dev_warn("Could not process link status event");
}

/**
 * ixgbe_schedule_fw_event - schedule Firmware event
 * @adapter: pointer to the adapter structure
 *
 * If the adapter is not in down, removing or resetting state,
 * an event is scheduled.
 */
static void ixgbe_schedule_fw_event(struct ixgbe_adapter *adapter)
{
	if (!test_bit(__IXGBE_DOWN, &adapter->state) &&
	    !test_bit(__IXGBE_REMOVING, &adapter->state) &&
	    !test_bit(__IXGBE_RESETTING, &adapter->state)) {
		adapter->flags2 |= IXGBE_FLAG2_FW_ASYNC_EVENT;
		ixgbe_service_event_schedule(adapter);
	}
}

/**
 * ixgbe_aci_event_cleanup - release msg_buf memory
 * @event: pointer to the event holding msg_buf to be released
 *
 * Clean memory allocated for event's msg_buf. Implements auto memory cleanup.
 */
static void ixgbe_aci_event_cleanup(struct ixgbe_aci_event *event)
{
	kfree(event->msg_buf);
}

/**
 * ixgbe_handle_fw_event - handle Firmware event
 * @adapter: pointer to the adapter structure
 *
 * Obtain an event from the ACI and then and then process it according to the
 * type of the event and the opcode.
 */
static void ixgbe_handle_fw_event(struct ixgbe_adapter *adapter)
{
	struct ixgbe_aci_event event __cleanup(ixgbe_aci_event_cleanup);
	struct ixgbe_hw *hw = &adapter->hw;
	bool pending = false;
	int err;

	if (adapter->flags2 & IXGBE_FLAG2_FW_ASYNC_EVENT)
		adapter->flags2 &= ~IXGBE_FLAG2_FW_ASYNC_EVENT;
	event.buf_len = IXGBE_ACI_MAX_BUFFER_SIZE;
	event.msg_buf = kzalloc(event.buf_len, GFP_KERNEL);
	if (!event.msg_buf)
		return;

	do {
		err = ixgbe_aci_get_event(hw, &event, &pending);
		if (err)
			break;

		switch (le16_to_cpu(event.desc.opcode)) {
		case ixgbe_aci_opc_get_link_status:
			ixgbe_handle_link_status_event(adapter, &event);
			break;
		case ixgbe_aci_opc_temp_tca_event:
			e_crit(drv, "%s\n", ixgbe_overheat_msg);
			ixgbe_down(adapter);
			break;
		default:
			e_warn(hw, "unknown FW async event captured\n");
			break;
		}
	} while (pending);
}

static inline void ixgbe_irq_enable_queues(struct ixgbe_adapter *adapter,
					   u64 qmask)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 mask;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		mask = (IXGBE_EIMS_RTX_QUEUE & qmask);
		IXGBE_WRITE_REG(hw, IXGBE_EIMS, mask);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		mask = (qmask & 0xFFFFFFFF);
		if (mask)
			IXGBE_WRITE_REG(hw, IXGBE_EIMS_EX(0), mask);
		mask = (qmask >> 32);
		if (mask)
			IXGBE_WRITE_REG(hw, IXGBE_EIMS_EX(1), mask);
		break;
	default:
		break;
	}
	/* skip the flush */
}

/**
 * ixgbe_irq_enable - Enable default interrupt generation settings
 * @adapter: board private structure
 * @queues: enable irqs for queues
 * @flush: flush register write
 **/
static inline void ixgbe_irq_enable(struct ixgbe_adapter *adapter, bool queues,
				    bool flush)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 mask = (IXGBE_EIMS_ENABLE_MASK & ~IXGBE_EIMS_RTX_QUEUE);

	/* don't reenable LSC while waiting for link */
	if (adapter->flags & IXGBE_FLAG_NEED_LINK_UPDATE)
		mask &= ~IXGBE_EIMS_LSC;

	if (adapter->flags2 & IXGBE_FLAG2_TEMP_SENSOR_CAPABLE)
		switch (adapter->hw.mac.type) {
		case ixgbe_mac_82599EB:
			mask |= IXGBE_EIMS_GPI_SDP0(hw);
			break;
		case ixgbe_mac_X540:
		case ixgbe_mac_X550:
		case ixgbe_mac_X550EM_x:
		case ixgbe_mac_x550em_a:
			mask |= IXGBE_EIMS_TS;
			break;
		default:
			break;
		}
	if (adapter->flags & IXGBE_FLAG_FAN_FAIL_CAPABLE)
		mask |= IXGBE_EIMS_GPI_SDP1(hw);
	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82599EB:
		mask |= IXGBE_EIMS_GPI_SDP1(hw);
		mask |= IXGBE_EIMS_GPI_SDP2(hw);
		fallthrough;
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_e610:
		mask |= IXGBE_EIMS_FW_EVENT;
		fallthrough;
	case ixgbe_mac_x550em_a:
		if (adapter->hw.device_id == IXGBE_DEV_ID_X550EM_X_SFP ||
		    adapter->hw.device_id == IXGBE_DEV_ID_X550EM_A_SFP ||
		    adapter->hw.device_id == IXGBE_DEV_ID_X550EM_A_SFP_N)
			mask |= IXGBE_EIMS_GPI_SDP0(&adapter->hw);
		if (adapter->hw.phy.type == ixgbe_phy_x550em_ext_t)
			mask |= IXGBE_EICR_GPI_SDP0_X540;
		mask |= IXGBE_EIMS_ECC;
		mask |= IXGBE_EIMS_MAILBOX;
		break;
	default:
		break;
	}

	if ((adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE) &&
	    !(adapter->flags2 & IXGBE_FLAG2_FDIR_REQUIRES_REINIT))
		mask |= IXGBE_EIMS_FLOW_DIR;

	IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMS, mask);
	if (queues)
		ixgbe_irq_enable_queues(adapter, ~0);
	if (flush)
		IXGBE_WRITE_FLUSH(&adapter->hw);
}

static irqreturn_t ixgbe_msix_other(int irq, void *data)
{
	struct ixgbe_adapter *adapter = data;
	struct ixgbe_hw *hw = &adapter->hw;
	u32 eicr;

	/*
	 * Workaround for Silicon errata.  Use clear-by-write instead
	 * of clear-by-read.  Reading with EICS will return the
	 * interrupt causes without clearing, which later be done
	 * with the write to EICR.
	 */
	eicr = IXGBE_READ_REG(hw, IXGBE_EICS);

	/* The lower 16bits of the EICR register are for the queue interrupts
	 * which should be masked here in order to not accidentally clear them if
	 * the bits are high when ixgbe_msix_other is called. There is a race
	 * condition otherwise which results in possible performance loss
	 * especially if the ixgbe_msix_other interrupt is triggering
	 * consistently (as it would when PPS is turned on for the X540 device)
	 */
	eicr &= 0xFFFF0000;

	IXGBE_WRITE_REG(hw, IXGBE_EICR, eicr);

	if (eicr & IXGBE_EICR_LSC)
		ixgbe_check_lsc(adapter);

	if (eicr & IXGBE_EICR_MAILBOX)
		ixgbe_msg_task(adapter);

	if (eicr & IXGBE_EICR_FW_EVENT)
		ixgbe_schedule_fw_event(adapter);

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		if (hw->phy.type == ixgbe_phy_x550em_ext_t &&
		    (eicr & IXGBE_EICR_GPI_SDP0_X540)) {
			adapter->flags2 |= IXGBE_FLAG2_PHY_INTERRUPT;
			ixgbe_service_event_schedule(adapter);
			IXGBE_WRITE_REG(hw, IXGBE_EICR,
					IXGBE_EICR_GPI_SDP0_X540);
		}
		if (eicr & IXGBE_EICR_ECC) {
			e_info(link, "Received ECC Err, initiating reset\n");
			set_bit(__IXGBE_RESET_REQUESTED, &adapter->state);
			ixgbe_service_event_schedule(adapter);
			IXGBE_WRITE_REG(hw, IXGBE_EICR, IXGBE_EICR_ECC);
		}
		/* Handle Flow Director Full threshold interrupt */
		if (eicr & IXGBE_EICR_FLOW_DIR) {
			int reinit_count = 0;
			int i;
			for (i = 0; i < adapter->num_tx_queues; i++) {
				struct ixgbe_ring *ring = adapter->tx_ring[i];
				if (test_and_clear_bit(__IXGBE_TX_FDIR_INIT_DONE,
						       &ring->state))
					reinit_count++;
			}
			if (reinit_count) {
				/* no more flow director interrupts until after init */
				IXGBE_WRITE_REG(hw, IXGBE_EIMC, IXGBE_EIMC_FLOW_DIR);
				adapter->flags2 |= IXGBE_FLAG2_FDIR_REQUIRES_REINIT;
				ixgbe_service_event_schedule(adapter);
			}
		}
		ixgbe_check_sfp_event(adapter, eicr);
		ixgbe_check_overtemp_event(adapter, eicr);
		break;
	default:
		break;
	}

	ixgbe_check_fan_failure(adapter, eicr);

	if (unlikely(eicr & IXGBE_EICR_TIMESYNC))
		ixgbe_ptp_check_pps_event(adapter);

	/* re-enable the original interrupt state, no lsc, no queues */
	if (!test_bit(__IXGBE_DOWN, &adapter->state))
		ixgbe_irq_enable(adapter, false, false);

	return IRQ_HANDLED;
}

static irqreturn_t ixgbe_msix_clean_rings(int irq, void *data)
{
	struct ixgbe_q_vector *q_vector = data;

	/* EIAM disabled interrupts (on this vector) for us */

	if (q_vector->rx.ring || q_vector->tx.ring)
		napi_schedule_irqoff(&q_vector->napi);

	return IRQ_HANDLED;
}

/**
 * ixgbe_poll - NAPI Rx polling callback
 * @napi: structure for representing this polling device
 * @budget: how many packets driver is allowed to clean
 *
 * This function is used for legacy and MSI, NAPI mode
 **/
int ixgbe_poll(struct napi_struct *napi, int budget)
{
	struct ixgbe_q_vector *q_vector =
				container_of(napi, struct ixgbe_q_vector, napi);
	struct ixgbe_adapter *adapter = q_vector->adapter;
	struct ixgbe_ring *ring;
	int per_ring_budget, work_done = 0;
	bool clean_complete = true;

#ifdef CONFIG_IXGBE_DCA
	if (adapter->flags & IXGBE_FLAG_DCA_ENABLED)
		ixgbe_update_dca(q_vector);
#endif

	ixgbe_for_each_ring(ring, q_vector->tx) {
		bool wd = ring->xsk_pool ?
			  ixgbe_clean_xdp_tx_irq(q_vector, ring, budget) :
			  ixgbe_clean_tx_irq(q_vector, ring, budget);

		if (!wd)
			clean_complete = false;
	}

	/* Exit if we are called by netpoll */
	if (budget <= 0)
		return budget;

	/* attempt to distribute budget to each queue fairly, but don't allow
	 * the budget to go below 1 because we'll exit polling */
	if (q_vector->rx.count > 1)
		per_ring_budget = max(budget/q_vector->rx.count, 1);
	else
		per_ring_budget = budget;

	ixgbe_for_each_ring(ring, q_vector->rx) {
		int cleaned = ring->xsk_pool ?
			      ixgbe_clean_rx_irq_zc(q_vector, ring,
						    per_ring_budget) :
			      ixgbe_clean_rx_irq(q_vector, ring,
						 per_ring_budget);

		work_done += cleaned;
		if (cleaned >= per_ring_budget)
			clean_complete = false;
	}

	/* If all work not completed, return budget and keep polling */
	if (!clean_complete)
		return budget;

	/* all work done, exit the polling mode */
	if (likely(napi_complete_done(napi, work_done))) {
		if (adapter->rx_itr_setting & 1)
			ixgbe_set_itr(q_vector);
		if (!test_bit(__IXGBE_DOWN, &adapter->state))
			ixgbe_irq_enable_queues(adapter,
						BIT_ULL(q_vector->v_idx));
	}

	return min(work_done, budget - 1);
}

/**
 * ixgbe_request_msix_irqs - Initialize MSI-X interrupts
 * @adapter: board private structure
 *
 * ixgbe_request_msix_irqs allocates MSI-X vectors and requests
 * interrupts from the kernel.
 **/
static int ixgbe_request_msix_irqs(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	unsigned int ri = 0, ti = 0;
	int vector, err;

	for (vector = 0; vector < adapter->num_q_vectors; vector++) {
		struct ixgbe_q_vector *q_vector = adapter->q_vector[vector];
		struct msix_entry *entry = &adapter->msix_entries[vector];

		if (q_vector->tx.ring && q_vector->rx.ring) {
			snprintf(q_vector->name, sizeof(q_vector->name),
				 "%s-TxRx-%u", netdev->name, ri++);
			ti++;
		} else if (q_vector->rx.ring) {
			snprintf(q_vector->name, sizeof(q_vector->name),
				 "%s-rx-%u", netdev->name, ri++);
		} else if (q_vector->tx.ring) {
			snprintf(q_vector->name, sizeof(q_vector->name),
				 "%s-tx-%u", netdev->name, ti++);
		} else {
			/* skip this unused q_vector */
			continue;
		}
		err = request_irq(entry->vector, &ixgbe_msix_clean_rings, 0,
				  q_vector->name, q_vector);
		if (err) {
			e_err(probe, "request_irq failed for MSIX interrupt "
			      "Error: %d\n", err);
			goto free_queue_irqs;
		}
		/* If Flow Director is enabled, set interrupt affinity */
		if (adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE) {
			/* assign the mask for this irq */
			irq_update_affinity_hint(entry->vector,
						 &q_vector->affinity_mask);
		}
	}

	err = request_irq(adapter->msix_entries[vector].vector,
			  ixgbe_msix_other, 0, netdev->name, adapter);
	if (err) {
		e_err(probe, "request_irq for msix_other failed: %d\n", err);
		goto free_queue_irqs;
	}

	return 0;

free_queue_irqs:
	while (vector) {
		vector--;
		irq_update_affinity_hint(adapter->msix_entries[vector].vector,
					 NULL);
		free_irq(adapter->msix_entries[vector].vector,
			 adapter->q_vector[vector]);
	}
	adapter->flags &= ~IXGBE_FLAG_MSIX_ENABLED;
	pci_disable_msix(adapter->pdev);
	kfree(adapter->msix_entries);
	adapter->msix_entries = NULL;
	return err;
}

/**
 * ixgbe_intr - legacy mode Interrupt Handler
 * @irq: interrupt number
 * @data: pointer to a network interface device structure
 **/
static irqreturn_t ixgbe_intr(int irq, void *data)
{
	struct ixgbe_adapter *adapter = data;
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_q_vector *q_vector = adapter->q_vector[0];
	u32 eicr;

	/*
	 * Workaround for silicon errata #26 on 82598.  Mask the interrupt
	 * before the read of EICR.
	 */
	IXGBE_WRITE_REG(hw, IXGBE_EIMC, IXGBE_IRQ_CLEAR_MASK);

	/* for NAPI, using EIAM to auto-mask tx/rx interrupt bits on read
	 * therefore no explicit interrupt disable is necessary */
	eicr = IXGBE_READ_REG(hw, IXGBE_EICR);
	if (!eicr) {
		/*
		 * shared interrupt alert!
		 * make sure interrupts are enabled because the read will
		 * have disabled interrupts due to EIAM
		 * finish the workaround of silicon errata on 82598.  Unmask
		 * the interrupt that we masked before the EICR read.
		 */
		if (!test_bit(__IXGBE_DOWN, &adapter->state))
			ixgbe_irq_enable(adapter, true, true);
		return IRQ_NONE;	/* Not our interrupt */
	}

	if (eicr & IXGBE_EICR_LSC)
		ixgbe_check_lsc(adapter);

	if (eicr & IXGBE_EICR_FW_EVENT)
		ixgbe_schedule_fw_event(adapter);

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
		ixgbe_check_sfp_event(adapter, eicr);
		fallthrough;
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		if (eicr & IXGBE_EICR_ECC) {
			e_info(link, "Received ECC Err, initiating reset\n");
			set_bit(__IXGBE_RESET_REQUESTED, &adapter->state);
			ixgbe_service_event_schedule(adapter);
			IXGBE_WRITE_REG(hw, IXGBE_EICR, IXGBE_EICR_ECC);
		}
		ixgbe_check_overtemp_event(adapter, eicr);
		break;
	default:
		break;
	}

	ixgbe_check_fan_failure(adapter, eicr);
	if (unlikely(eicr & IXGBE_EICR_TIMESYNC))
		ixgbe_ptp_check_pps_event(adapter);

	/* would disable interrupts here but EIAM disabled it */
	napi_schedule_irqoff(&q_vector->napi);

	/*
	 * re-enable link(maybe) and non-queue interrupts, no flush.
	 * ixgbe_poll will re-enable the queue interrupts
	 */
	if (!test_bit(__IXGBE_DOWN, &adapter->state))
		ixgbe_irq_enable(adapter, false, false);

	return IRQ_HANDLED;
}

/**
 * ixgbe_request_irq - initialize interrupts
 * @adapter: board private structure
 *
 * Attempts to configure interrupts using the best available
 * capabilities of the hardware and kernel.
 **/
static int ixgbe_request_irq(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	int err;

	if (adapter->flags & IXGBE_FLAG_MSIX_ENABLED)
		err = ixgbe_request_msix_irqs(adapter);
	else if (adapter->flags & IXGBE_FLAG_MSI_ENABLED)
		err = request_irq(adapter->pdev->irq, ixgbe_intr, 0,
				  netdev->name, adapter);
	else
		err = request_irq(adapter->pdev->irq, ixgbe_intr, IRQF_SHARED,
				  netdev->name, adapter);

	if (err)
		e_err(probe, "request_irq failed, Error %d\n", err);

	return err;
}

static void ixgbe_free_irq(struct ixgbe_adapter *adapter)
{
	int vector;

	if (!(adapter->flags & IXGBE_FLAG_MSIX_ENABLED)) {
		free_irq(adapter->pdev->irq, adapter);
		return;
	}

	if (!adapter->msix_entries)
		return;

	for (vector = 0; vector < adapter->num_q_vectors; vector++) {
		struct ixgbe_q_vector *q_vector = adapter->q_vector[vector];
		struct msix_entry *entry = &adapter->msix_entries[vector];

		/* free only the irqs that were actually requested */
		if (!q_vector->rx.ring && !q_vector->tx.ring)
			continue;

		/* clear the affinity_mask in the IRQ descriptor */
		irq_update_affinity_hint(entry->vector, NULL);

		free_irq(entry->vector, q_vector);
	}

	free_irq(adapter->msix_entries[vector].vector, adapter);
}

/**
 * ixgbe_irq_disable - Mask off interrupt generation on the NIC
 * @adapter: board private structure
 **/
static inline void ixgbe_irq_disable(struct ixgbe_adapter *adapter)
{
	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82598EB:
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC, ~0);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC, 0xFFFF0000);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC_EX(0), ~0);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC_EX(1), ~0);
		break;
	default:
		break;
	}
	IXGBE_WRITE_FLUSH(&adapter->hw);
	if (adapter->flags & IXGBE_FLAG_MSIX_ENABLED) {
		int vector;

		for (vector = 0; vector < adapter->num_q_vectors; vector++)
			synchronize_irq(adapter->msix_entries[vector].vector);

		synchronize_irq(adapter->msix_entries[vector++].vector);
	} else {
		synchronize_irq(adapter->pdev->irq);
	}
}

/**
 * ixgbe_configure_msi_and_legacy - Initialize PIN (INTA...) and MSI interrupts
 * @adapter: board private structure
 *
 **/
static void ixgbe_configure_msi_and_legacy(struct ixgbe_adapter *adapter)
{
	struct ixgbe_q_vector *q_vector = adapter->q_vector[0];

	ixgbe_write_eitr(q_vector);

	ixgbe_set_ivar(adapter, 0, 0, 0);
	ixgbe_set_ivar(adapter, 1, 0, 0);

	e_info(hw, "Legacy interrupt IVAR setup done\n");
}

/**
 * ixgbe_configure_tx_ring - Configure 8259x Tx ring after Reset
 * @adapter: board private structure
 * @ring: structure containing ring specific data
 *
 * Configure the Tx descriptor ring after a reset.
 **/
void ixgbe_configure_tx_ring(struct ixgbe_adapter *adapter,
			     struct ixgbe_ring *ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u64 tdba = ring->dma;
	int wait_loop = 10;
	u32 txdctl = IXGBE_TXDCTL_ENABLE;
	u8 reg_idx = ring->reg_idx;

	ring->xsk_pool = NULL;
	if (ring_is_xdp(ring))
		ring->xsk_pool = ixgbe_xsk_pool(adapter, ring);

	/* disable queue to avoid issues while updating state */
	IXGBE_WRITE_REG(hw, IXGBE_TXDCTL(reg_idx), 0);
	IXGBE_WRITE_FLUSH(hw);

	IXGBE_WRITE_REG(hw, IXGBE_TDBAL(reg_idx),
			(tdba & DMA_BIT_MASK(32)));
	IXGBE_WRITE_REG(hw, IXGBE_TDBAH(reg_idx), (tdba >> 32));
	IXGBE_WRITE_REG(hw, IXGBE_TDLEN(reg_idx),
			ring->count * sizeof(union ixgbe_adv_tx_desc));
	IXGBE_WRITE_REG(hw, IXGBE_TDH(reg_idx), 0);
	IXGBE_WRITE_REG(hw, IXGBE_TDT(reg_idx), 0);
	ring->tail = adapter->io_addr + IXGBE_TDT(reg_idx);

	/*
	 * set WTHRESH to encourage burst writeback, it should not be set
	 * higher than 1 when:
	 * - ITR is 0 as it could cause false TX hangs
	 * - ITR is set to > 100k int/sec and BQL is enabled
	 *
	 * In order to avoid issues WTHRESH + PTHRESH should always be equal
	 * to or less than the number of on chip descriptors, which is
	 * currently 40.
	 */
	if (!ring->q_vector || (ring->q_vector->itr < IXGBE_100K_ITR))
		txdctl |= 1u << 16;	/* WTHRESH = 1 */
	else
		txdctl |= 8u << 16;	/* WTHRESH = 8 */

	/*
	 * Setting PTHRESH to 32 both improves performance
	 * and avoids a TX hang with DFP enabled
	 */
	txdctl |= (1u << 8) |	/* HTHRESH = 1 */
		   32;		/* PTHRESH = 32 */

	/* reinitialize flowdirector state */
	if (adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE) {
		ring->atr_sample_rate = adapter->atr_sample_rate;
		ring->atr_count = 0;
		set_bit(__IXGBE_TX_FDIR_INIT_DONE, &ring->state);
	} else {
		ring->atr_sample_rate = 0;
	}

	/* initialize XPS */
	if (!test_and_set_bit(__IXGBE_TX_XPS_INIT_DONE, &ring->state)) {
		struct ixgbe_q_vector *q_vector = ring->q_vector;

		if (q_vector)
			netif_set_xps_queue(ring->netdev,
					    &q_vector->affinity_mask,
					    ring->queue_index);
	}

	clear_bit(__IXGBE_HANG_CHECK_ARMED, &ring->state);

	/* reinitialize tx_buffer_info */
	memset(ring->tx_buffer_info, 0,
	       sizeof(struct ixgbe_tx_buffer) * ring->count);

	/* enable queue */
	IXGBE_WRITE_REG(hw, IXGBE_TXDCTL(reg_idx), txdctl);

	/* TXDCTL.EN will return 0 on 82598 if link is down, so skip it */
	if (hw->mac.type == ixgbe_mac_82598EB &&
	    !(IXGBE_READ_REG(hw, IXGBE_LINKS) & IXGBE_LINKS_UP))
		return;

	/* poll to verify queue is enabled */
	do {
		usleep_range(1000, 2000);
		txdctl = IXGBE_READ_REG(hw, IXGBE_TXDCTL(reg_idx));
	} while (--wait_loop && !(txdctl & IXGBE_TXDCTL_ENABLE));
	if (!wait_loop)
		hw_dbg(hw, "Could not enable Tx Queue %d\n", reg_idx);
}

static void ixgbe_setup_mtqc(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 rttdcs, mtqc;
	u8 tcs = adapter->hw_tcs;

	if (hw->mac.type == ixgbe_mac_82598EB)
		return;

	/* disable the arbiter while setting MTQC */
	rttdcs = IXGBE_READ_REG(hw, IXGBE_RTTDCS);
	rttdcs |= IXGBE_RTTDCS_ARBDIS;
	IXGBE_WRITE_REG(hw, IXGBE_RTTDCS, rttdcs);

	/* set transmit pool layout */
	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) {
		mtqc = IXGBE_MTQC_VT_ENA;
		if (tcs > 4)
			mtqc |= IXGBE_MTQC_RT_ENA | IXGBE_MTQC_8TC_8TQ;
		else if (tcs > 1)
			mtqc |= IXGBE_MTQC_RT_ENA | IXGBE_MTQC_4TC_4TQ;
		else if (adapter->ring_feature[RING_F_VMDQ].mask ==
			 IXGBE_82599_VMDQ_4Q_MASK)
			mtqc |= IXGBE_MTQC_32VF;
		else
			mtqc |= IXGBE_MTQC_64VF;
	} else {
		if (tcs > 4) {
			mtqc = IXGBE_MTQC_RT_ENA | IXGBE_MTQC_8TC_8TQ;
		} else if (tcs > 1) {
			mtqc = IXGBE_MTQC_RT_ENA | IXGBE_MTQC_4TC_4TQ;
		} else {
			u8 max_txq = adapter->num_tx_queues +
				adapter->num_xdp_queues;
			if (max_txq > 63)
				mtqc = IXGBE_MTQC_RT_ENA | IXGBE_MTQC_4TC_4TQ;
			else
				mtqc = IXGBE_MTQC_64Q_1PB;
		}
	}

	IXGBE_WRITE_REG(hw, IXGBE_MTQC, mtqc);

	/* Enable Security TX Buffer IFG for multiple pb */
	if (tcs) {
		u32 sectx = IXGBE_READ_REG(hw, IXGBE_SECTXMINIFG);
		sectx |= IXGBE_SECTX_DCB;
		IXGBE_WRITE_REG(hw, IXGBE_SECTXMINIFG, sectx);
	}

	/* re-enable the arbiter */
	rttdcs &= ~IXGBE_RTTDCS_ARBDIS;
	IXGBE_WRITE_REG(hw, IXGBE_RTTDCS, rttdcs);
}

/**
 * ixgbe_configure_tx - Configure 8259x Transmit Unit after Reset
 * @adapter: board private structure
 *
 * Configure the Tx unit of the MAC after a reset.
 **/
static void ixgbe_configure_tx(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 dmatxctl;
	u32 i;

	ixgbe_setup_mtqc(adapter);

	if (hw->mac.type != ixgbe_mac_82598EB) {
		/* DMATXCTL.EN must be before Tx queues are enabled */
		dmatxctl = IXGBE_READ_REG(hw, IXGBE_DMATXCTL);
		dmatxctl |= IXGBE_DMATXCTL_TE;
		IXGBE_WRITE_REG(hw, IXGBE_DMATXCTL, dmatxctl);
	}

	/* Setup the HW Tx Head and Tail descriptor pointers */
	for (i = 0; i < adapter->num_tx_queues; i++)
		ixgbe_configure_tx_ring(adapter, adapter->tx_ring[i]);
	for (i = 0; i < adapter->num_xdp_queues; i++)
		ixgbe_configure_tx_ring(adapter, adapter->xdp_ring[i]);
}

static void ixgbe_enable_rx_drop(struct ixgbe_adapter *adapter,
				 struct ixgbe_ring *ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u8 reg_idx = ring->reg_idx;
	u32 srrctl = IXGBE_READ_REG(hw, IXGBE_SRRCTL(reg_idx));

	srrctl |= IXGBE_SRRCTL_DROP_EN;

	IXGBE_WRITE_REG(hw, IXGBE_SRRCTL(reg_idx), srrctl);
}

static void ixgbe_disable_rx_drop(struct ixgbe_adapter *adapter,
				  struct ixgbe_ring *ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u8 reg_idx = ring->reg_idx;
	u32 srrctl = IXGBE_READ_REG(hw, IXGBE_SRRCTL(reg_idx));

	srrctl &= ~IXGBE_SRRCTL_DROP_EN;

	IXGBE_WRITE_REG(hw, IXGBE_SRRCTL(reg_idx), srrctl);
}

#ifdef CONFIG_IXGBE_DCB
void ixgbe_set_rx_drop_en(struct ixgbe_adapter *adapter)
#else
static void ixgbe_set_rx_drop_en(struct ixgbe_adapter *adapter)
#endif
{
	int i;
	bool pfc_en = adapter->dcb_cfg.pfc_mode_enable;

	if (adapter->ixgbe_ieee_pfc)
		pfc_en |= !!(adapter->ixgbe_ieee_pfc->pfc_en);

	/*
	 * We should set the drop enable bit if:
	 *  SR-IOV is enabled
	 *   or
	 *  Number of Rx queues > 1 and flow control is disabled
	 *
	 *  This allows us to avoid head of line blocking for security
	 *  and performance reasons.
	 */
	if (adapter->num_vfs || (adapter->num_rx_queues > 1 &&
	    !(adapter->hw.fc.current_mode & ixgbe_fc_tx_pause) && !pfc_en)) {
		for (i = 0; i < adapter->num_rx_queues; i++)
			ixgbe_enable_rx_drop(adapter, adapter->rx_ring[i]);
	} else {
		for (i = 0; i < adapter->num_rx_queues; i++)
			ixgbe_disable_rx_drop(adapter, adapter->rx_ring[i]);
	}
}

#define IXGBE_SRRCTL_BSIZEHDRSIZE_SHIFT 2

static void ixgbe_configure_srrctl(struct ixgbe_adapter *adapter,
				   struct ixgbe_ring *rx_ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 srrctl;
	u8 reg_idx = rx_ring->reg_idx;

	if (hw->mac.type == ixgbe_mac_82598EB) {
		u16 mask = adapter->ring_feature[RING_F_RSS].mask;

		/*
		 * if VMDq is not active we must program one srrctl register
		 * per RSS queue since we have enabled RDRXCTL.MVMEN
		 */
		reg_idx &= mask;
	}

	/* configure header buffer length, needed for RSC */
	srrctl = IXGBE_RX_HDR_SIZE << IXGBE_SRRCTL_BSIZEHDRSIZE_SHIFT;

	/* configure the packet buffer length */
	if (rx_ring->xsk_pool) {
		u32 xsk_buf_len = xsk_pool_get_rx_frame_size(rx_ring->xsk_pool);

		/* If the MAC support setting RXDCTL.RLPML, the
		 * SRRCTL[n].BSIZEPKT is set to PAGE_SIZE and
		 * RXDCTL.RLPML is set to the actual UMEM buffer
		 * size. If not, then we are stuck with a 1k buffer
		 * size resolution. In this case frames larger than
		 * the UMEM buffer size viewed in a 1k resolution will
		 * be dropped.
		 */
		if (hw->mac.type != ixgbe_mac_82599EB)
			srrctl |= PAGE_SIZE >> IXGBE_SRRCTL_BSIZEPKT_SHIFT;
		else
			srrctl |= xsk_buf_len >> IXGBE_SRRCTL_BSIZEPKT_SHIFT;
	} else if (test_bit(__IXGBE_RX_3K_BUFFER, &rx_ring->state)) {
		srrctl |= IXGBE_RXBUFFER_3K >> IXGBE_SRRCTL_BSIZEPKT_SHIFT;
	} else {
		srrctl |= IXGBE_RXBUFFER_2K >> IXGBE_SRRCTL_BSIZEPKT_SHIFT;
	}

	/* configure descriptor type */
	srrctl |= IXGBE_SRRCTL_DESCTYPE_ADV_ONEBUF;

	IXGBE_WRITE_REG(hw, IXGBE_SRRCTL(reg_idx), srrctl);
}

/**
 * ixgbe_rss_indir_tbl_entries - Return RSS indirection table entries
 * @adapter: device handle
 *
 *  - 82598/82599/X540:     128
 *  - X550(non-SRIOV mode): 512
 *  - X550(SRIOV mode):     64
 */
u32 ixgbe_rss_indir_tbl_entries(struct ixgbe_adapter *adapter)
{
	if (adapter->hw.mac.type < ixgbe_mac_X550)
		return 128;
	else if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)
		return 64;
	else
		return 512;
}

/**
 * ixgbe_store_key - Write the RSS key to HW
 * @adapter: device handle
 *
 * Write the RSS key stored in adapter.rss_key to HW.
 */
void ixgbe_store_key(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	for (i = 0; i < 10; i++)
		IXGBE_WRITE_REG(hw, IXGBE_RSSRK(i), adapter->rss_key[i]);
}

/**
 * ixgbe_init_rss_key - Initialize adapter RSS key
 * @adapter: device handle
 *
 * Allocates and initializes the RSS key if it is not allocated.
 **/
static inline int ixgbe_init_rss_key(struct ixgbe_adapter *adapter)
{
	u32 *rss_key;

	if (!adapter->rss_key) {
		rss_key = kzalloc(IXGBE_RSS_KEY_SIZE, GFP_KERNEL);
		if (unlikely(!rss_key))
			return -ENOMEM;

		netdev_rss_key_fill(rss_key, IXGBE_RSS_KEY_SIZE);
		adapter->rss_key = rss_key;
	}

	return 0;
}

/**
 * ixgbe_store_reta - Write the RETA table to HW
 * @adapter: device handle
 *
 * Write the RSS redirection table stored in adapter.rss_indir_tbl[] to HW.
 */
void ixgbe_store_reta(struct ixgbe_adapter *adapter)
{
	u32 i, reta_entries = ixgbe_rss_indir_tbl_entries(adapter);
	struct ixgbe_hw *hw = &adapter->hw;
	u32 reta = 0;
	u32 indices_multi;
	u8 *indir_tbl = adapter->rss_indir_tbl;

	/* Fill out the redirection table as follows:
	 *  - 82598:      8 bit wide entries containing pair of 4 bit RSS
	 *    indices.
	 *  - 82599/X540: 8 bit wide entries containing 4 bit RSS index
	 *  - X550:       8 bit wide entries containing 6 bit RSS index
	 */
	if (adapter->hw.mac.type == ixgbe_mac_82598EB)
		indices_multi = 0x11;
	else
		indices_multi = 0x1;

	/* Write redirection table to HW */
	for (i = 0; i < reta_entries; i++) {
		reta |= indices_multi * indir_tbl[i] << (i & 0x3) * 8;
		if ((i & 3) == 3) {
			if (i < 128)
				IXGBE_WRITE_REG(hw, IXGBE_RETA(i >> 2), reta);
			else
				IXGBE_WRITE_REG(hw, IXGBE_ERETA((i >> 2) - 32),
						reta);
			reta = 0;
		}
	}
}

/**
 * ixgbe_store_vfreta - Write the RETA table to HW (x550 devices in SRIOV mode)
 * @adapter: device handle
 *
 * Write the RSS redirection table stored in adapter.rss_indir_tbl[] to HW.
 */
static void ixgbe_store_vfreta(struct ixgbe_adapter *adapter)
{
	u32 i, reta_entries = ixgbe_rss_indir_tbl_entries(adapter);
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vfreta = 0;

	/* Write redirection table to HW */
	for (i = 0; i < reta_entries; i++) {
		u16 pool = adapter->num_rx_pools;

		vfreta |= (u32)adapter->rss_indir_tbl[i] << (i & 0x3) * 8;
		if ((i & 3) != 3)
			continue;

		while (pool--)
			IXGBE_WRITE_REG(hw,
					IXGBE_PFVFRETA(i >> 2, VMDQ_P(pool)),
					vfreta);
		vfreta = 0;
	}
}

static void ixgbe_setup_reta(struct ixgbe_adapter *adapter)
{
	u32 i, j;
	u32 reta_entries = ixgbe_rss_indir_tbl_entries(adapter);
	u16 rss_i = adapter->ring_feature[RING_F_RSS].indices;

	/* Program table for at least 4 queues w/ SR-IOV so that VFs can
	 * make full use of any rings they may have.  We will use the
	 * PSRTYPE register to control how many rings we use within the PF.
	 */
	if ((adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) && (rss_i < 4))
		rss_i = 4;

	/* Fill out hash function seeds */
	ixgbe_store_key(adapter);

	/* Fill out redirection table */
	memset(adapter->rss_indir_tbl, 0, sizeof(adapter->rss_indir_tbl));

	for (i = 0, j = 0; i < reta_entries; i++, j++) {
		if (j == rss_i)
			j = 0;

		adapter->rss_indir_tbl[i] = j;
	}

	ixgbe_store_reta(adapter);
}

static void ixgbe_setup_vfreta(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u16 rss_i = adapter->ring_feature[RING_F_RSS].indices;
	int i, j;

	/* Fill out hash function seeds */
	for (i = 0; i < 10; i++) {
		u16 pool = adapter->num_rx_pools;

		while (pool--)
			IXGBE_WRITE_REG(hw,
					IXGBE_PFVFRSSRK(i, VMDQ_P(pool)),
					*(adapter->rss_key + i));
	}

	/* Fill out the redirection table */
	for (i = 0, j = 0; i < 64; i++, j++) {
		if (j == rss_i)
			j = 0;

		adapter->rss_indir_tbl[i] = j;
	}

	ixgbe_store_vfreta(adapter);
}

static void ixgbe_setup_mrqc(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 mrqc = 0, rss_field = 0, vfmrqc = 0;
	u32 rxcsum;

	/* Disable indicating checksum in descriptor, enables RSS hash */
	rxcsum = IXGBE_READ_REG(hw, IXGBE_RXCSUM);
	rxcsum |= IXGBE_RXCSUM_PCSD;
	IXGBE_WRITE_REG(hw, IXGBE_RXCSUM, rxcsum);

	if (adapter->hw.mac.type == ixgbe_mac_82598EB) {
		if (adapter->ring_feature[RING_F_RSS].mask)
			mrqc = IXGBE_MRQC_RSSEN;
	} else {
		u8 tcs = adapter->hw_tcs;

		if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) {
			if (tcs > 4)
				mrqc = IXGBE_MRQC_VMDQRT8TCEN;	/* 8 TCs */
			else if (tcs > 1)
				mrqc = IXGBE_MRQC_VMDQRT4TCEN;	/* 4 TCs */
			else if (adapter->ring_feature[RING_F_VMDQ].mask ==
				 IXGBE_82599_VMDQ_4Q_MASK)
				mrqc = IXGBE_MRQC_VMDQRSS32EN;
			else
				mrqc = IXGBE_MRQC_VMDQRSS64EN;

			/* Enable L3/L4 for Tx Switched packets only for X550,
			 * older devices do not support this feature
			 */
			if (hw->mac.type >= ixgbe_mac_X550)
				mrqc |= IXGBE_MRQC_L3L4TXSWEN;
		} else {
			if (tcs > 4)
				mrqc = IXGBE_MRQC_RTRSS8TCEN;
			else if (tcs > 1)
				mrqc = IXGBE_MRQC_RTRSS4TCEN;
			else
				mrqc = IXGBE_MRQC_RSSEN;
		}
	}

	/* Perform hash on these packet types */
	rss_field |= IXGBE_MRQC_RSS_FIELD_IPV4 |
		     IXGBE_MRQC_RSS_FIELD_IPV4_TCP |
		     IXGBE_MRQC_RSS_FIELD_IPV6 |
		     IXGBE_MRQC_RSS_FIELD_IPV6_TCP;

	if (adapter->flags2 & IXGBE_FLAG2_RSS_FIELD_IPV4_UDP)
		rss_field |= IXGBE_MRQC_RSS_FIELD_IPV4_UDP;
	if (adapter->flags2 & IXGBE_FLAG2_RSS_FIELD_IPV6_UDP)
		rss_field |= IXGBE_MRQC_RSS_FIELD_IPV6_UDP;

	if ((hw->mac.type >= ixgbe_mac_X550) &&
	    (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)) {
		u16 pool = adapter->num_rx_pools;

		/* Enable VF RSS mode */
		mrqc |= IXGBE_MRQC_MULTIPLE_RSS;
		IXGBE_WRITE_REG(hw, IXGBE_MRQC, mrqc);

		/* Setup RSS through the VF registers */
		ixgbe_setup_vfreta(adapter);
		vfmrqc = IXGBE_MRQC_RSSEN;
		vfmrqc |= rss_field;

		while (pool--)
			IXGBE_WRITE_REG(hw,
					IXGBE_PFVFMRQC(VMDQ_P(pool)),
					vfmrqc);
	} else {
		ixgbe_setup_reta(adapter);
		mrqc |= rss_field;
		IXGBE_WRITE_REG(hw, IXGBE_MRQC, mrqc);
	}
}

/**
 * ixgbe_configure_rscctl - enable RSC for the indicated ring
 * @adapter: address of board private structure
 * @ring: structure containing ring specific data
 **/
static void ixgbe_configure_rscctl(struct ixgbe_adapter *adapter,
				   struct ixgbe_ring *ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 rscctrl;
	u8 reg_idx = ring->reg_idx;

	if (!ring_is_rsc_enabled(ring))
		return;

	rscctrl = IXGBE_READ_REG(hw, IXGBE_RSCCTL(reg_idx));
	rscctrl |= IXGBE_RSCCTL_RSCEN;
	/*
	 * we must limit the number of descriptors so that the
	 * total size of max desc * buf_len is not greater
	 * than 65536
	 */
	rscctrl |= IXGBE_RSCCTL_MAXDESC_16;
	IXGBE_WRITE_REG(hw, IXGBE_RSCCTL(reg_idx), rscctrl);
}

#define IXGBE_MAX_RX_DESC_POLL 10
static void ixgbe_rx_desc_queue_enable(struct ixgbe_adapter *adapter,
				       struct ixgbe_ring *ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int wait_loop = IXGBE_MAX_RX_DESC_POLL;
	u32 rxdctl;
	u8 reg_idx = ring->reg_idx;

	if (ixgbe_removed(hw->hw_addr))
		return;
	/* RXDCTL.EN will return 0 on 82598 if link is down, so skip it */
	if (hw->mac.type == ixgbe_mac_82598EB &&
	    !(IXGBE_READ_REG(hw, IXGBE_LINKS) & IXGBE_LINKS_UP))
		return;

	do {
		usleep_range(1000, 2000);
		rxdctl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg_idx));
	} while (--wait_loop && !(rxdctl & IXGBE_RXDCTL_ENABLE));

	if (!wait_loop) {
		e_err(drv, "RXDCTL.ENABLE on Rx queue %d not set within "
		      "the polling period\n", reg_idx);
	}
}

void ixgbe_configure_rx_ring(struct ixgbe_adapter *adapter,
                             struct ixgbe_ring *ring) {
    struct ixgbe_hw *hw = &adapter->hw;
    u64 rdba = ring->dma;
    u32 rxdctl;
    u8 reg = ring->reg_idx;
    union ixgbe_adv_rx_desc *rx_desc;

    /* ---------------- DMA-zero bookkeeping -------------------- */
    xdp_rxq_info_unreg_mem_model(&ring->xdp_rxq);
    ring->xsk_pool = ixgbe_xsk_pool(adapter, ring);
    if (unlikely(ring->xsk_pool)) {  /* Rare in desktop/gaming; optimize common path */
        WARN_ON(xdp_rxq_info_reg_mem_model(&ring->xdp_rxq,
                                            MEM_TYPE_XSK_BUFF_POOL, NULL));
        xsk_pool_set_rxq_info(ring->xsk_pool, &ring->xdp_rxq);
    } else {
        WARN_ON(xdp_rxq_info_reg_mem_model(&ring->xdp_rxq,
                                            MEM_TYPE_PAGE_SHARED, NULL));
    }

    /* ---------------- disable queue while we update ------------ */
    rxdctl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg));
    rxdctl &= ~IXGBE_RXDCTL_ENABLE;
    IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(reg), rxdctl);
    IXGBE_WRITE_FLUSH(hw);

    /* ---------------- base, len, head/tail --------------------- */
    IXGBE_WRITE_REG(hw, IXGBE_RDBAL(reg), rdba & DMA_BIT_MASK(32));
    IXGBE_WRITE_REG(hw, IXGBE_RDBAH(reg), rdba >> 32);
    IXGBE_WRITE_REG(hw, IXGBE_RDLEN(reg),
                    ring->count * sizeof(union ixgbe_adv_rx_desc));
    IXGBE_WRITE_FLUSH(hw);  /* prevent MDD */

    IXGBE_WRITE_REG(hw, IXGBE_RDH(reg), 0);
    IXGBE_WRITE_REG(hw, IXGBE_RDT(reg), 0);
    ring->tail = adapter->io_addr + IXGBE_RDT(reg);

    ixgbe_configure_srrctl(adapter, ring);
    ixgbe_configure_rscctl(adapter, ring);

    /* ---------------- per-family tweaks ------------------------ */
    if (hw->mac.type == ixgbe_mac_82598EB) {
        rxdctl &= ~0x3FFFFF;
        rxdctl |= 0x080420;  /* Intel default 82598 */
#if PAGE_SIZE < 8192
    } else if (hw->mac.type != ixgbe_mac_82599EB) {
        rxdctl &= ~(IXGBE_RXDCTL_RLPMLMASK | IXGBE_RXDCTL_RLPML_EN);
        if (ring_uses_build_skb(ring) &&
            !test_bit(__IXGBE_RX_3K_BUFFER, &ring->state)) {
            rxdctl |= IXGBE_MAX_2K_FRAME_BUILD_SKB | IXGBE_RXDCTL_RLPML_EN;
        }
#endif
    }

    /* ---------------- X540: enable write-back watermark -------- */
    if (hw->mac.type == ixgbe_mac_X540) {
#define IXGBE_RXDCTL_WTHRESH_MASK  (0x3F << 16)
#define IXGBE_RXDCTL_WTHRESH_SHIFT 16
        rxdctl &= ~IXGBE_RXDCTL_WTHRESH_MASK;
        rxdctl |= 31 << IXGBE_RXDCTL_WTHRESH_SHIFT; /* burst-2-cl; optimal for x540 latency */
    }

    /* ---------------- XSK pool RLPML override ------------------ */
    ring->rx_offset = ixgbe_rx_offset(ring);
    if (unlikely(ring->xsk_pool && hw->mac.type != ixgbe_mac_82599EB)) {  /* Rare in desktop/gaming */
        u32 xsk_len = xsk_pool_get_rx_frame_size(ring->xsk_pool);

        rxdctl &= ~(IXGBE_RXDCTL_RLPMLMASK | IXGBE_RXDCTL_RLPML_EN);
        rxdctl |= xsk_len | IXGBE_RXDCTL_RLPML_EN;
        ring->rx_buf_len = xsk_len;
    }

    /* ---------------- clear software ring ---------------------- */
    prefetch(ring->rx_buffer_info);  /* Prefetch for faster memset in gaming init */
    memset(ring->rx_buffer_info, 0,
           sizeof(struct ixgbe_rx_buffer) * ring->count);

    rx_desc = IXGBE_RX_DESC(ring, 0);
    rx_desc->wb.upper.length = 0;

    /* ---------------- enable queue ----------------------------- */
    rxdctl |= IXGBE_RXDCTL_ENABLE;
    IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(reg), rxdctl);

    ixgbe_rx_desc_queue_enable(adapter, ring);

    if (unlikely(ring->xsk_pool)) {  /* Rare in desktop/gaming */
        ixgbe_alloc_rx_buffers_zc(ring, ixgbe_desc_unused(ring));
    } else {
        ixgbe_alloc_rx_buffers(ring, ixgbe_desc_unused(ring));
    }
}

static void ixgbe_setup_psrtype(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int rss_i = adapter->ring_feature[RING_F_RSS].indices;
	u16 pool = adapter->num_rx_pools;

	/* PSRTYPE must be initialized in non 82598 adapters */
	u32 psrtype = IXGBE_PSRTYPE_TCPHDR |
		      IXGBE_PSRTYPE_UDPHDR |
		      IXGBE_PSRTYPE_IPV4HDR |
		      IXGBE_PSRTYPE_L2HDR |
		      IXGBE_PSRTYPE_IPV6HDR;

	if (hw->mac.type == ixgbe_mac_82598EB)
		return;

	if (rss_i > 3)
		psrtype |= 2u << 29;
	else if (rss_i > 1)
		psrtype |= 1u << 29;

	while (pool--)
		IXGBE_WRITE_REG(hw, IXGBE_PSRTYPE(VMDQ_P(pool)), psrtype);
}

static void ixgbe_configure_virtualization(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u16 pool = adapter->num_rx_pools;
	u32 reg_offset, vf_shift, vmolr;
	u32 gcr_ext, vmdctl;
	int i;

	if (!(adapter->flags & IXGBE_FLAG_SRIOV_ENABLED))
		return;

	vmdctl = IXGBE_READ_REG(hw, IXGBE_VT_CTL);
	vmdctl |= IXGBE_VMD_CTL_VMDQ_EN;
	vmdctl &= ~IXGBE_VT_CTL_POOL_MASK;
	vmdctl |= VMDQ_P(0) << IXGBE_VT_CTL_POOL_SHIFT;
	vmdctl |= IXGBE_VT_CTL_REPLEN;
	IXGBE_WRITE_REG(hw, IXGBE_VT_CTL, vmdctl);

	/* accept untagged packets until a vlan tag is
	 * specifically set for the VMDQ queue/pool
	 */
	vmolr = IXGBE_VMOLR_AUPE;
	while (pool--)
		IXGBE_WRITE_REG(hw, IXGBE_VMOLR(VMDQ_P(pool)), vmolr);

	vf_shift = VMDQ_P(0) % 32;
	reg_offset = (VMDQ_P(0) >= 32) ? 1 : 0;

	/* Enable only the PF's pool for Tx/Rx */
	IXGBE_WRITE_REG(hw, IXGBE_VFRE(reg_offset), GENMASK(31, vf_shift));
	IXGBE_WRITE_REG(hw, IXGBE_VFRE(reg_offset ^ 1), reg_offset - 1);
	IXGBE_WRITE_REG(hw, IXGBE_VFTE(reg_offset), GENMASK(31, vf_shift));
	IXGBE_WRITE_REG(hw, IXGBE_VFTE(reg_offset ^ 1), reg_offset - 1);
	if (adapter->bridge_mode == BRIDGE_MODE_VEB)
		IXGBE_WRITE_REG(hw, IXGBE_PFDTXGSWC, IXGBE_PFDTXGSWC_VT_LBEN);

	/* Map PF MAC address in RAR Entry 0 to first pool following VFs */
	hw->mac.ops.set_vmdq(hw, 0, VMDQ_P(0));

	/* clear VLAN promisc flag so VFTA will be updated if necessary */
	adapter->flags2 &= ~IXGBE_FLAG2_VLAN_PROMISC;

	/*
	 * Set up VF register offsets for selected VT Mode,
	 * i.e. 32 or 64 VFs for SR-IOV
	 */
	switch (adapter->ring_feature[RING_F_VMDQ].mask) {
	case IXGBE_82599_VMDQ_8Q_MASK:
		gcr_ext = IXGBE_GCR_EXT_VT_MODE_16;
		break;
	case IXGBE_82599_VMDQ_4Q_MASK:
		gcr_ext = IXGBE_GCR_EXT_VT_MODE_32;
		break;
	default:
		gcr_ext = IXGBE_GCR_EXT_VT_MODE_64;
		break;
	}

	IXGBE_WRITE_REG(hw, IXGBE_GCR_EXT, gcr_ext);

	for (i = 0; i < adapter->num_vfs; i++) {
		/* configure spoof checking */
		ixgbe_ndo_set_vf_spoofchk(adapter->netdev, i,
					  adapter->vfinfo[i].spoofchk_enabled);

		/* Enable/Disable RSS query feature  */
		ixgbe_ndo_set_vf_rss_query_en(adapter->netdev, i,
					  adapter->vfinfo[i].rss_query_enabled);
	}
}

static void ixgbe_set_rx_buffer_len(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct net_device *netdev = adapter->netdev;
	int max_frame = netdev->mtu + ETH_HLEN + ETH_FCS_LEN;
	struct ixgbe_ring *rx_ring;
	int i;
	u32 mhadd, hlreg0;

#ifdef IXGBE_FCOE
	/* adjust max frame to be able to do baby jumbo for FCoE */
	if ((adapter->flags & IXGBE_FLAG_FCOE_ENABLED) &&
	    (max_frame < IXGBE_FCOE_JUMBO_FRAME_SIZE))
		max_frame = IXGBE_FCOE_JUMBO_FRAME_SIZE;

#endif /* IXGBE_FCOE */

	/* adjust max frame to be at least the size of a standard frame */
	if (max_frame < (ETH_FRAME_LEN + ETH_FCS_LEN))
		max_frame = (ETH_FRAME_LEN + ETH_FCS_LEN);

	mhadd = IXGBE_READ_REG(hw, IXGBE_MHADD);
	if (max_frame != (mhadd >> IXGBE_MHADD_MFS_SHIFT)) {
		mhadd &= ~IXGBE_MHADD_MFS_MASK;
		mhadd |= max_frame << IXGBE_MHADD_MFS_SHIFT;

		IXGBE_WRITE_REG(hw, IXGBE_MHADD, mhadd);
	}

	hlreg0 = IXGBE_READ_REG(hw, IXGBE_HLREG0);
	/* set jumbo enable since MHADD.MFS is keeping size locked at max_frame */
	hlreg0 |= IXGBE_HLREG0_JUMBOEN;
	IXGBE_WRITE_REG(hw, IXGBE_HLREG0, hlreg0);

	/*
	 * Setup the HW Rx Head and Tail Descriptor Pointers and
	 * the Base and Length of the Rx Descriptor Ring
	 */
	for (i = 0; i < adapter->num_rx_queues; i++) {
		rx_ring = adapter->rx_ring[i];

		clear_ring_rsc_enabled(rx_ring);
		clear_bit(__IXGBE_RX_3K_BUFFER, &rx_ring->state);
		clear_bit(__IXGBE_RX_BUILD_SKB_ENABLED, &rx_ring->state);

		if (adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED)
			set_ring_rsc_enabled(rx_ring);

		if (test_bit(__IXGBE_RX_FCOE, &rx_ring->state))
			set_bit(__IXGBE_RX_3K_BUFFER, &rx_ring->state);

		if (adapter->flags2 & IXGBE_FLAG2_RX_LEGACY)
			continue;

		set_bit(__IXGBE_RX_BUILD_SKB_ENABLED, &rx_ring->state);

#if (PAGE_SIZE < 8192)
		if (adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED)
			set_bit(__IXGBE_RX_3K_BUFFER, &rx_ring->state);

		if (IXGBE_2K_TOO_SMALL_WITH_PADDING ||
		    (max_frame > (ETH_FRAME_LEN + ETH_FCS_LEN)))
			set_bit(__IXGBE_RX_3K_BUFFER, &rx_ring->state);
#endif
	}
}

static void ixgbe_setup_rdrxctl(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 rdrxctl = IXGBE_READ_REG(hw, IXGBE_RDRXCTL);

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		/*
		 * For VMDq support of different descriptor types or
		 * buffer sizes through the use of multiple SRRCTL
		 * registers, RDRXCTL.MVMEN must be set to 1
		 *
		 * also, the manual doesn't mention it clearly but DCA hints
		 * will only use queue 0's tags unless this bit is set.  Side
		 * effects of setting this bit are only that SRRCTL must be
		 * fully programmed [0..15]
		 */
		rdrxctl |= IXGBE_RDRXCTL_MVMEN;
		break;
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		if (adapter->num_vfs)
			rdrxctl |= IXGBE_RDRXCTL_PSP;
		fallthrough;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
		/* Disable RSC for ACK packets */
		IXGBE_WRITE_REG(hw, IXGBE_RSCDBU,
		   (IXGBE_RSCDBU_RSCACKDIS | IXGBE_READ_REG(hw, IXGBE_RSCDBU)));
		rdrxctl &= ~IXGBE_RDRXCTL_RSCFRSTSIZE;
		/* hardware requires some bits to be set by default */
		rdrxctl |= (IXGBE_RDRXCTL_RSCACKC | IXGBE_RDRXCTL_FCOE_WRFIX);
		rdrxctl |= IXGBE_RDRXCTL_CRCSTRIP;
		break;
	default:
		/* We should do nothing since we don't know this hardware */
		return;
	}

	IXGBE_WRITE_REG(hw, IXGBE_RDRXCTL, rdrxctl);
}

/**
 * ixgbe_configure_rx - Configure 8259x Receive Unit after Reset
 * @adapter: board private structure
 *
 * Configure the Rx unit of the MAC after a reset.
 **/
static void ixgbe_configure_rx(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int i;
	u32 rxctrl, rfctl;

	/* disable receives while setting up the descriptors */
	hw->mac.ops.disable_rx(hw);

	ixgbe_setup_psrtype(adapter);
	ixgbe_setup_rdrxctl(adapter);

	/* RSC Setup */
	rfctl = IXGBE_READ_REG(hw, IXGBE_RFCTL);
	rfctl &= ~IXGBE_RFCTL_RSC_DIS;
	if (!(adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED))
		rfctl |= IXGBE_RFCTL_RSC_DIS;

	/* disable NFS filtering */
	rfctl |= (IXGBE_RFCTL_NFSW_DIS | IXGBE_RFCTL_NFSR_DIS);
	IXGBE_WRITE_REG(hw, IXGBE_RFCTL, rfctl);

	/* Program registers for the distribution of queues */
	ixgbe_setup_mrqc(adapter);

	/* set_rx_buffer_len must be called before ring initialization */
	ixgbe_set_rx_buffer_len(adapter);

	/*
	 * Setup the HW Rx Head and Tail Descriptor Pointers and
	 * the Base and Length of the Rx Descriptor Ring
	 */
	for (i = 0; i < adapter->num_rx_queues; i++)
		ixgbe_configure_rx_ring(adapter, adapter->rx_ring[i]);

	rxctrl = IXGBE_READ_REG(hw, IXGBE_RXCTRL);
	/* disable drop enable for 82598 parts */
	if (hw->mac.type == ixgbe_mac_82598EB)
		rxctrl |= IXGBE_RXCTRL_DMBYPS;

	/* enable all receives */
	rxctrl |= IXGBE_RXCTRL_RXEN;
	hw->mac.ops.enable_rx_dma(hw, rxctrl);
}

static int ixgbe_vlan_rx_add_vid(struct net_device *netdev,
				 __be16 proto, u16 vid)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;

	/* add VID to filter table */
	if (!vid || !(adapter->flags2 & IXGBE_FLAG2_VLAN_PROMISC))
		hw->mac.ops.set_vfta(&adapter->hw, vid, VMDQ_P(0), true, !!vid);

	set_bit(vid, adapter->active_vlans);

	return 0;
}

static int ixgbe_find_vlvf_entry(struct ixgbe_hw *hw, u32 vlan)
{
	u32 vlvf;
	int idx;

	/* short cut the special case */
	if (vlan == 0)
		return 0;

	/* Search for the vlan id in the VLVF entries */
	for (idx = IXGBE_VLVF_ENTRIES; --idx;) {
		vlvf = IXGBE_READ_REG(hw, IXGBE_VLVF(idx));
		if ((vlvf & VLAN_VID_MASK) == vlan)
			break;
	}

	return idx;
}

void ixgbe_update_pf_promisc_vlvf(struct ixgbe_adapter *adapter, u32 vid)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 bits, word;
	int idx;

	idx = ixgbe_find_vlvf_entry(hw, vid);
	if (!idx)
		return;

	/* See if any other pools are set for this VLAN filter
	 * entry other than the PF.
	 */
	word = idx * 2 + (VMDQ_P(0) / 32);
	bits = ~BIT(VMDQ_P(0) % 32);
	bits &= IXGBE_READ_REG(hw, IXGBE_VLVFB(word));

	/* Disable the filter so this falls into the default pool. */
	if (!bits && !IXGBE_READ_REG(hw, IXGBE_VLVFB(word ^ 1))) {
		if (!(adapter->flags2 & IXGBE_FLAG2_VLAN_PROMISC))
			IXGBE_WRITE_REG(hw, IXGBE_VLVFB(word), 0);
		IXGBE_WRITE_REG(hw, IXGBE_VLVF(idx), 0);
	}
}

static int ixgbe_vlan_rx_kill_vid(struct net_device *netdev,
				  __be16 proto, u16 vid)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;

	/* remove VID from filter table */
	if (vid && !(adapter->flags2 & IXGBE_FLAG2_VLAN_PROMISC))
		hw->mac.ops.set_vfta(hw, vid, VMDQ_P(0), false, true);

	clear_bit(vid, adapter->active_vlans);

	return 0;
}

/**
 * ixgbe_vlan_strip_disable - helper to disable hw vlan stripping
 * @adapter: driver data
 */
static void ixgbe_vlan_strip_disable(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vlnctrl;
	int i, j;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		vlnctrl = IXGBE_READ_REG(hw, IXGBE_VLNCTRL);
		vlnctrl &= ~IXGBE_VLNCTRL_VME;
		IXGBE_WRITE_REG(hw, IXGBE_VLNCTRL, vlnctrl);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		for (i = 0; i < adapter->num_rx_queues; i++) {
			struct ixgbe_ring *ring = adapter->rx_ring[i];

			if (!netif_is_ixgbe(ring->netdev))
				continue;

			j = ring->reg_idx;
			vlnctrl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(j));
			vlnctrl &= ~IXGBE_RXDCTL_VME;
			IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(j), vlnctrl);
		}
		break;
	default:
		break;
	}
}

/**
 * ixgbe_vlan_strip_enable - helper to enable hw vlan stripping
 * @adapter: driver data
 */
static void ixgbe_vlan_strip_enable(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vlnctrl;
	int i, j;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		vlnctrl = IXGBE_READ_REG(hw, IXGBE_VLNCTRL);
		vlnctrl |= IXGBE_VLNCTRL_VME;
		IXGBE_WRITE_REG(hw, IXGBE_VLNCTRL, vlnctrl);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		for (i = 0; i < adapter->num_rx_queues; i++) {
			struct ixgbe_ring *ring = adapter->rx_ring[i];

			if (!netif_is_ixgbe(ring->netdev))
				continue;

			j = ring->reg_idx;
			vlnctrl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(j));
			vlnctrl |= IXGBE_RXDCTL_VME;
			IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(j), vlnctrl);
		}
		break;
	default:
		break;
	}
}

static void ixgbe_vlan_promisc_enable(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vlnctrl, i;

	vlnctrl = IXGBE_READ_REG(hw, IXGBE_VLNCTRL);

	if (adapter->flags & IXGBE_FLAG_VMDQ_ENABLED) {
	/* For VMDq and SR-IOV we must leave VLAN filtering enabled */
		vlnctrl |= IXGBE_VLNCTRL_VFE;
		IXGBE_WRITE_REG(hw, IXGBE_VLNCTRL, vlnctrl);
	} else {
		vlnctrl &= ~IXGBE_VLNCTRL_VFE;
		IXGBE_WRITE_REG(hw, IXGBE_VLNCTRL, vlnctrl);
		return;
	}

	/* Nothing to do for 82598 */
	if (hw->mac.type == ixgbe_mac_82598EB)
		return;

	/* We are already in VLAN promisc, nothing to do */
	if (adapter->flags2 & IXGBE_FLAG2_VLAN_PROMISC)
		return;

	/* Set flag so we don't redo unnecessary work */
	adapter->flags2 |= IXGBE_FLAG2_VLAN_PROMISC;

	/* Add PF to all active pools */
	for (i = IXGBE_VLVF_ENTRIES; --i;) {
		u32 reg_offset = IXGBE_VLVFB(i * 2 + VMDQ_P(0) / 32);
		u32 vlvfb = IXGBE_READ_REG(hw, reg_offset);

		vlvfb |= BIT(VMDQ_P(0) % 32);
		IXGBE_WRITE_REG(hw, reg_offset, vlvfb);
	}

	/* Set all bits in the VLAN filter table array */
	for (i = hw->mac.vft_size; i--;)
		IXGBE_WRITE_REG(hw, IXGBE_VFTA(i), ~0U);
}

#define VFTA_BLOCK_SIZE 8
static void ixgbe_scrub_vfta(struct ixgbe_adapter *adapter, u32 vfta_offset)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vfta[VFTA_BLOCK_SIZE] = { 0 };
	u32 vid_start = vfta_offset * 32;
	u32 vid_end = vid_start + (VFTA_BLOCK_SIZE * 32);
	u32 i, vid, word, bits;

	for (i = IXGBE_VLVF_ENTRIES; --i;) {
		u32 vlvf = IXGBE_READ_REG(hw, IXGBE_VLVF(i));

		/* pull VLAN ID from VLVF */
		vid = vlvf & VLAN_VID_MASK;

		/* only concern outselves with a certain range */
		if (vid < vid_start || vid >= vid_end)
			continue;

		if (vlvf) {
			/* record VLAN ID in VFTA */
			vfta[(vid - vid_start) / 32] |= BIT(vid % 32);

			/* if PF is part of this then continue */
			if (test_bit(vid, adapter->active_vlans))
				continue;
		}

		/* remove PF from the pool */
		word = i * 2 + VMDQ_P(0) / 32;
		bits = ~BIT(VMDQ_P(0) % 32);
		bits &= IXGBE_READ_REG(hw, IXGBE_VLVFB(word));
		IXGBE_WRITE_REG(hw, IXGBE_VLVFB(word), bits);
	}

	/* extract values from active_vlans and write back to VFTA */
	for (i = VFTA_BLOCK_SIZE; i--;) {
		vid = (vfta_offset + i) * 32;
		word = vid / BITS_PER_LONG;
		bits = vid % BITS_PER_LONG;

		vfta[i] |= adapter->active_vlans[word] >> bits;

		IXGBE_WRITE_REG(hw, IXGBE_VFTA(vfta_offset + i), vfta[i]);
	}
}

static void ixgbe_vlan_promisc_disable(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 vlnctrl, i;

	/* Set VLAN filtering to enabled */
	vlnctrl = IXGBE_READ_REG(hw, IXGBE_VLNCTRL);
	vlnctrl |= IXGBE_VLNCTRL_VFE;
	IXGBE_WRITE_REG(hw, IXGBE_VLNCTRL, vlnctrl);

	if (!(adapter->flags & IXGBE_FLAG_VMDQ_ENABLED) ||
	    hw->mac.type == ixgbe_mac_82598EB)
		return;

	/* We are not in VLAN promisc, nothing to do */
	if (!(adapter->flags2 & IXGBE_FLAG2_VLAN_PROMISC))
		return;

	/* Set flag so we don't redo unnecessary work */
	adapter->flags2 &= ~IXGBE_FLAG2_VLAN_PROMISC;

	for (i = 0; i < hw->mac.vft_size; i += VFTA_BLOCK_SIZE)
		ixgbe_scrub_vfta(adapter, i);
}

static void ixgbe_restore_vlan(struct ixgbe_adapter *adapter)
{
	u16 vid = 1;

	ixgbe_vlan_rx_add_vid(adapter->netdev, htons(ETH_P_8021Q), 0);

	for_each_set_bit_from(vid, adapter->active_vlans, VLAN_N_VID)
		ixgbe_vlan_rx_add_vid(adapter->netdev, htons(ETH_P_8021Q), vid);
}

/**
 * ixgbe_write_mc_addr_list - write multicast addresses to MTA
 * @netdev: network interface device structure
 *
 * Writes multicast address list to the MTA hash table.
 * Returns: -ENOMEM on failure
 *                0 on no addresses written
 *                X on writing X addresses to MTA
 **/
static int ixgbe_write_mc_addr_list(struct net_device *netdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;

	if (!netif_running(netdev))
		return 0;

	if (hw->mac.ops.update_mc_addr_list)
		hw->mac.ops.update_mc_addr_list(hw, netdev);
	else
		return -ENOMEM;

#ifdef CONFIG_PCI_IOV
	ixgbe_restore_vf_multicasts(adapter);
#endif

	return netdev_mc_count(netdev);
}

#ifdef CONFIG_PCI_IOV
void ixgbe_full_sync_mac_table(struct ixgbe_adapter *adapter)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		mac_table->state &= ~IXGBE_MAC_STATE_MODIFIED;

		if (mac_table->state & IXGBE_MAC_STATE_IN_USE)
			hw->mac.ops.set_rar(hw, i,
					    mac_table->addr,
					    mac_table->pool,
					    IXGBE_RAH_AV);
		else
			hw->mac.ops.clear_rar(hw, i);
	}
}

#endif
static void ixgbe_sync_mac_table(struct ixgbe_adapter *adapter)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		if (!(mac_table->state & IXGBE_MAC_STATE_MODIFIED))
			continue;

		mac_table->state &= ~IXGBE_MAC_STATE_MODIFIED;

		if (mac_table->state & IXGBE_MAC_STATE_IN_USE)
			hw->mac.ops.set_rar(hw, i,
					    mac_table->addr,
					    mac_table->pool,
					    IXGBE_RAH_AV);
		else
			hw->mac.ops.clear_rar(hw, i);
	}
}

static void ixgbe_flush_sw_mac_table(struct ixgbe_adapter *adapter)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		mac_table->state |= IXGBE_MAC_STATE_MODIFIED;
		mac_table->state &= ~IXGBE_MAC_STATE_IN_USE;
	}

	ixgbe_sync_mac_table(adapter);
}

static int ixgbe_available_rars(struct ixgbe_adapter *adapter, u16 pool)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i, count = 0;

	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		/* do not count default RAR as available */
		if (mac_table->state & IXGBE_MAC_STATE_DEFAULT)
			continue;

		/* only count unused and addresses that belong to us */
		if (mac_table->state & IXGBE_MAC_STATE_IN_USE) {
			if (mac_table->pool != pool)
				continue;
		}

		count++;
	}

	return count;
}

/* this function destroys the first RAR entry */
static void ixgbe_mac_set_default_filter(struct ixgbe_adapter *adapter)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;

	memcpy(&mac_table->addr, hw->mac.addr, ETH_ALEN);
	mac_table->pool = VMDQ_P(0);

	mac_table->state = IXGBE_MAC_STATE_DEFAULT | IXGBE_MAC_STATE_IN_USE;

	hw->mac.ops.set_rar(hw, 0, mac_table->addr, mac_table->pool,
			    IXGBE_RAH_AV);
}

int ixgbe_add_mac_filter(struct ixgbe_adapter *adapter,
			 const u8 *addr, u16 pool)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	if (is_zero_ether_addr(addr))
		return -EINVAL;

	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		if (mac_table->state & IXGBE_MAC_STATE_IN_USE)
			continue;

		ether_addr_copy(mac_table->addr, addr);
		mac_table->pool = pool;

		mac_table->state |= IXGBE_MAC_STATE_MODIFIED |
				    IXGBE_MAC_STATE_IN_USE;

		ixgbe_sync_mac_table(adapter);

		return i;
	}

	return -ENOMEM;
}

int ixgbe_del_mac_filter(struct ixgbe_adapter *adapter,
			 const u8 *addr, u16 pool)
{
	struct ixgbe_mac_addr *mac_table = &adapter->mac_table[0];
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	if (is_zero_ether_addr(addr))
		return -EINVAL;

	/* search table for addr, if found clear IN_USE flag and sync */
	for (i = 0; i < hw->mac.num_rar_entries; i++, mac_table++) {
		/* we can only delete an entry if it is in use */
		if (!(mac_table->state & IXGBE_MAC_STATE_IN_USE))
			continue;
		/* we only care about entries that belong to the given pool */
		if (mac_table->pool != pool)
			continue;
		/* we only care about a specific MAC address */
		if (!ether_addr_equal(addr, mac_table->addr))
			continue;

		mac_table->state |= IXGBE_MAC_STATE_MODIFIED;
		mac_table->state &= ~IXGBE_MAC_STATE_IN_USE;

		ixgbe_sync_mac_table(adapter);

		return 0;
	}

	return -ENOMEM;
}

static int ixgbe_uc_sync(struct net_device *netdev, const unsigned char *addr)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	int ret;

	ret = ixgbe_add_mac_filter(adapter, addr, VMDQ_P(0));

	return min_t(int, ret, 0);
}

static int ixgbe_uc_unsync(struct net_device *netdev, const unsigned char *addr)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	ixgbe_del_mac_filter(adapter, addr, VMDQ_P(0));

	return 0;
}

/**
 * ixgbe_set_rx_mode - Unicast, Multicast and Promiscuous mode set
 * @netdev: network interface device structure
 *
 * The set_rx_method entry point is called whenever the unicast/multicast
 * address list or the network interface flags are updated.  This routine is
 * responsible for configuring the hardware for proper unicast, multicast and
 * promiscuous mode.
 **/
void ixgbe_set_rx_mode(struct net_device *netdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;
	u32 fctrl, vmolr = IXGBE_VMOLR_BAM | IXGBE_VMOLR_AUPE;
	netdev_features_t features = netdev->features;
	int count;

	/* Check for Promiscuous and All Multicast modes */
	fctrl = IXGBE_READ_REG(hw, IXGBE_FCTRL);

	/* set all bits that we expect to always be set */
	fctrl &= ~IXGBE_FCTRL_SBP; /* disable store-bad-packets */
	fctrl |= IXGBE_FCTRL_BAM;
	fctrl |= IXGBE_FCTRL_DPF; /* discard pause frames when FC enabled */
	fctrl |= IXGBE_FCTRL_PMCF;

	/* clear the bits we are changing the status of */
	fctrl &= ~(IXGBE_FCTRL_UPE | IXGBE_FCTRL_MPE);
	if (netdev->flags & IFF_PROMISC) {
		hw->addr_ctrl.user_set_promisc = true;
		fctrl |= (IXGBE_FCTRL_UPE | IXGBE_FCTRL_MPE);
		vmolr |= IXGBE_VMOLR_MPE;
		features &= ~NETIF_F_HW_VLAN_CTAG_FILTER;
	} else {
		if (netdev->flags & IFF_ALLMULTI) {
			fctrl |= IXGBE_FCTRL_MPE;
			vmolr |= IXGBE_VMOLR_MPE;
		}
		hw->addr_ctrl.user_set_promisc = false;
	}

	/*
	 * Write addresses to available RAR registers, if there is not
	 * sufficient space to store all the addresses then enable
	 * unicast promiscuous mode
	 */
	if (__dev_uc_sync(netdev, ixgbe_uc_sync, ixgbe_uc_unsync)) {
		fctrl |= IXGBE_FCTRL_UPE;
		vmolr |= IXGBE_VMOLR_ROPE;
	}

	/* Write addresses to the MTA, if the attempt fails
	 * then we should just turn on promiscuous mode so
	 * that we can at least receive multicast traffic
	 */
	count = ixgbe_write_mc_addr_list(netdev);
	if (count < 0) {
		fctrl |= IXGBE_FCTRL_MPE;
		vmolr |= IXGBE_VMOLR_MPE;
	} else if (count) {
		vmolr |= IXGBE_VMOLR_ROMPE;
	}

	if (hw->mac.type != ixgbe_mac_82598EB) {
		vmolr |= IXGBE_READ_REG(hw, IXGBE_VMOLR(VMDQ_P(0))) &
			 ~(IXGBE_VMOLR_MPE | IXGBE_VMOLR_ROMPE |
			   IXGBE_VMOLR_ROPE);
		IXGBE_WRITE_REG(hw, IXGBE_VMOLR(VMDQ_P(0)), vmolr);
	}

	/* This is useful for sniffing bad packets. */
	if (features & NETIF_F_RXALL) {
		/* UPE and MPE will be handled by normal PROMISC logic
		 * in e1000e_set_rx_mode */
		fctrl |= (IXGBE_FCTRL_SBP | /* Receive bad packets */
			  IXGBE_FCTRL_BAM | /* RX All Bcast Pkts */
			  IXGBE_FCTRL_PMCF); /* RX All MAC Ctrl Pkts */

		fctrl &= ~(IXGBE_FCTRL_DPF);
		/* NOTE:  VLAN filtering is disabled by setting PROMISC */
	}

	IXGBE_WRITE_REG(hw, IXGBE_FCTRL, fctrl);

	if (features & NETIF_F_HW_VLAN_CTAG_RX)
		ixgbe_vlan_strip_enable(adapter);
	else
		ixgbe_vlan_strip_disable(adapter);

	if (features & NETIF_F_HW_VLAN_CTAG_FILTER)
		ixgbe_vlan_promisc_disable(adapter);
	else
		ixgbe_vlan_promisc_enable(adapter);
}

static void ixgbe_napi_enable_all(struct ixgbe_adapter *adapter)
{
	int q_idx;

	for (q_idx = 0; q_idx < adapter->num_q_vectors; q_idx++)
		napi_enable(&adapter->q_vector[q_idx]->napi);
}

static void ixgbe_napi_disable_all(struct ixgbe_adapter *adapter)
{
	int q_idx;

	for (q_idx = 0; q_idx < adapter->num_q_vectors; q_idx++)
		napi_disable(&adapter->q_vector[q_idx]->napi);
}

static int ixgbe_udp_tunnel_sync(struct net_device *dev, unsigned int table)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct ixgbe_hw *hw = &adapter->hw;
	struct udp_tunnel_info ti;

	udp_tunnel_nic_get_port(dev, table, 0, &ti);
	if (ti.type == UDP_TUNNEL_TYPE_VXLAN)
		adapter->vxlan_port = ti.port;
	else
		adapter->geneve_port = ti.port;

	IXGBE_WRITE_REG(hw, IXGBE_VXLANCTRL,
			ntohs(adapter->vxlan_port) |
			ntohs(adapter->geneve_port) <<
				IXGBE_VXLANCTRL_GENEVE_UDPPORT_SHIFT);
	return 0;
}

static const struct udp_tunnel_nic_info ixgbe_udp_tunnels_x550 = {
	.sync_table	= ixgbe_udp_tunnel_sync,
	.flags		= UDP_TUNNEL_NIC_INFO_IPV4_ONLY,
	.tables		= {
		{ .n_entries = 1, .tunnel_types = UDP_TUNNEL_TYPE_VXLAN,  },
	},
};

static const struct udp_tunnel_nic_info ixgbe_udp_tunnels_x550em_a = {
	.sync_table	= ixgbe_udp_tunnel_sync,
	.flags		= UDP_TUNNEL_NIC_INFO_IPV4_ONLY,
	.tables		= {
		{ .n_entries = 1, .tunnel_types = UDP_TUNNEL_TYPE_VXLAN,  },
		{ .n_entries = 1, .tunnel_types = UDP_TUNNEL_TYPE_GENEVE, },
	},
};

#ifdef CONFIG_IXGBE_DCB
/**
 * ixgbe_configure_dcb - Configure DCB hardware
 * @adapter: ixgbe adapter struct
 *
 * This is called by the driver on open to configure the DCB hardware.
 * This is also called by the gennetlink interface when reconfiguring
 * the DCB state.
 */
static void ixgbe_configure_dcb(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int max_frame = adapter->netdev->mtu + ETH_HLEN + ETH_FCS_LEN;

	if (!(adapter->flags & IXGBE_FLAG_DCB_ENABLED)) {
		if (hw->mac.type == ixgbe_mac_82598EB)
			netif_set_tso_max_size(adapter->netdev, 65536);
		return;
	}

	if (hw->mac.type == ixgbe_mac_82598EB)
		netif_set_tso_max_size(adapter->netdev, 32768);

#ifdef IXGBE_FCOE
	if (adapter->netdev->fcoe_mtu)
		max_frame = max(max_frame, IXGBE_FCOE_JUMBO_FRAME_SIZE);
#endif

	/* reconfigure the hardware */
	if (adapter->dcbx_cap & DCB_CAP_DCBX_VER_CEE) {
		ixgbe_dcb_calculate_tc_credits(hw, &adapter->dcb_cfg, max_frame,
						DCB_TX_CONFIG);
		ixgbe_dcb_calculate_tc_credits(hw, &adapter->dcb_cfg, max_frame,
						DCB_RX_CONFIG);
		ixgbe_dcb_hw_config(hw, &adapter->dcb_cfg);
	} else if (adapter->ixgbe_ieee_ets && adapter->ixgbe_ieee_pfc) {
		ixgbe_dcb_hw_ets(&adapter->hw,
				 adapter->ixgbe_ieee_ets,
				 max_frame);
		ixgbe_dcb_hw_pfc_config(&adapter->hw,
					adapter->ixgbe_ieee_pfc->pfc_en,
					adapter->ixgbe_ieee_ets->prio_tc);
	}

	/* Enable RSS Hash per TC */
	if (hw->mac.type != ixgbe_mac_82598EB) {
		u32 msb = 0;
		u16 rss_i = adapter->ring_feature[RING_F_RSS].indices - 1;

		while (rss_i) {
			msb++;
			rss_i >>= 1;
		}

		/* write msb to all 8 TCs in one write */
		IXGBE_WRITE_REG(hw, IXGBE_RQTC, msb * 0x11111111);
	}
}
#endif

/* Additional bittime to account for IXGBE framing */
#define IXGBE_ETH_FRAMING 20

/**
 * ixgbe_hpbthresh - calculate high water mark for flow control
 *
 * @adapter: board private structure to calculate for
 * @pb: packet buffer to calculate
 */
static int ixgbe_hpbthresh(struct ixgbe_adapter *adapter, int pb)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct net_device *dev = adapter->netdev;
	int link, tc, kb, marker;
	u32 dv_id, rx_pba;

	/* Calculate max LAN frame size */
	tc = link = dev->mtu + ETH_HLEN + ETH_FCS_LEN + IXGBE_ETH_FRAMING;

#ifdef IXGBE_FCOE
	/* FCoE traffic class uses FCOE jumbo frames */
	if (dev->fcoe_mtu && tc < IXGBE_FCOE_JUMBO_FRAME_SIZE &&
	    (pb == ixgbe_fcoe_get_tc(adapter)))
		tc = IXGBE_FCOE_JUMBO_FRAME_SIZE;
#endif

	/* Calculate delay value for device */
	switch (hw->mac.type) {
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		dv_id = IXGBE_DV_X540(link, tc);
		break;
	default:
		dv_id = IXGBE_DV(link, tc);
		break;
	}

	/* Loopback switch introduces additional latency */
	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)
		dv_id += IXGBE_B2BT(tc);

	/* Delay value is calculated in bit times convert to KB */
	kb = IXGBE_BT2KB(dv_id);
	rx_pba = IXGBE_READ_REG(hw, IXGBE_RXPBSIZE(pb)) >> 10;

	marker = rx_pba - kb;

	/* It is possible that the packet buffer is not large enough
	 * to provide required headroom. In this case throw an error
	 * to user and a do the best we can.
	 */
	if (marker < 0) {
		e_warn(drv, "Packet Buffer(%i) can not provide enough"
			    "headroom to support flow control."
			    "Decrease MTU or number of traffic classes\n", pb);
		marker = tc + 1;
	}

	return marker;
}

/**
 * ixgbe_lpbthresh - calculate low water mark for flow control
 *
 * @adapter: board private structure to calculate for
 * @pb: packet buffer to calculate
 */
static int ixgbe_lpbthresh(struct ixgbe_adapter *adapter, int pb)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct net_device *dev = adapter->netdev;
	int tc;
	u32 dv_id;

	/* Calculate max LAN frame size */
	tc = dev->mtu + ETH_HLEN + ETH_FCS_LEN;

#ifdef IXGBE_FCOE
	/* FCoE traffic class uses FCOE jumbo frames */
	if (dev->fcoe_mtu && tc < IXGBE_FCOE_JUMBO_FRAME_SIZE &&
	    (pb == netdev_get_prio_tc_map(dev, adapter->fcoe.up)))
		tc = IXGBE_FCOE_JUMBO_FRAME_SIZE;
#endif

	/* Calculate delay value for device */
	switch (hw->mac.type) {
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		dv_id = IXGBE_LOW_DV_X540(tc);
		break;
	default:
		dv_id = IXGBE_LOW_DV(tc);
		break;
	}

	/* Delay value is calculated in bit times convert to KB */
	return IXGBE_BT2KB(dv_id);
}

/*
 * ixgbe_pbthresh_setup - calculate and setup high low water marks
 */
static void ixgbe_pbthresh_setup(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int num_tc = adapter->hw_tcs;
	int i;

	if (!num_tc)
		num_tc = 1;

	for (i = 0; i < num_tc; i++) {
		hw->fc.high_water[i] = ixgbe_hpbthresh(adapter, i);
		hw->fc.low_water[i] = ixgbe_lpbthresh(adapter, i);

		/* Low water marks must not be larger than high water marks */
		if (hw->fc.low_water[i] > hw->fc.high_water[i])
			hw->fc.low_water[i] = 0;
	}

	for (; i < MAX_TRAFFIC_CLASS; i++)
		hw->fc.high_water[i] = 0;
}

static void ixgbe_configure_pb(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int hdrm;
	u8 tc = adapter->hw_tcs;

	if (adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE ||
	    adapter->flags & IXGBE_FLAG_FDIR_PERFECT_CAPABLE)
		hdrm = 32 << adapter->fdir_pballoc;
	else
		hdrm = 0;

	hw->mac.ops.set_rxpba(hw, tc, hdrm, PBA_STRATEGY_EQUAL);
	ixgbe_pbthresh_setup(adapter);
}

static void ixgbe_fdir_filter_restore(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct hlist_node *node2;
	struct ixgbe_fdir_filter *filter;
	u8 queue;

	spin_lock(&adapter->fdir_perfect_lock);

	if (!hlist_empty(&adapter->fdir_filter_list))
		ixgbe_fdir_set_input_mask_82599(hw, &adapter->fdir_mask);

	hlist_for_each_entry_safe(filter, node2,
				  &adapter->fdir_filter_list, fdir_node) {
		if (filter->action == IXGBE_FDIR_DROP_QUEUE) {
			queue = IXGBE_FDIR_DROP_QUEUE;
		} else {
			u32 ring = ethtool_get_flow_spec_ring(filter->action);
			u8 vf = ethtool_get_flow_spec_ring_vf(filter->action);

			if (!vf && (ring >= adapter->num_rx_queues)) {
				e_err(drv, "FDIR restore failed without VF, ring: %u\n",
				      ring);
				continue;
			} else if (vf &&
				   ((vf > adapter->num_vfs) ||
				     ring >= adapter->num_rx_queues_per_pool)) {
				e_err(drv, "FDIR restore failed with VF, vf: %hhu, ring: %u\n",
				      vf, ring);
				continue;
			}

			/* Map the ring onto the absolute queue index */
			if (!vf)
				queue = adapter->rx_ring[ring]->reg_idx;
			else
				queue = ((vf - 1) *
					adapter->num_rx_queues_per_pool) + ring;
		}

		ixgbe_fdir_write_perfect_filter_82599(hw,
				&filter->filter, filter->sw_idx, queue);
	}

	spin_unlock(&adapter->fdir_perfect_lock);
}

/**
 * ixgbe_clean_rx_ring - Free Rx Buffers per Queue
 * @rx_ring: ring to free buffers from
 **/
static void ixgbe_clean_rx_ring(struct ixgbe_ring *rx_ring)
{
	u16 i = rx_ring->next_to_clean;
	struct ixgbe_rx_buffer *rx_buffer = &rx_ring->rx_buffer_info[i];

	if (rx_ring->xsk_pool) {
		ixgbe_xsk_clean_rx_ring(rx_ring);
		goto skip_free;
	}

	/* Free all the Rx ring sk_buffs */
	while (i != rx_ring->next_to_alloc) {
		if (rx_buffer->skb) {
			struct sk_buff *skb = rx_buffer->skb;
			if (IXGBE_CB(skb)->page_released)
				dma_unmap_page_attrs(rx_ring->dev,
						     IXGBE_CB(skb)->dma,
						     ixgbe_rx_pg_size(rx_ring),
						     DMA_FROM_DEVICE,
						     IXGBE_RX_DMA_ATTR);
			dev_kfree_skb(skb);
		}

		/* Invalidate cache lines that may have been written to by
		 * device so that we avoid corrupting memory.
		 */
		dma_sync_single_range_for_cpu(rx_ring->dev,
					      rx_buffer->dma,
					      rx_buffer->page_offset,
					      ixgbe_rx_bufsz(rx_ring),
					      DMA_FROM_DEVICE);

		/* free resources associated with mapping */
		dma_unmap_page_attrs(rx_ring->dev, rx_buffer->dma,
				     ixgbe_rx_pg_size(rx_ring),
				     DMA_FROM_DEVICE,
				     IXGBE_RX_DMA_ATTR);
		__page_frag_cache_drain(rx_buffer->page,
					rx_buffer->pagecnt_bias);

		i++;
		rx_buffer++;
		if (i == rx_ring->count) {
			i = 0;
			rx_buffer = rx_ring->rx_buffer_info;
		}
	}

skip_free:
	rx_ring->next_to_alloc = 0;
	rx_ring->next_to_clean = 0;
	rx_ring->next_to_use = 0;
}

static int ixgbe_fwd_ring_up(struct ixgbe_adapter *adapter,
			     struct ixgbe_fwd_adapter *accel)
{
	u16 rss_i = adapter->ring_feature[RING_F_RSS].indices;
	int num_tc = netdev_get_num_tc(adapter->netdev);
	struct net_device *vdev = accel->netdev;
	int i, baseq, err;

	baseq = accel->pool * adapter->num_rx_queues_per_pool;
	netdev_dbg(vdev, "pool %i:%i queues %i:%i\n",
		   accel->pool, adapter->num_rx_pools,
		   baseq, baseq + adapter->num_rx_queues_per_pool);

	accel->rx_base_queue = baseq;
	accel->tx_base_queue = baseq;

	/* record configuration for macvlan interface in vdev */
	for (i = 0; i < num_tc; i++)
		netdev_bind_sb_channel_queue(adapter->netdev, vdev,
					     i, rss_i, baseq + (rss_i * i));

	for (i = 0; i < adapter->num_rx_queues_per_pool; i++)
		adapter->rx_ring[baseq + i]->netdev = vdev;

	/* Guarantee all rings are updated before we update the
	 * MAC address filter.
	 */
	wmb();

	/* ixgbe_add_mac_filter will return an index if it succeeds, so we
	 * need to only treat it as an error value if it is negative.
	 */
	err = ixgbe_add_mac_filter(adapter, vdev->dev_addr,
				   VMDQ_P(accel->pool));
	if (err >= 0)
		return 0;

	/* if we cannot add the MAC rule then disable the offload */
	macvlan_release_l2fw_offload(vdev);

	for (i = 0; i < adapter->num_rx_queues_per_pool; i++)
		adapter->rx_ring[baseq + i]->netdev = NULL;

	netdev_err(vdev, "L2FW offload disabled due to L2 filter error\n");

	/* unbind the queues and drop the subordinate channel config */
	netdev_unbind_sb_channel(adapter->netdev, vdev);
	netdev_set_sb_channel(vdev, 0);

	clear_bit(accel->pool, adapter->fwd_bitmask);
	kfree(accel);

	return err;
}

static int ixgbe_macvlan_up(struct net_device *vdev,
			    struct netdev_nested_priv *priv)
{
	struct ixgbe_adapter *adapter = (struct ixgbe_adapter *)priv->data;
	struct ixgbe_fwd_adapter *accel;

	if (!netif_is_macvlan(vdev))
		return 0;

	accel = macvlan_accel_priv(vdev);
	if (!accel)
		return 0;

	ixgbe_fwd_ring_up(adapter, accel);

	return 0;
}

static void ixgbe_configure_dfwd(struct ixgbe_adapter *adapter)
{
	struct netdev_nested_priv priv = {
		.data = (void *)adapter,
	};

	netdev_walk_all_upper_dev_rcu(adapter->netdev,
				      ixgbe_macvlan_up, &priv);
}

static void ixgbe_configure(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;

	ixgbe_configure_pb(adapter);
#ifdef CONFIG_IXGBE_DCB
	ixgbe_configure_dcb(adapter);
#endif
	/*
	 * We must restore virtualization before VLANs or else
	 * the VLVF registers will not be populated
	 */
	ixgbe_configure_virtualization(adapter);

	ixgbe_set_rx_mode(adapter->netdev);
	ixgbe_restore_vlan(adapter);
	ixgbe_ipsec_restore(adapter);

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
		hw->mac.ops.disable_rx_buff(hw);
		break;
	default:
		break;
	}

	if (adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE) {
		ixgbe_init_fdir_signature_82599(&adapter->hw,
						adapter->fdir_pballoc);
	} else if (adapter->flags & IXGBE_FLAG_FDIR_PERFECT_CAPABLE) {
		ixgbe_init_fdir_perfect_82599(&adapter->hw,
					      adapter->fdir_pballoc);
		ixgbe_fdir_filter_restore(adapter);
	}

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
		hw->mac.ops.enable_rx_buff(hw);
		break;
	default:
		break;
	}

#ifdef CONFIG_IXGBE_DCA
	/* configure DCA */
	if (adapter->flags & IXGBE_FLAG_DCA_CAPABLE)
		ixgbe_setup_dca(adapter);
#endif /* CONFIG_IXGBE_DCA */

#ifdef IXGBE_FCOE
	/* configure FCoE L2 filters, redirection table, and Rx control */
	ixgbe_configure_fcoe(adapter);

#endif /* IXGBE_FCOE */
	ixgbe_configure_tx(adapter);
	ixgbe_configure_rx(adapter);
	ixgbe_configure_dfwd(adapter);
}

/**
 * ixgbe_enable_link_status_events - enable link status events
 * @adapter: pointer to the adapter structure
 * @mask: event mask to be set
 *
 * Enables link status events by invoking ixgbe_configure_lse()
 *
 * Return: the exit code of the operation.
 */
static int ixgbe_enable_link_status_events(struct ixgbe_adapter *adapter,
					   u16 mask)
{
	int err;

	err = ixgbe_configure_lse(&adapter->hw, true, mask);
	if (err)
		return err;

	adapter->lse_mask = mask;
	return 0;
}

/**
 * ixgbe_disable_link_status_events - disable link status events
 * @adapter: pointer to the adapter structure
 *
 * Disables link status events by invoking ixgbe_configure_lse()
 *
 * Return: the exit code of the operation.
 */
static int ixgbe_disable_link_status_events(struct ixgbe_adapter *adapter)
{
	int err;

	err = ixgbe_configure_lse(&adapter->hw, false, adapter->lse_mask);
	if (err)
		return err;

	adapter->lse_mask = 0;
	return 0;
}

/**
 * ixgbe_sfp_link_config - set up SFP+ link
 * @adapter: pointer to private adapter struct
 **/
static void ixgbe_sfp_link_config(struct ixgbe_adapter *adapter)
{
	/*
	 * We are assuming the worst case scenario here, and that
	 * is that an SFP was inserted/removed after the reset
	 * but before SFP detection was enabled.  As such the best
	 * solution is to just start searching as soon as we start
	 */
	if (adapter->hw.mac.type == ixgbe_mac_82598EB)
		adapter->flags2 |= IXGBE_FLAG2_SEARCH_FOR_SFP;

	adapter->flags2 |= IXGBE_FLAG2_SFP_NEEDS_RESET;
	adapter->sfp_poll_time = 0;
}

/**
 * ixgbe_non_sfp_link_config - set up non-SFP+ link
 * @hw: pointer to private hardware struct
 *
 * Configure non-SFP link.
 *
 * Return: 0 on success, negative on failure
 **/
static int ixgbe_non_sfp_link_config(struct ixgbe_hw *hw)
{
	struct ixgbe_adapter *adapter = container_of(hw, struct ixgbe_adapter,
						     hw);
	u16 mask = ~((u16)(IXGBE_ACI_LINK_EVENT_UPDOWN |
			   IXGBE_ACI_LINK_EVENT_MEDIA_NA |
			   IXGBE_ACI_LINK_EVENT_MODULE_QUAL_FAIL |
			   IXGBE_ACI_LINK_EVENT_PHY_FW_LOAD_FAIL));
	bool autoneg, link_up = false;
	int ret = -EIO;
	u32 speed;

	if (hw->mac.ops.check_link)
		ret = hw->mac.ops.check_link(hw, &speed, &link_up, false);

	if (ret)
		return ret;

	speed = hw->phy.autoneg_advertised;
	if (!speed && hw->mac.ops.get_link_capabilities) {
		ret = hw->mac.ops.get_link_capabilities(hw, &speed,
							&autoneg);
		/* remove NBASE-T speeds from default autonegotiation
		 * to accommodate broken network switches in the field
		 * which cannot cope with advertised NBASE-T speeds
		 */
		speed &= ~(IXGBE_LINK_SPEED_5GB_FULL |
			   IXGBE_LINK_SPEED_2_5GB_FULL);
	}

	if (ret)
		return ret;

	if (hw->mac.ops.setup_link) {
		if (adapter->hw.mac.type == ixgbe_mac_e610) {
			ret = ixgbe_enable_link_status_events(adapter, mask);
			if (ret)
				return ret;
		}
		ret = hw->mac.ops.setup_link(hw, speed, link_up);
	}

	return ret;
}

/**
 * ixgbe_check_media_subtask - check for media
 * @adapter: pointer to adapter structure
 *
 * If media is available then initialize PHY user configuration. Configure the
 * PHY if the interface is up.
 */
static void ixgbe_check_media_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;

	/* No need to check for media if it's already present */
	if (!(adapter->flags2 & IXGBE_FLAG2_NO_MEDIA))
		return;

	/* Refresh link info and check if media is present */
	if (ixgbe_update_link_info(hw))
		return;

	ixgbe_check_link_cfg_err(adapter, hw->link.link_info.link_cfg_err);

	if (hw->link.link_info.link_info & IXGBE_ACI_MEDIA_AVAILABLE) {
		/* PHY settings are reset on media insertion, reconfigure
		 * PHY to preserve settings.
		 */
		if (!(ixgbe_non_sfp_link_config(&adapter->hw)))
			adapter->flags2 &= ~IXGBE_FLAG2_NO_MEDIA;

		/* A Link Status Event will be generated; the event handler
		 * will complete bringing the interface up
		 */
	}
}

/**
 * ixgbe_clear_vf_stats_counters - Clear out VF stats after reset
 * @adapter: board private structure
 *
 * On a reset we need to clear out the VF stats or accounting gets
 * messed up because they're not clear on read.
 **/
static void ixgbe_clear_vf_stats_counters(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	for (i = 0; i < adapter->num_vfs; i++) {
		adapter->vfinfo[i].last_vfstats.gprc =
			IXGBE_READ_REG(hw, IXGBE_PVFGPRC(i));
		adapter->vfinfo[i].saved_rst_vfstats.gprc +=
			adapter->vfinfo[i].vfstats.gprc;
		adapter->vfinfo[i].vfstats.gprc = 0;
		adapter->vfinfo[i].last_vfstats.gptc =
			IXGBE_READ_REG(hw, IXGBE_PVFGPTC(i));
		adapter->vfinfo[i].saved_rst_vfstats.gptc +=
			adapter->vfinfo[i].vfstats.gptc;
		adapter->vfinfo[i].vfstats.gptc = 0;
		adapter->vfinfo[i].last_vfstats.gorc =
			IXGBE_READ_REG(hw, IXGBE_PVFGORC_LSB(i));
		adapter->vfinfo[i].saved_rst_vfstats.gorc +=
			adapter->vfinfo[i].vfstats.gorc;
		adapter->vfinfo[i].vfstats.gorc = 0;
		adapter->vfinfo[i].last_vfstats.gotc =
			IXGBE_READ_REG(hw, IXGBE_PVFGOTC_LSB(i));
		adapter->vfinfo[i].saved_rst_vfstats.gotc +=
			adapter->vfinfo[i].vfstats.gotc;
		adapter->vfinfo[i].vfstats.gotc = 0;
		adapter->vfinfo[i].last_vfstats.mprc =
			IXGBE_READ_REG(hw, IXGBE_PVFMPRC(i));
		adapter->vfinfo[i].saved_rst_vfstats.mprc +=
			adapter->vfinfo[i].vfstats.mprc;
		adapter->vfinfo[i].vfstats.mprc = 0;
	}
}

static void ixgbe_setup_gpie(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 gpie = 0;

	if (adapter->flags & IXGBE_FLAG_MSIX_ENABLED) {
		gpie = IXGBE_GPIE_MSIX_MODE | IXGBE_GPIE_PBA_SUPPORT |
		       IXGBE_GPIE_OCD;
		gpie |= IXGBE_GPIE_EIAME;
		/*
		 * use EIAM to auto-mask when MSI-X interrupt is asserted
		 * this saves a register write for every interrupt
		 */
		switch (hw->mac.type) {
		case ixgbe_mac_82598EB:
			IXGBE_WRITE_REG(hw, IXGBE_EIAM, IXGBE_EICS_RTX_QUEUE);
			break;
		case ixgbe_mac_82599EB:
		case ixgbe_mac_X540:
		case ixgbe_mac_X550:
		case ixgbe_mac_X550EM_x:
		case ixgbe_mac_x550em_a:
		case ixgbe_mac_e610:
		default:
			IXGBE_WRITE_REG(hw, IXGBE_EIAM_EX(0), 0xFFFFFFFF);
			IXGBE_WRITE_REG(hw, IXGBE_EIAM_EX(1), 0xFFFFFFFF);
			break;
		}
	} else {
		/* legacy interrupts, use EIAM to auto-mask when reading EICR,
		 * specifically only auto mask tx and rx interrupts */
		IXGBE_WRITE_REG(hw, IXGBE_EIAM, IXGBE_EICS_RTX_QUEUE);
	}

	/* XXX: to interrupt immediately for EICS writes, enable this */
	/* gpie |= IXGBE_GPIE_EIMEN; */

	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) {
		gpie &= ~IXGBE_GPIE_VTMODE_MASK;

		switch (adapter->ring_feature[RING_F_VMDQ].mask) {
		case IXGBE_82599_VMDQ_8Q_MASK:
			gpie |= IXGBE_GPIE_VTMODE_16;
			break;
		case IXGBE_82599_VMDQ_4Q_MASK:
			gpie |= IXGBE_GPIE_VTMODE_32;
			break;
		default:
			gpie |= IXGBE_GPIE_VTMODE_64;
			break;
		}
	}

	/* Enable Thermal over heat sensor interrupt */
	if (adapter->flags2 & IXGBE_FLAG2_TEMP_SENSOR_CAPABLE) {
		switch (adapter->hw.mac.type) {
		case ixgbe_mac_82599EB:
			gpie |= IXGBE_SDP0_GPIEN_8259X;
			break;
		default:
			break;
		}
	}

	/* Enable fan failure interrupt */
	if (adapter->flags & IXGBE_FLAG_FAN_FAIL_CAPABLE)
		gpie |= IXGBE_SDP1_GPIEN(hw);

	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
		gpie |= IXGBE_SDP1_GPIEN_8259X | IXGBE_SDP2_GPIEN_8259X;
		break;
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
		gpie |= IXGBE_SDP0_GPIEN_X540;
		break;
	default:
		break;
	}

	IXGBE_WRITE_REG(hw, IXGBE_GPIE, gpie);
}

static void ixgbe_up_complete(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int err;
	u32 ctrl_ext;

	ixgbe_get_hw_control(adapter);
	ixgbe_setup_gpie(adapter);

	if (adapter->flags & IXGBE_FLAG_MSIX_ENABLED)
		ixgbe_configure_msix(adapter);
	else
		ixgbe_configure_msi_and_legacy(adapter);

	/* enable the optics for 82599 SFP+ fiber */
	if (hw->mac.ops.enable_tx_laser)
		hw->mac.ops.enable_tx_laser(hw);

	if (hw->phy.ops.set_phy_power)
		hw->phy.ops.set_phy_power(hw, true);

	smp_mb__before_atomic();
	clear_bit(__IXGBE_DOWN, &adapter->state);
	ixgbe_napi_enable_all(adapter);

	if (ixgbe_is_sfp(hw)) {
		ixgbe_sfp_link_config(adapter);
	} else {
		err = ixgbe_non_sfp_link_config(hw);
		if (err)
			e_err(probe, "link_config FAILED %d\n", err);
	}

	/* clear any pending interrupts, may auto mask */
	IXGBE_READ_REG(hw, IXGBE_EICR);
	ixgbe_irq_enable(adapter, true, true);

	/*
	 * If this adapter has a fan, check to see if we had a failure
	 * before we enabled the interrupt.
	 */
	if (adapter->flags & IXGBE_FLAG_FAN_FAIL_CAPABLE) {
		u32 esdp = IXGBE_READ_REG(hw, IXGBE_ESDP);
		if (esdp & IXGBE_ESDP_SDP1)
			e_crit(drv, "Fan has stopped, replace the adapter\n");
	}

	/* bring the link up in the watchdog, this could race with our first
	 * link up interrupt but shouldn't be a problem */
	adapter->flags |= IXGBE_FLAG_NEED_LINK_UPDATE;
	adapter->link_check_timeout = jiffies;
	mod_timer(&adapter->service_timer, jiffies);

	ixgbe_clear_vf_stats_counters(adapter);
	/* Set PF Reset Done bit so PF/VF Mail Ops can work */
	ctrl_ext = IXGBE_READ_REG(hw, IXGBE_CTRL_EXT);
	ctrl_ext |= IXGBE_CTRL_EXT_PFRSTD;
	IXGBE_WRITE_REG(hw, IXGBE_CTRL_EXT, ctrl_ext);

	/* update setting rx tx for all active vfs */
	ixgbe_set_all_vfs(adapter);
}

void ixgbe_reinit_locked(struct ixgbe_adapter *adapter)
{
	/* put off any impending NetWatchDogTimeout */
	netif_trans_update(adapter->netdev);

	while (test_and_set_bit(__IXGBE_RESETTING, &adapter->state))
		usleep_range(1000, 2000);
	if (adapter->hw.phy.type == ixgbe_phy_fw)
		ixgbe_watchdog_link_is_down(adapter);
	ixgbe_down(adapter);
	/*
	 * If SR-IOV enabled then wait a bit before bringing the adapter
	 * back up to give the VFs time to respond to the reset.  The
	 * two second wait is based upon the watchdog timer cycle in
	 * the VF driver.
	 */
	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)
		msleep(2000);
	ixgbe_up(adapter);
	clear_bit(__IXGBE_RESETTING, &adapter->state);
}

void ixgbe_up(struct ixgbe_adapter *adapter)
{
	/* hardware has been reset, we need to reload some things */
	ixgbe_configure(adapter);

	ixgbe_up_complete(adapter);
}

static unsigned long ixgbe_get_completion_timeout(struct ixgbe_adapter *adapter)
{
	u16 devctl2;

	pcie_capability_read_word(adapter->pdev, PCI_EXP_DEVCTL2, &devctl2);

	switch (devctl2 & IXGBE_PCIDEVCTRL2_TIMEO_MASK) {
	case IXGBE_PCIDEVCTRL2_17_34s:
	case IXGBE_PCIDEVCTRL2_4_8s:
		/* For now we cap the upper limit on delay to 2 seconds
		 * as we end up going up to 34 seconds of delay in worst
		 * case timeout value.
		 */
	case IXGBE_PCIDEVCTRL2_1_2s:
		return 2000000ul;	/* 2.0 s */
	case IXGBE_PCIDEVCTRL2_260_520ms:
		return 520000ul;	/* 520 ms */
	case IXGBE_PCIDEVCTRL2_65_130ms:
		return 130000ul;	/* 130 ms */
	case IXGBE_PCIDEVCTRL2_16_32ms:
		return 32000ul;		/* 32 ms */
	case IXGBE_PCIDEVCTRL2_1_2ms:
		return 2000ul;		/* 2 ms */
	case IXGBE_PCIDEVCTRL2_50_100us:
		return 100ul;		/* 100 us */
	case IXGBE_PCIDEVCTRL2_16_32ms_def:
		return 32000ul;		/* 32 ms */
	default:
		break;
	}

	/* We shouldn't need to hit this path, but just in case default as
	 * though completion timeout is not supported and support 32ms.
	 */
	return 32000ul;
}

void ixgbe_disable_rx(struct ixgbe_adapter *adapter)
{
	unsigned long wait_delay, delay_interval;
	struct ixgbe_hw *hw = &adapter->hw;
	int i, wait_loop;
	u32 rxdctl;

	/* disable receives */
	hw->mac.ops.disable_rx(hw);

	if (ixgbe_removed(hw->hw_addr))
		return;

	/* disable all enabled Rx queues */
	for (i = 0; i < adapter->num_rx_queues; i++) {
		struct ixgbe_ring *ring = adapter->rx_ring[i];
		u8 reg_idx = ring->reg_idx;

		rxdctl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg_idx));
		rxdctl &= ~IXGBE_RXDCTL_ENABLE;
		rxdctl |= IXGBE_RXDCTL_SWFLSH;

		/* write value back with RXDCTL.ENABLE bit cleared */
		IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(reg_idx), rxdctl);
	}

	/* RXDCTL.EN may not change on 82598 if link is down, so skip it */
	if (hw->mac.type == ixgbe_mac_82598EB &&
	    !(IXGBE_READ_REG(hw, IXGBE_LINKS) & IXGBE_LINKS_UP))
		return;

	/* Determine our minimum delay interval. We will increase this value
	 * with each subsequent test. This way if the device returns quickly
	 * we should spend as little time as possible waiting, however as
	 * the time increases we will wait for larger periods of time.
	 *
	 * The trick here is that we increase the interval using the
	 * following pattern: 1x 3x 5x 7x 9x 11x 13x 15x 17x 19x. The result
	 * of that wait is that it totals up to 100x whatever interval we
	 * choose. Since our minimum wait is 100us we can just divide the
	 * total timeout by 100 to get our minimum delay interval.
	 */
	delay_interval = ixgbe_get_completion_timeout(adapter) / 100;

	wait_loop = IXGBE_MAX_RX_DESC_POLL;
	wait_delay = delay_interval;

	while (wait_loop--) {
		usleep_range(wait_delay, wait_delay + 10);
		wait_delay += delay_interval * 2;
		rxdctl = 0;

		/* OR together the reading of all the active RXDCTL registers,
		 * and then test the result. We need the disable to complete
		 * before we start freeing the memory and invalidating the
		 * DMA mappings.
		 */
		for (i = 0; i < adapter->num_rx_queues; i++) {
			struct ixgbe_ring *ring = adapter->rx_ring[i];
			u8 reg_idx = ring->reg_idx;

			rxdctl |= IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg_idx));
		}

		if (!(rxdctl & IXGBE_RXDCTL_ENABLE))
			return;
	}

	e_err(drv,
	      "RXDCTL.ENABLE for one or more queues not cleared within the polling period\n");
}

void ixgbe_disable_tx(struct ixgbe_adapter *adapter)
{
	unsigned long wait_delay, delay_interval;
	struct ixgbe_hw *hw = &adapter->hw;
	int i, wait_loop;
	u32 txdctl;

	if (ixgbe_removed(hw->hw_addr))
		return;

	/* disable all enabled Tx queues */
	for (i = 0; i < adapter->num_tx_queues; i++) {
		struct ixgbe_ring *ring = adapter->tx_ring[i];
		u8 reg_idx = ring->reg_idx;

		IXGBE_WRITE_REG(hw, IXGBE_TXDCTL(reg_idx), IXGBE_TXDCTL_SWFLSH);
	}

	/* disable all enabled XDP Tx queues */
	for (i = 0; i < adapter->num_xdp_queues; i++) {
		struct ixgbe_ring *ring = adapter->xdp_ring[i];
		u8 reg_idx = ring->reg_idx;

		IXGBE_WRITE_REG(hw, IXGBE_TXDCTL(reg_idx), IXGBE_TXDCTL_SWFLSH);
	}

	/* If the link is not up there shouldn't be much in the way of
	 * pending transactions. Those that are left will be flushed out
	 * when the reset logic goes through the flush sequence to clean out
	 * the pending Tx transactions.
	 */
	if (!(IXGBE_READ_REG(hw, IXGBE_LINKS) & IXGBE_LINKS_UP))
		goto dma_engine_disable;

	/* Determine our minimum delay interval. We will increase this value
	 * with each subsequent test. This way if the device returns quickly
	 * we should spend as little time as possible waiting, however as
	 * the time increases we will wait for larger periods of time.
	 *
	 * The trick here is that we increase the interval using the
	 * following pattern: 1x 3x 5x 7x 9x 11x 13x 15x 17x 19x. The result
	 * of that wait is that it totals up to 100x whatever interval we
	 * choose. Since our minimum wait is 100us we can just divide the
	 * total timeout by 100 to get our minimum delay interval.
	 */
	delay_interval = ixgbe_get_completion_timeout(adapter) / 100;

	wait_loop = IXGBE_MAX_RX_DESC_POLL;
	wait_delay = delay_interval;

	while (wait_loop--) {
		usleep_range(wait_delay, wait_delay + 10);
		wait_delay += delay_interval * 2;
		txdctl = 0;

		/* OR together the reading of all the active TXDCTL registers,
		 * and then test the result. We need the disable to complete
		 * before we start freeing the memory and invalidating the
		 * DMA mappings.
		 */
		for (i = 0; i < adapter->num_tx_queues; i++) {
			struct ixgbe_ring *ring = adapter->tx_ring[i];
			u8 reg_idx = ring->reg_idx;

			txdctl |= IXGBE_READ_REG(hw, IXGBE_TXDCTL(reg_idx));
		}
		for (i = 0; i < adapter->num_xdp_queues; i++) {
			struct ixgbe_ring *ring = adapter->xdp_ring[i];
			u8 reg_idx = ring->reg_idx;

			txdctl |= IXGBE_READ_REG(hw, IXGBE_TXDCTL(reg_idx));
		}

		if (!(txdctl & IXGBE_TXDCTL_ENABLE))
			goto dma_engine_disable;
	}

	e_err(drv,
	      "TXDCTL.ENABLE for one or more queues not cleared within the polling period\n");

dma_engine_disable:
	/* Disable the Tx DMA engine on 82599 and later MAC */
	switch (hw->mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		IXGBE_WRITE_REG(hw, IXGBE_DMATXCTL,
				(IXGBE_READ_REG(hw, IXGBE_DMATXCTL) &
				 ~IXGBE_DMATXCTL_TE));
		fallthrough;
	default:
		break;
	}
}

void ixgbe_reset(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct net_device *netdev = adapter->netdev;
	int err;

	if (ixgbe_removed(hw->hw_addr))
		return;
	/* lock SFP init bit to prevent race conditions with the watchdog */
	while (test_and_set_bit(__IXGBE_IN_SFP_INIT, &adapter->state))
		usleep_range(1000, 2000);

	/* clear all SFP and link config related flags while holding SFP_INIT */
	adapter->flags2 &= ~(IXGBE_FLAG2_SEARCH_FOR_SFP |
			     IXGBE_FLAG2_SFP_NEEDS_RESET);
	adapter->flags &= ~IXGBE_FLAG_NEED_LINK_CONFIG;

	err = hw->mac.ops.init_hw(hw);
	switch (err) {
	case 0:
	case -ENOENT:
	case -EOPNOTSUPP:
		break;
	case -EALREADY:
		e_dev_err("primary disable timed out\n");
		break;
	case -EACCES:
		/* We are running on a pre-production device, log a warning */
		e_dev_warn("This device is a pre-production adapter/LOM. "
			   "Please be aware there may be issues associated with "
			   "your hardware.  If you are experiencing problems "
			   "please contact your Intel or hardware "
			   "representative who provided you with this "
			   "hardware.\n");
		break;
	default:
		e_dev_err("Hardware Error: %d\n", err);
	}

	clear_bit(__IXGBE_IN_SFP_INIT, &adapter->state);

	/* flush entries out of MAC table */
	ixgbe_flush_sw_mac_table(adapter);
	__dev_uc_unsync(netdev, NULL);

	/* do not flush user set addresses */
	ixgbe_mac_set_default_filter(adapter);

	/* update SAN MAC vmdq pool selection */
	if (hw->mac.san_mac_rar_index)
		hw->mac.ops.set_vmdq_san_mac(hw, VMDQ_P(0));

	if (test_bit(__IXGBE_PTP_RUNNING, &adapter->state))
		ixgbe_ptp_reset(adapter);

	if (hw->phy.ops.set_phy_power) {
		if (!netif_running(adapter->netdev) && !adapter->wol)
			hw->phy.ops.set_phy_power(hw, false);
		else
			hw->phy.ops.set_phy_power(hw, true);
	}
}

/**
 * ixgbe_clean_tx_ring - Free Tx Buffers
 * @tx_ring: ring to be cleaned
 **/
static void ixgbe_clean_tx_ring(struct ixgbe_ring *tx_ring)
{
	u16 i = tx_ring->next_to_clean;
	struct ixgbe_tx_buffer *tx_buffer = &tx_ring->tx_buffer_info[i];

	if (tx_ring->xsk_pool) {
		ixgbe_xsk_clean_tx_ring(tx_ring);
		goto out;
	}

	while (i != tx_ring->next_to_use) {
		union ixgbe_adv_tx_desc *eop_desc, *tx_desc;

		/* Free all the Tx ring sk_buffs */
		if (ring_is_xdp(tx_ring))
			xdp_return_frame(tx_buffer->xdpf);
		else
			dev_kfree_skb_any(tx_buffer->skb);

		/* unmap skb header data */
		dma_unmap_single(tx_ring->dev,
				 dma_unmap_addr(tx_buffer, dma),
				 dma_unmap_len(tx_buffer, len),
				 DMA_TO_DEVICE);

		/* check for eop_desc to determine the end of the packet */
		eop_desc = tx_buffer->next_to_watch;
		tx_desc = IXGBE_TX_DESC(tx_ring, i);

		/* unmap remaining buffers */
		while (tx_desc != eop_desc) {
			tx_buffer++;
			tx_desc++;
			i++;
			if (unlikely(i == tx_ring->count)) {
				i = 0;
				tx_buffer = tx_ring->tx_buffer_info;
				tx_desc = IXGBE_TX_DESC(tx_ring, 0);
			}

			/* unmap any remaining paged data */
			if (dma_unmap_len(tx_buffer, len))
				dma_unmap_page(tx_ring->dev,
					       dma_unmap_addr(tx_buffer, dma),
					       dma_unmap_len(tx_buffer, len),
					       DMA_TO_DEVICE);
		}

		/* move us one more past the eop_desc for start of next pkt */
		tx_buffer++;
		i++;
		if (unlikely(i == tx_ring->count)) {
			i = 0;
			tx_buffer = tx_ring->tx_buffer_info;
		}
	}

	/* reset BQL for queue */
	if (!ring_is_xdp(tx_ring))
		netdev_tx_reset_queue(txring_txq(tx_ring));

out:
	/* reset next_to_use and next_to_clean */
	tx_ring->next_to_use = 0;
	tx_ring->next_to_clean = 0;
}

/**
 * ixgbe_clean_all_rx_rings - Free Rx Buffers for all queues
 * @adapter: board private structure
 **/
static void ixgbe_clean_all_rx_rings(struct ixgbe_adapter *adapter)
{
	int i;

	for (i = 0; i < adapter->num_rx_queues; i++)
		ixgbe_clean_rx_ring(adapter->rx_ring[i]);
}

/**
 * ixgbe_clean_all_tx_rings - Free Tx Buffers for all queues
 * @adapter: board private structure
 **/
static void ixgbe_clean_all_tx_rings(struct ixgbe_adapter *adapter)
{
	int i;

	for (i = 0; i < adapter->num_tx_queues; i++)
		ixgbe_clean_tx_ring(adapter->tx_ring[i]);
	for (i = 0; i < adapter->num_xdp_queues; i++)
		ixgbe_clean_tx_ring(adapter->xdp_ring[i]);
}

static void ixgbe_fdir_filter_exit(struct ixgbe_adapter *adapter)
{
	struct hlist_node *node2;
	struct ixgbe_fdir_filter *filter;

	spin_lock(&adapter->fdir_perfect_lock);

	hlist_for_each_entry_safe(filter, node2,
				  &adapter->fdir_filter_list, fdir_node) {
		hlist_del(&filter->fdir_node);
		kfree(filter);
	}
	adapter->fdir_filter_count = 0;

	spin_unlock(&adapter->fdir_perfect_lock);
}

void ixgbe_down(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	/* signal that we are down to the interrupt handler */
	if (test_and_set_bit(__IXGBE_DOWN, &adapter->state))
		return; /* do nothing if already down */

	/* Shut off incoming Tx traffic */
	netif_tx_stop_all_queues(netdev);

	/* call carrier off first to avoid false dev_watchdog timeouts */
	netif_carrier_off(netdev);
	netif_tx_disable(netdev);

	/* Disable Rx */
	ixgbe_disable_rx(adapter);

	/* synchronize_rcu() needed for pending XDP buffers to drain */
	if (adapter->xdp_ring[0])
		synchronize_rcu();

	ixgbe_irq_disable(adapter);

	ixgbe_napi_disable_all(adapter);

	clear_bit(__IXGBE_RESET_REQUESTED, &adapter->state);
	adapter->flags2 &= ~IXGBE_FLAG2_FDIR_REQUIRES_REINIT;
	adapter->flags &= ~IXGBE_FLAG_NEED_LINK_UPDATE;

	timer_delete_sync(&adapter->service_timer);

	if (adapter->num_vfs) {
		/* Clear EITR Select mapping */
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EITRSEL, 0);

		/* Mark all the VFs as inactive */
		for (i = 0 ; i < adapter->num_vfs; i++)
			adapter->vfinfo[i].clear_to_send = false;

		/* update setting rx tx for all active vfs */
		ixgbe_set_all_vfs(adapter);
	}

	/* disable transmits in the hardware now that interrupts are off */
	ixgbe_disable_tx(adapter);

	if (!pci_channel_offline(adapter->pdev))
		ixgbe_reset(adapter);

	/* power down the optics for 82599 SFP+ fiber */
	if (hw->mac.ops.disable_tx_laser)
		hw->mac.ops.disable_tx_laser(hw);

	ixgbe_clean_all_tx_rings(adapter);
	ixgbe_clean_all_rx_rings(adapter);
	if (adapter->hw.mac.type == ixgbe_mac_e610)
		ixgbe_disable_link_status_events(adapter);
}

/**
 * ixgbe_set_eee_capable - helper function to determine EEE support on X550
 * @adapter: board private structure
 */
static void ixgbe_set_eee_capable(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;

	switch (hw->device_id) {
	case IXGBE_DEV_ID_X550EM_A_1G_T:
	case IXGBE_DEV_ID_X550EM_A_1G_T_L:
		if (!hw->phy.eee_speeds_supported)
			break;
		adapter->flags2 |= IXGBE_FLAG2_EEE_CAPABLE;
		if (!hw->phy.eee_speeds_advertised)
			break;
		adapter->flags2 |= IXGBE_FLAG2_EEE_ENABLED;
		break;
	default:
		adapter->flags2 &= ~IXGBE_FLAG2_EEE_CAPABLE;
		adapter->flags2 &= ~IXGBE_FLAG2_EEE_ENABLED;
		break;
	}
}

/**
 * ixgbe_tx_timeout - Respond to a Tx Hang
 * @netdev: network interface device structure
 * @txqueue: queue number that timed out
 **/
static void ixgbe_tx_timeout(struct net_device *netdev, unsigned int __always_unused txqueue)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	/* Do the reset outside of interrupt context */
	ixgbe_tx_timeout_reset(adapter);
}

#ifdef CONFIG_IXGBE_DCB
static void ixgbe_init_dcb(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct tc_configuration *tc;
	int j;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
	case ixgbe_mac_82599EB:
		adapter->dcb_cfg.num_tcs.pg_tcs = MAX_TRAFFIC_CLASS;
		adapter->dcb_cfg.num_tcs.pfc_tcs = MAX_TRAFFIC_CLASS;
		break;
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_e610:
		adapter->dcb_cfg.num_tcs.pg_tcs = X540_TRAFFIC_CLASS;
		adapter->dcb_cfg.num_tcs.pfc_tcs = X540_TRAFFIC_CLASS;
		break;
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	default:
		adapter->dcb_cfg.num_tcs.pg_tcs = DEF_TRAFFIC_CLASS;
		adapter->dcb_cfg.num_tcs.pfc_tcs = DEF_TRAFFIC_CLASS;
		break;
	}

	/* Configure DCB traffic classes */
	for (j = 0; j < MAX_TRAFFIC_CLASS; j++) {
		tc = &adapter->dcb_cfg.tc_config[j];
		tc->path[DCB_TX_CONFIG].bwg_id = 0;
		tc->path[DCB_TX_CONFIG].bwg_percent = 12 + (j & 1);
		tc->path[DCB_RX_CONFIG].bwg_id = 0;
		tc->path[DCB_RX_CONFIG].bwg_percent = 12 + (j & 1);
		tc->dcb_pfc = pfc_disabled;
	}

	/* Initialize default user to priority mapping, UPx->TC0 */
	tc = &adapter->dcb_cfg.tc_config[0];
	tc->path[DCB_TX_CONFIG].up_to_tc_bitmap = 0xFF;
	tc->path[DCB_RX_CONFIG].up_to_tc_bitmap = 0xFF;

	adapter->dcb_cfg.bw_percentage[DCB_TX_CONFIG][0] = 100;
	adapter->dcb_cfg.bw_percentage[DCB_RX_CONFIG][0] = 100;
	adapter->dcb_cfg.pfc_mode_enable = false;
	adapter->dcb_set_bitmap = 0x00;
	if (adapter->flags & IXGBE_FLAG_DCB_CAPABLE)
		adapter->dcbx_cap = DCB_CAP_DCBX_HOST | DCB_CAP_DCBX_VER_CEE;
	memcpy(&adapter->temp_dcb_cfg, &adapter->dcb_cfg,
	       sizeof(adapter->temp_dcb_cfg));
}
#endif

/**
 * ixgbe_sw_init - Initialize general software structures (struct ixgbe_adapter)
 * @adapter: board private structure to initialize
 * @ii: pointer to ixgbe_info for device
 *
 * ixgbe_sw_init initializes the Adapter private data structure.
 * Fields are initialized based on PCI device information and
 * OS network device settings (MTU size).
 **/
static int ixgbe_sw_init(struct ixgbe_adapter *adapter,
			 const struct ixgbe_info *ii)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct pci_dev *pdev = adapter->pdev;
	unsigned int rss, fdir;
	u32 fwsm;
	int i;

	/* PCI config space info */

	hw->vendor_id = pdev->vendor;
	hw->device_id = pdev->device;
	hw->revision_id = pdev->revision;
	hw->subsystem_vendor_id = pdev->subsystem_vendor;
	hw->subsystem_device_id = pdev->subsystem_device;

	hw->mac.max_link_up_time = IXGBE_LINK_UP_TIME;

	/* get_invariants needs the device IDs */
	ii->get_invariants(hw);

	/* Set common capability flags and settings */
	rss = min_t(int, ixgbe_max_rss_indices(adapter), num_online_cpus());
	adapter->ring_feature[RING_F_RSS].limit = rss;
	adapter->flags2 |= IXGBE_FLAG2_RSC_CAPABLE;
	adapter->max_q_vectors = MAX_Q_VECTORS_82599;
	adapter->atr_sample_rate = 20;
	fdir = min_t(int, IXGBE_MAX_FDIR_INDICES, num_online_cpus());
	adapter->ring_feature[RING_F_FDIR].limit = fdir;
	adapter->fdir_pballoc = IXGBE_FDIR_PBALLOC_64K;
	adapter->ring_feature[RING_F_VMDQ].limit = 1;
#ifdef CONFIG_IXGBE_DCA
	adapter->flags |= IXGBE_FLAG_DCA_CAPABLE;
#endif
#ifdef CONFIG_IXGBE_DCB
	adapter->flags |= IXGBE_FLAG_DCB_CAPABLE;
	adapter->flags &= ~IXGBE_FLAG_DCB_ENABLED;
#endif
#ifdef IXGBE_FCOE
	adapter->flags |= IXGBE_FLAG_FCOE_CAPABLE;
	adapter->flags &= ~IXGBE_FLAG_FCOE_ENABLED;
#ifdef CONFIG_IXGBE_DCB
	/* Default traffic class to use for FCoE */
	adapter->fcoe.up = IXGBE_FCOE_DEFTC;
#endif /* CONFIG_IXGBE_DCB */
#endif /* IXGBE_FCOE */

	/* initialize static ixgbe jump table entries */
	adapter->jump_tables[0] = kzalloc(sizeof(*adapter->jump_tables[0]),
					  GFP_KERNEL);
	if (!adapter->jump_tables[0])
		return -ENOMEM;
	adapter->jump_tables[0]->mat = ixgbe_ipv4_fields;

	for (i = 1; i < IXGBE_MAX_LINK_HANDLE; i++)
		adapter->jump_tables[i] = NULL;

	adapter->mac_table = kcalloc(hw->mac.num_rar_entries,
				     sizeof(struct ixgbe_mac_addr),
				     GFP_KERNEL);
	if (!adapter->mac_table)
		return -ENOMEM;

	if (ixgbe_init_rss_key(adapter))
		return -ENOMEM;

	adapter->af_xdp_zc_qps = bitmap_zalloc(IXGBE_MAX_XDP_QS, GFP_KERNEL);
	if (!adapter->af_xdp_zc_qps)
		return -ENOMEM;

	/* Set MAC specific capability flags and exceptions */
	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		adapter->flags2 &= ~IXGBE_FLAG2_RSC_CAPABLE;

		if (hw->device_id == IXGBE_DEV_ID_82598AT)
			adapter->flags |= IXGBE_FLAG_FAN_FAIL_CAPABLE;

		adapter->max_q_vectors = MAX_Q_VECTORS_82598;
		adapter->ring_feature[RING_F_FDIR].limit = 0;
		adapter->atr_sample_rate = 0;
		adapter->fdir_pballoc = 0;
#ifdef IXGBE_FCOE
		adapter->flags &= ~IXGBE_FLAG_FCOE_CAPABLE;
		adapter->flags &= ~IXGBE_FLAG_FCOE_ENABLED;
#ifdef CONFIG_IXGBE_DCB
		adapter->fcoe.up = 0;
#endif /* IXGBE_DCB */
#endif /* IXGBE_FCOE */
		break;
	case ixgbe_mac_82599EB:
		if (hw->device_id == IXGBE_DEV_ID_82599_T3_LOM)
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_CAPABLE;
		break;
	case ixgbe_mac_X540:
		fwsm = IXGBE_READ_REG(hw, IXGBE_FWSM(hw));
		if (fwsm & IXGBE_FWSM_TS_ENABLED)
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_CAPABLE;
		break;
	case ixgbe_mac_x550em_a:
		switch (hw->device_id) {
		case IXGBE_DEV_ID_X550EM_A_1G_T:
		case IXGBE_DEV_ID_X550EM_A_1G_T_L:
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_CAPABLE;
			break;
		default:
			break;
		}
		fallthrough;
	case ixgbe_mac_X550EM_x:
#ifdef CONFIG_IXGBE_DCB
		adapter->flags &= ~IXGBE_FLAG_DCB_CAPABLE;
#endif
#ifdef IXGBE_FCOE
		adapter->flags &= ~IXGBE_FLAG_FCOE_CAPABLE;
#ifdef CONFIG_IXGBE_DCB
		adapter->fcoe.up = 0;
#endif /* IXGBE_DCB */
#endif /* IXGBE_FCOE */
		fallthrough;
	case ixgbe_mac_X550:
		if (hw->mac.type == ixgbe_mac_X550)
			adapter->flags2 |= IXGBE_FLAG2_TEMP_SENSOR_CAPABLE;
#ifdef CONFIG_IXGBE_DCA
		adapter->flags &= ~IXGBE_FLAG_DCA_CAPABLE;
#endif
		break;
	default:
		break;
	}

#ifdef IXGBE_FCOE
	/* FCoE support exists, always init the FCoE lock */
	spin_lock_init(&adapter->fcoe.lock);

#endif
	/* n-tuple support exists, always init our spinlock */
	spin_lock_init(&adapter->fdir_perfect_lock);

	/* init spinlock to avoid concurrency of VF resources */
	spin_lock_init(&adapter->vfs_lock);

#ifdef CONFIG_IXGBE_DCB
	ixgbe_init_dcb(adapter);
#endif
	ixgbe_init_ipsec_offload(adapter);

	/* default flow control settings */
	hw->fc.requested_mode = ixgbe_fc_full;
	hw->fc.current_mode = ixgbe_fc_full;	/* init for ethtool output */
	ixgbe_pbthresh_setup(adapter);
	hw->fc.pause_time = IXGBE_DEFAULT_FCPAUSE;
	hw->fc.send_xon = true;
	hw->fc.disable_fc_autoneg = ixgbe_device_supports_autoneg_fc(hw);

#ifdef CONFIG_PCI_IOV
	if (max_vfs > 0)
		e_dev_warn("Enabling SR-IOV VFs using the max_vfs module parameter is deprecated - please use the pci sysfs interface instead.\n");

	/* assign number of SR-IOV VFs */
	if (hw->mac.type != ixgbe_mac_82598EB) {
		if (max_vfs > IXGBE_MAX_VFS_DRV_LIMIT) {
			max_vfs = 0;
			e_dev_warn("max_vfs parameter out of range. Not assigning any SR-IOV VFs\n");
		}
	}
#endif /* CONFIG_PCI_IOV */

	/* enable itr by default in dynamic mode */
	adapter->rx_itr_setting = 1;
	adapter->tx_itr_setting = 1;

	/* set default ring sizes */
	adapter->tx_ring_count = IXGBE_DEFAULT_TXD;
	adapter->rx_ring_count = IXGBE_DEFAULT_RXD;

	/* set default work limits */
	adapter->tx_work_limit = IXGBE_DEFAULT_TX_WORK;

	/* initialize eeprom parameters */
	if (ixgbe_init_eeprom_params_generic(hw)) {
		e_dev_err("EEPROM initialization failed\n");
		return -EIO;
	}

	/* PF holds first pool slot */
	set_bit(0, adapter->fwd_bitmask);
	set_bit(__IXGBE_DOWN, &adapter->state);

	/* enable locking for XDP_TX if we have more CPUs than queues */
	if (nr_cpu_ids > IXGBE_MAX_XDP_QS)
		static_branch_enable(&ixgbe_xdp_locking_key);

	return 0;
}

/**
 * ixgbe_setup_tx_resources - allocate Tx resources (Descriptors)
 * @tx_ring:    tx descriptor ring (for a specific queue) to setup
 *
 * Return 0 on success, negative on failure
 **/
int ixgbe_setup_tx_resources(struct ixgbe_ring *tx_ring)
{
	struct device *dev = tx_ring->dev;
	int orig_node = dev_to_node(dev);
	int ring_node = NUMA_NO_NODE;
	int size;

	size = sizeof(struct ixgbe_tx_buffer) * tx_ring->count;

	if (tx_ring->q_vector)
		ring_node = tx_ring->q_vector->numa_node;

	tx_ring->tx_buffer_info = vmalloc_node(size, ring_node);
	if (!tx_ring->tx_buffer_info)
		tx_ring->tx_buffer_info = vmalloc(size);
	if (!tx_ring->tx_buffer_info)
		goto err;

	/* round up to nearest 4K */
	tx_ring->size = tx_ring->count * sizeof(union ixgbe_adv_tx_desc);
	tx_ring->size = ALIGN(tx_ring->size, 4096);

	set_dev_node(dev, ring_node);
	tx_ring->desc = dma_alloc_coherent(dev,
					   tx_ring->size,
					   &tx_ring->dma,
					   GFP_KERNEL);
	set_dev_node(dev, orig_node);
	if (!tx_ring->desc)
		tx_ring->desc = dma_alloc_coherent(dev, tx_ring->size,
						   &tx_ring->dma, GFP_KERNEL);
	if (!tx_ring->desc)
		goto err;

	tx_ring->next_to_use = 0;
	tx_ring->next_to_clean = 0;
	return 0;

err:
	vfree(tx_ring->tx_buffer_info);
	tx_ring->tx_buffer_info = NULL;
	dev_err(dev, "Unable to allocate memory for the Tx descriptor ring\n");
	return -ENOMEM;
}

/**
 * ixgbe_setup_all_tx_resources - allocate all queues Tx resources
 * @adapter: board private structure
 *
 * If this function returns with an error, then it's possible one or
 * more of the rings is populated (while the rest are not).  It is the
 * callers duty to clean those orphaned rings.
 *
 * Return 0 on success, negative on failure
 **/
static int ixgbe_setup_all_tx_resources(struct ixgbe_adapter *adapter)
{
	int i, j = 0, err = 0;

	for (i = 0; i < adapter->num_tx_queues; i++) {
		err = ixgbe_setup_tx_resources(adapter->tx_ring[i]);
		if (!err)
			continue;

		e_err(probe, "Allocation for Tx Queue %u failed\n", i);
		goto err_setup_tx;
	}
	for (j = 0; j < adapter->num_xdp_queues; j++) {
		err = ixgbe_setup_tx_resources(adapter->xdp_ring[j]);
		if (!err)
			continue;

		e_err(probe, "Allocation for Tx Queue %u failed\n", j);
		goto err_setup_tx;
	}

	return 0;
err_setup_tx:
	/* rewind the index freeing the rings as we go */
	while (j--)
		ixgbe_free_tx_resources(adapter->xdp_ring[j]);
	while (i--)
		ixgbe_free_tx_resources(adapter->tx_ring[i]);
	return err;
}

static int ixgbe_rx_napi_id(struct ixgbe_ring *rx_ring)
{
	struct ixgbe_q_vector *q_vector = rx_ring->q_vector;

	return q_vector ? q_vector->napi.napi_id : 0;
}

/**
 * ixgbe_setup_rx_resources - allocate Rx resources (Descriptors)
 * @adapter: pointer to ixgbe_adapter
 * @rx_ring:    rx descriptor ring (for a specific queue) to setup
 *
 * Returns 0 on success, negative on failure
 **/
int ixgbe_setup_rx_resources(struct ixgbe_adapter *adapter,
			     struct ixgbe_ring *rx_ring)
{
	struct device *dev = rx_ring->dev;
	int orig_node = dev_to_node(dev);
	int ring_node = NUMA_NO_NODE;
	int size;

	size = sizeof(struct ixgbe_rx_buffer) * rx_ring->count;

	if (rx_ring->q_vector)
		ring_node = rx_ring->q_vector->numa_node;

	rx_ring->rx_buffer_info = vmalloc_node(size, ring_node);
	if (!rx_ring->rx_buffer_info)
		rx_ring->rx_buffer_info = vmalloc(size);
	if (!rx_ring->rx_buffer_info)
		goto err;

	/* Round up to nearest 4K */
	rx_ring->size = rx_ring->count * sizeof(union ixgbe_adv_rx_desc);
	rx_ring->size = ALIGN(rx_ring->size, 4096);

	set_dev_node(dev, ring_node);
	rx_ring->desc = dma_alloc_coherent(dev,
					   rx_ring->size,
					   &rx_ring->dma,
					   GFP_KERNEL);
	set_dev_node(dev, orig_node);
	if (!rx_ring->desc)
		rx_ring->desc = dma_alloc_coherent(dev, rx_ring->size,
						   &rx_ring->dma, GFP_KERNEL);
	if (!rx_ring->desc)
		goto err;

	rx_ring->next_to_clean = 0;
	rx_ring->next_to_use = 0;

	/* XDP RX-queue info */
	if (xdp_rxq_info_reg(&rx_ring->xdp_rxq, adapter->netdev,
			     rx_ring->queue_index, ixgbe_rx_napi_id(rx_ring)) < 0)
		goto err;

	WRITE_ONCE(rx_ring->xdp_prog, adapter->xdp_prog);

	return 0;
err:
	vfree(rx_ring->rx_buffer_info);
	rx_ring->rx_buffer_info = NULL;
	dev_err(dev, "Unable to allocate memory for the Rx descriptor ring\n");
	return -ENOMEM;
}

/**
 * ixgbe_setup_all_rx_resources - allocate all queues Rx resources
 * @adapter: board private structure
 *
 * If this function returns with an error, then it's possible one or
 * more of the rings is populated (while the rest are not).  It is the
 * callers duty to clean those orphaned rings.
 *
 * Return 0 on success, negative on failure
 **/
static int ixgbe_setup_all_rx_resources(struct ixgbe_adapter *adapter)
{
	int i, err = 0;

	for (i = 0; i < adapter->num_rx_queues; i++) {
		err = ixgbe_setup_rx_resources(adapter, adapter->rx_ring[i]);
		if (!err)
			continue;

		e_err(probe, "Allocation for Rx Queue %u failed\n", i);
		goto err_setup_rx;
	}

#ifdef IXGBE_FCOE
	err = ixgbe_setup_fcoe_ddp_resources(adapter);
	if (!err)
#endif
		return 0;
err_setup_rx:
	/* rewind the index freeing the rings as we go */
	while (i--)
		ixgbe_free_rx_resources(adapter->rx_ring[i]);
	return err;
}

/**
 * ixgbe_free_tx_resources - Free Tx Resources per Queue
 * @tx_ring: Tx descriptor ring for a specific queue
 *
 * Free all transmit software resources
 **/
void ixgbe_free_tx_resources(struct ixgbe_ring *tx_ring)
{
	ixgbe_clean_tx_ring(tx_ring);

	vfree(tx_ring->tx_buffer_info);
	tx_ring->tx_buffer_info = NULL;

	/* if not set, then don't free */
	if (!tx_ring->desc)
		return;

	dma_free_coherent(tx_ring->dev, tx_ring->size,
			  tx_ring->desc, tx_ring->dma);

	tx_ring->desc = NULL;
}

/**
 * ixgbe_free_all_tx_resources - Free Tx Resources for All Queues
 * @adapter: board private structure
 *
 * Free all transmit software resources
 **/
static void ixgbe_free_all_tx_resources(struct ixgbe_adapter *adapter)
{
	int i;

	for (i = 0; i < adapter->num_tx_queues; i++)
		if (adapter->tx_ring[i]->desc)
			ixgbe_free_tx_resources(adapter->tx_ring[i]);
	for (i = 0; i < adapter->num_xdp_queues; i++)
		if (adapter->xdp_ring[i]->desc)
			ixgbe_free_tx_resources(adapter->xdp_ring[i]);
}

/**
 * ixgbe_free_rx_resources - Free Rx Resources
 * @rx_ring: ring to clean the resources from
 *
 * Free all receive software resources
 **/
void ixgbe_free_rx_resources(struct ixgbe_ring *rx_ring)
{
	ixgbe_clean_rx_ring(rx_ring);

	rx_ring->xdp_prog = NULL;
	xdp_rxq_info_unreg(&rx_ring->xdp_rxq);
	vfree(rx_ring->rx_buffer_info);
	rx_ring->rx_buffer_info = NULL;

	/* if not set, then don't free */
	if (!rx_ring->desc)
		return;

	dma_free_coherent(rx_ring->dev, rx_ring->size,
			  rx_ring->desc, rx_ring->dma);

	rx_ring->desc = NULL;
}

/**
 * ixgbe_free_all_rx_resources - Free Rx Resources for All Queues
 * @adapter: board private structure
 *
 * Free all receive software resources
 **/
static void ixgbe_free_all_rx_resources(struct ixgbe_adapter *adapter)
{
	int i;

#ifdef IXGBE_FCOE
	ixgbe_free_fcoe_ddp_resources(adapter);

#endif
	for (i = 0; i < adapter->num_rx_queues; i++)
		if (adapter->rx_ring[i]->desc)
			ixgbe_free_rx_resources(adapter->rx_ring[i]);
}

/**
 * ixgbe_max_xdp_frame_size - returns the maximum allowed frame size for XDP
 * @adapter: device handle, pointer to adapter
 */
static int ixgbe_max_xdp_frame_size(struct ixgbe_adapter *adapter)
{
	if (PAGE_SIZE >= 8192 || adapter->flags2 & IXGBE_FLAG2_RX_LEGACY)
		return IXGBE_RXBUFFER_2K;
	else
		return IXGBE_RXBUFFER_3K;
}

/**
 * ixgbe_change_mtu - Change the Maximum Transfer Unit
 * @netdev: network interface device structure
 * @new_mtu: new value for maximum frame size
 *
 * Returns 0 on success, negative on failure
 **/
static int ixgbe_change_mtu(struct net_device *netdev, int new_mtu)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	if (ixgbe_enabled_xdp_adapter(adapter)) {
		int new_frame_size = new_mtu + IXGBE_PKT_HDR_PAD;

		if (new_frame_size > ixgbe_max_xdp_frame_size(adapter)) {
			e_warn(probe, "Requested MTU size is not supported with XDP\n");
			return -EINVAL;
		}
	}

	/*
	 * For 82599EB we cannot allow legacy VFs to enable their receive
	 * paths when MTU greater than 1500 is configured.  So display a
	 * warning that legacy VFs will be disabled.
	 */
	if ((adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) &&
	    (adapter->hw.mac.type == ixgbe_mac_82599EB) &&
	    (new_mtu > ETH_DATA_LEN))
		e_warn(probe, "Setting MTU > 1500 will disable legacy VFs\n");

	netdev_dbg(netdev, "changing MTU from %d to %d\n",
		   netdev->mtu, new_mtu);

	/* must set new MTU before calling down or up */
	WRITE_ONCE(netdev->mtu, new_mtu);

	if (netif_running(netdev))
		ixgbe_reinit_locked(adapter);

	return 0;
}

/**
 * ixgbe_open - Called when a network interface is made active
 * @netdev: network interface device structure
 *
 * Returns 0 on success, negative value on failure
 *
 * The open entry point is called when a network interface is made
 * active by the system (IFF_UP).  At this point all resources needed
 * for transmit and receive operations are allocated, the interrupt
 * handler is registered with the OS, the watchdog timer is started,
 * and the stack is notified that the interface is ready.
 **/
int ixgbe_open(struct net_device *netdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;
	int err, queues;

	/* disallow open during test */
	if (test_bit(__IXGBE_TESTING, &adapter->state))
		return -EBUSY;

	netif_carrier_off(netdev);

	/* allocate transmit descriptors */
	err = ixgbe_setup_all_tx_resources(adapter);
	if (err)
		goto err_setup_tx;

	/* allocate receive descriptors */
	err = ixgbe_setup_all_rx_resources(adapter);
	if (err)
		goto err_setup_rx;

	ixgbe_configure(adapter);

	err = ixgbe_request_irq(adapter);
	if (err)
		goto err_req_irq;

	/* Notify the stack of the actual queue counts. */
	queues = adapter->num_tx_queues;
	err = netif_set_real_num_tx_queues(netdev, queues);
	if (err)
		goto err_set_queues;

	queues = adapter->num_rx_queues;
	err = netif_set_real_num_rx_queues(netdev, queues);
	if (err)
		goto err_set_queues;

	ixgbe_ptp_init(adapter);

	ixgbe_up_complete(adapter);

	udp_tunnel_nic_reset_ntf(netdev);
	if (adapter->hw.mac.type == ixgbe_mac_e610) {
		int err = ixgbe_update_link_info(&adapter->hw);

		if (err)
			e_dev_err("Failed to update link info, err %d.\n", err);

		ixgbe_check_link_cfg_err(adapter,
					 adapter->hw.link.link_info.link_cfg_err);

		err = ixgbe_non_sfp_link_config(&adapter->hw);
		if (ixgbe_non_sfp_link_config(&adapter->hw))
			e_dev_err("Link setup failed, err %d.\n", err);
	}

	return 0;

err_set_queues:
	ixgbe_free_irq(adapter);
err_req_irq:
	ixgbe_free_all_rx_resources(adapter);
	if (hw->phy.ops.set_phy_power && !adapter->wol)
		hw->phy.ops.set_phy_power(&adapter->hw, false);
err_setup_rx:
	ixgbe_free_all_tx_resources(adapter);
err_setup_tx:
	ixgbe_reset(adapter);

	return err;
}

static void ixgbe_close_suspend(struct ixgbe_adapter *adapter)
{
	ixgbe_ptp_suspend(adapter);

	if (adapter->hw.phy.ops.enter_lplu) {
		adapter->hw.phy.reset_disable = true;
		ixgbe_down(adapter);
		adapter->hw.phy.ops.enter_lplu(&adapter->hw);
		adapter->hw.phy.reset_disable = false;
	} else {
		ixgbe_down(adapter);
	}

	ixgbe_free_irq(adapter);

	ixgbe_free_all_tx_resources(adapter);
	ixgbe_free_all_rx_resources(adapter);
}

/**
 * ixgbe_close - Disables a network interface
 * @netdev: network interface device structure
 *
 * Returns 0, this is not allowed to fail
 *
 * The close entry point is called when an interface is de-activated
 * by the OS.  The hardware is still under the drivers control, but
 * needs to be disabled.  A global MAC reset is issued to stop the
 * hardware, and all transmit and receive resources are freed.
 **/
int ixgbe_close(struct net_device *netdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	ixgbe_ptp_stop(adapter);

	if (netif_device_present(netdev))
		ixgbe_close_suspend(adapter);

	ixgbe_fdir_filter_exit(adapter);

	ixgbe_release_hw_control(adapter);

	return 0;
}

static int ixgbe_resume(struct device *dev_d)
{
	struct pci_dev *pdev = to_pci_dev(dev_d);
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	struct net_device *netdev = adapter->netdev;
	u32 err;

	adapter->hw.hw_addr = adapter->io_addr;

	err = pci_enable_device_mem(pdev);
	if (err) {
		e_dev_err("Cannot enable PCI device from suspend\n");
		return err;
	}
	smp_mb__before_atomic();
	clear_bit(__IXGBE_DISABLED, &adapter->state);
	pci_set_master(pdev);

	device_wakeup_disable(dev_d);

	ixgbe_reset(adapter);

	IXGBE_WRITE_REG(&adapter->hw, IXGBE_WUS, ~0);

	rtnl_lock();
	err = ixgbe_init_interrupt_scheme(adapter);
	if (!err && netif_running(netdev))
		err = ixgbe_open(netdev);


	if (!err)
		netif_device_attach(netdev);
	rtnl_unlock();

	return err;
}

static int __ixgbe_shutdown(struct pci_dev *pdev, bool *enable_wake)
{
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;
	u32 ctrl;
	u32 wufc = adapter->wol;

	rtnl_lock();
	netif_device_detach(netdev);

	if (netif_running(netdev))
		ixgbe_close_suspend(adapter);

	ixgbe_clear_interrupt_scheme(adapter);
	rtnl_unlock();

	if (hw->mac.ops.stop_link_on_d3)
		hw->mac.ops.stop_link_on_d3(hw);

	if (wufc) {
		u32 fctrl;

		ixgbe_set_rx_mode(netdev);

		/* enable the optics for 82599 SFP+ fiber as we can WoL */
		if (hw->mac.ops.enable_tx_laser)
			hw->mac.ops.enable_tx_laser(hw);

		/* enable the reception of multicast packets */
		fctrl = IXGBE_READ_REG(hw, IXGBE_FCTRL);
		fctrl |= IXGBE_FCTRL_MPE;
		IXGBE_WRITE_REG(hw, IXGBE_FCTRL, fctrl);

		ctrl = IXGBE_READ_REG(hw, IXGBE_CTRL);
		ctrl |= IXGBE_CTRL_GIO_DIS;
		IXGBE_WRITE_REG(hw, IXGBE_CTRL, ctrl);

		IXGBE_WRITE_REG(hw, IXGBE_WUFC, wufc);
	} else {
		IXGBE_WRITE_REG(hw, IXGBE_WUC, 0);
		IXGBE_WRITE_REG(hw, IXGBE_WUFC, 0);
	}

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		pci_wake_from_d3(pdev, false);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		pci_wake_from_d3(pdev, !!wufc);
		break;
	default:
		break;
	}

	*enable_wake = !!wufc;
	if (hw->phy.ops.set_phy_power && !*enable_wake)
		hw->phy.ops.set_phy_power(hw, false);

	ixgbe_release_hw_control(adapter);

	if (!test_and_set_bit(__IXGBE_DISABLED, &adapter->state))
		pci_disable_device(pdev);

	return 0;
}

static int ixgbe_suspend(struct device *dev_d)
{
	struct pci_dev *pdev = to_pci_dev(dev_d);
	int retval;
	bool wake;

	retval = __ixgbe_shutdown(pdev, &wake);

	device_set_wakeup_enable(dev_d, wake);

	return retval;
}

static void ixgbe_shutdown(struct pci_dev *pdev)
{
	bool wake;

	__ixgbe_shutdown(pdev, &wake);

	if (system_state == SYSTEM_POWER_OFF) {
		pci_wake_from_d3(pdev, wake);
		pci_set_power_state(pdev, PCI_D3hot);
	}
}

/**
 * ixgbe_update_stats - Update the board statistics counters.
 * @adapter: board private structure
 **/
void ixgbe_update_stats(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_hw_stats *hwstats = &adapter->stats;
	u64 total_mpc = 0;
	u32 i, missed_rx = 0, mpc, bprc, lxon, lxoff, xon_off_tot;
	u64 non_eop_descs = 0, restart_queue = 0, tx_busy = 0;
	u64 alloc_rx_page_failed = 0, alloc_rx_buff_failed = 0;
	u64 alloc_rx_page = 0;
	u64 bytes = 0, packets = 0, hw_csum_rx_error = 0;

	if (test_bit(__IXGBE_DOWN, &adapter->state) ||
	    test_bit(__IXGBE_RESETTING, &adapter->state))
		return;

	if (adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED) {
		u64 rsc_count = 0;
		u64 rsc_flush = 0;
		for (i = 0; i < adapter->num_rx_queues; i++) {
			rsc_count += adapter->rx_ring[i]->rx_stats.rsc_count;
			rsc_flush += adapter->rx_ring[i]->rx_stats.rsc_flush;
		}
		adapter->rsc_total_count = rsc_count;
		adapter->rsc_total_flush = rsc_flush;
	}

	for (i = 0; i < adapter->num_rx_queues; i++) {
		struct ixgbe_ring *rx_ring = READ_ONCE(adapter->rx_ring[i]);

		if (!rx_ring)
			continue;
		non_eop_descs += rx_ring->rx_stats.non_eop_descs;
		alloc_rx_page += rx_ring->rx_stats.alloc_rx_page;
		alloc_rx_page_failed += rx_ring->rx_stats.alloc_rx_page_failed;
		alloc_rx_buff_failed += rx_ring->rx_stats.alloc_rx_buff_failed;
		hw_csum_rx_error += rx_ring->rx_stats.csum_err;
		bytes += rx_ring->stats.bytes;
		packets += rx_ring->stats.packets;
	}
	adapter->non_eop_descs = non_eop_descs;
	adapter->alloc_rx_page = alloc_rx_page;
	adapter->alloc_rx_page_failed = alloc_rx_page_failed;
	adapter->alloc_rx_buff_failed = alloc_rx_buff_failed;
	adapter->hw_csum_rx_error = hw_csum_rx_error;
	netdev->stats.rx_bytes = bytes;
	netdev->stats.rx_packets = packets;

	bytes = 0;
	packets = 0;
	/* gather some stats to the adapter struct that are per queue */
	for (i = 0; i < adapter->num_tx_queues; i++) {
		struct ixgbe_ring *tx_ring = READ_ONCE(adapter->tx_ring[i]);

		if (!tx_ring)
			continue;
		restart_queue += tx_ring->tx_stats.restart_queue;
		tx_busy += tx_ring->tx_stats.tx_busy;
		bytes += tx_ring->stats.bytes;
		packets += tx_ring->stats.packets;
	}
	for (i = 0; i < adapter->num_xdp_queues; i++) {
		struct ixgbe_ring *xdp_ring = READ_ONCE(adapter->xdp_ring[i]);

		if (!xdp_ring)
			continue;
		restart_queue += xdp_ring->tx_stats.restart_queue;
		tx_busy += xdp_ring->tx_stats.tx_busy;
		bytes += xdp_ring->stats.bytes;
		packets += xdp_ring->stats.packets;
	}
	adapter->restart_queue = restart_queue;
	adapter->tx_busy = tx_busy;
	netdev->stats.tx_bytes = bytes;
	netdev->stats.tx_packets = packets;

	hwstats->crcerrs += IXGBE_READ_REG(hw, IXGBE_CRCERRS);

	/* 8 register reads */
	for (i = 0; i < 8; i++) {
		/* for packet buffers not used, the register should read 0 */
		mpc = IXGBE_READ_REG(hw, IXGBE_MPC(i));
		missed_rx += mpc;
		hwstats->mpc[i] += mpc;
		total_mpc += hwstats->mpc[i];
		hwstats->pxontxc[i] += IXGBE_READ_REG(hw, IXGBE_PXONTXC(i));
		hwstats->pxofftxc[i] += IXGBE_READ_REG(hw, IXGBE_PXOFFTXC(i));
		switch (hw->mac.type) {
		case ixgbe_mac_82598EB:
			hwstats->rnbc[i] += IXGBE_READ_REG(hw, IXGBE_RNBC(i));
			hwstats->qbtc[i] += IXGBE_READ_REG(hw, IXGBE_QBTC(i));
			hwstats->qbrc[i] += IXGBE_READ_REG(hw, IXGBE_QBRC(i));
			hwstats->pxonrxc[i] +=
				IXGBE_READ_REG(hw, IXGBE_PXONRXC(i));
			break;
		case ixgbe_mac_82599EB:
		case ixgbe_mac_X540:
		case ixgbe_mac_X550:
		case ixgbe_mac_X550EM_x:
		case ixgbe_mac_x550em_a:
		case ixgbe_mac_e610:
			hwstats->pxonrxc[i] +=
				IXGBE_READ_REG(hw, IXGBE_PXONRXCNT(i));
			break;
		default:
			break;
		}
	}

	/*16 register reads */
	for (i = 0; i < 16; i++) {
		hwstats->qptc[i] += IXGBE_READ_REG(hw, IXGBE_QPTC(i));
		hwstats->qprc[i] += IXGBE_READ_REG(hw, IXGBE_QPRC(i));
		if (hw->mac.type == ixgbe_mac_82599EB ||
		    hw->mac.type == ixgbe_mac_X540 ||
		    hw->mac.type == ixgbe_mac_X550 ||
		    hw->mac.type == ixgbe_mac_X550EM_x ||
		    hw->mac.type == ixgbe_mac_x550em_a ||
		    hw->mac.type == ixgbe_mac_e610) {
			hwstats->qbtc[i] += IXGBE_READ_REG(hw, IXGBE_QBTC_L(i));
			IXGBE_READ_REG(hw, IXGBE_QBTC_H(i)); /* to clear */
			hwstats->qbrc[i] += IXGBE_READ_REG(hw, IXGBE_QBRC_L(i));
			IXGBE_READ_REG(hw, IXGBE_QBRC_H(i)); /* to clear */
		}
	}

	hwstats->gprc += IXGBE_READ_REG(hw, IXGBE_GPRC);
	/* work around hardware counting issue */
	hwstats->gprc -= missed_rx;

	ixgbe_update_xoff_received(adapter);

	/* 82598 hardware only has a 32 bit counter in the high register */
	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		hwstats->lxonrxc += IXGBE_READ_REG(hw, IXGBE_LXONRXC);
		hwstats->gorc += IXGBE_READ_REG(hw, IXGBE_GORCH);
		hwstats->gotc += IXGBE_READ_REG(hw, IXGBE_GOTCH);
		hwstats->tor += IXGBE_READ_REG(hw, IXGBE_TORH);
		break;
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		/* OS2BMC stats are X540 and later */
		hwstats->o2bgptc += IXGBE_READ_REG(hw, IXGBE_O2BGPTC);
		hwstats->o2bspc += IXGBE_READ_REG(hw, IXGBE_O2BSPC);
		hwstats->b2ospc += IXGBE_READ_REG(hw, IXGBE_B2OSPC);
		hwstats->b2ogprc += IXGBE_READ_REG(hw, IXGBE_B2OGPRC);
		fallthrough;
	case ixgbe_mac_82599EB:
		for (i = 0; i < 16; i++)
			adapter->hw_rx_no_dma_resources +=
					     IXGBE_READ_REG(hw, IXGBE_QPRDC(i));
		hwstats->gorc += IXGBE_READ_REG(hw, IXGBE_GORCL);
		IXGBE_READ_REG(hw, IXGBE_GORCH); /* to clear */
		hwstats->gotc += IXGBE_READ_REG(hw, IXGBE_GOTCL);
		IXGBE_READ_REG(hw, IXGBE_GOTCH); /* to clear */
		hwstats->tor += IXGBE_READ_REG(hw, IXGBE_TORL);
		IXGBE_READ_REG(hw, IXGBE_TORH); /* to clear */
		hwstats->lxonrxc += IXGBE_READ_REG(hw, IXGBE_LXONRXCNT);
		hwstats->fdirmatch += IXGBE_READ_REG(hw, IXGBE_FDIRMATCH);
		hwstats->fdirmiss += IXGBE_READ_REG(hw, IXGBE_FDIRMISS);
#ifdef IXGBE_FCOE
		hwstats->fccrc += IXGBE_READ_REG(hw, IXGBE_FCCRC);
		hwstats->fcoerpdc += IXGBE_READ_REG(hw, IXGBE_FCOERPDC);
		hwstats->fcoeprc += IXGBE_READ_REG(hw, IXGBE_FCOEPRC);
		hwstats->fcoeptc += IXGBE_READ_REG(hw, IXGBE_FCOEPTC);
		hwstats->fcoedwrc += IXGBE_READ_REG(hw, IXGBE_FCOEDWRC);
		hwstats->fcoedwtc += IXGBE_READ_REG(hw, IXGBE_FCOEDWTC);
		/* Add up per cpu counters for total ddp aloc fail */
		if (adapter->fcoe.ddp_pool) {
			struct ixgbe_fcoe *fcoe = &adapter->fcoe;
			struct ixgbe_fcoe_ddp_pool *ddp_pool;
			unsigned int cpu;
			u64 noddp = 0, noddp_ext_buff = 0;
			for_each_possible_cpu(cpu) {
				ddp_pool = per_cpu_ptr(fcoe->ddp_pool, cpu);
				noddp += ddp_pool->noddp;
				noddp_ext_buff += ddp_pool->noddp_ext_buff;
			}
			hwstats->fcoe_noddp = noddp;
			hwstats->fcoe_noddp_ext_buff = noddp_ext_buff;
		}
#endif /* IXGBE_FCOE */
		break;
	default:
		break;
	}
	bprc = IXGBE_READ_REG(hw, IXGBE_BPRC);
	hwstats->bprc += bprc;
	hwstats->mprc += IXGBE_READ_REG(hw, IXGBE_MPRC);
	if (hw->mac.type == ixgbe_mac_82598EB)
		hwstats->mprc -= bprc;
	hwstats->roc += IXGBE_READ_REG(hw, IXGBE_ROC);
	hwstats->prc64 += IXGBE_READ_REG(hw, IXGBE_PRC64);
	hwstats->prc127 += IXGBE_READ_REG(hw, IXGBE_PRC127);
	hwstats->prc255 += IXGBE_READ_REG(hw, IXGBE_PRC255);
	hwstats->prc511 += IXGBE_READ_REG(hw, IXGBE_PRC511);
	hwstats->prc1023 += IXGBE_READ_REG(hw, IXGBE_PRC1023);
	hwstats->prc1522 += IXGBE_READ_REG(hw, IXGBE_PRC1522);
	hwstats->rlec += IXGBE_READ_REG(hw, IXGBE_RLEC);
	lxon = IXGBE_READ_REG(hw, IXGBE_LXONTXC);
	hwstats->lxontxc += lxon;
	lxoff = IXGBE_READ_REG(hw, IXGBE_LXOFFTXC);
	hwstats->lxofftxc += lxoff;
	hwstats->gptc += IXGBE_READ_REG(hw, IXGBE_GPTC);
	hwstats->mptc += IXGBE_READ_REG(hw, IXGBE_MPTC);
	/*
	 * 82598 errata - tx of flow control packets is included in tx counters
	 */
	xon_off_tot = lxon + lxoff;
	hwstats->gptc -= xon_off_tot;
	hwstats->mptc -= xon_off_tot;
	hwstats->gotc -= (xon_off_tot * (ETH_ZLEN + ETH_FCS_LEN));
	hwstats->ruc += IXGBE_READ_REG(hw, IXGBE_RUC);
	hwstats->rfc += IXGBE_READ_REG(hw, IXGBE_RFC);
	hwstats->rjc += IXGBE_READ_REG(hw, IXGBE_RJC);
	hwstats->tpr += IXGBE_READ_REG(hw, IXGBE_TPR);
	hwstats->ptc64 += IXGBE_READ_REG(hw, IXGBE_PTC64);
	hwstats->ptc64 -= xon_off_tot;
	hwstats->ptc127 += IXGBE_READ_REG(hw, IXGBE_PTC127);
	hwstats->ptc255 += IXGBE_READ_REG(hw, IXGBE_PTC255);
	hwstats->ptc511 += IXGBE_READ_REG(hw, IXGBE_PTC511);
	hwstats->ptc1023 += IXGBE_READ_REG(hw, IXGBE_PTC1023);
	hwstats->ptc1522 += IXGBE_READ_REG(hw, IXGBE_PTC1522);
	hwstats->bptc += IXGBE_READ_REG(hw, IXGBE_BPTC);

	/* Fill out the OS statistics structure */
	netdev->stats.multicast = hwstats->mprc;

	/* Rx Errors */
	netdev->stats.rx_errors = hwstats->crcerrs + hwstats->rlec;
	netdev->stats.rx_dropped = 0;
	netdev->stats.rx_length_errors = hwstats->rlec;
	netdev->stats.rx_crc_errors = hwstats->crcerrs;
	netdev->stats.rx_missed_errors = total_mpc;

	/* VF Stats Collection - skip while resetting because these
	 * are not clear on read and otherwise you'll sometimes get
	 * crazy values.
	 */
	if (!test_bit(__IXGBE_RESETTING, &adapter->state)) {
		for (i = 0; i < adapter->num_vfs; i++) {
			UPDATE_VF_COUNTER_32bit(IXGBE_PVFGPRC(i),
						adapter->vfinfo[i].last_vfstats.gprc,
						adapter->vfinfo[i].vfstats.gprc);
			UPDATE_VF_COUNTER_32bit(IXGBE_PVFGPTC(i),
						adapter->vfinfo[i].last_vfstats.gptc,
						adapter->vfinfo[i].vfstats.gptc);
			UPDATE_VF_COUNTER_36bit(IXGBE_PVFGORC_LSB(i),
						IXGBE_PVFGORC_MSB(i),
						adapter->vfinfo[i].last_vfstats.gorc,
						adapter->vfinfo[i].vfstats.gorc);
			UPDATE_VF_COUNTER_36bit(IXGBE_PVFGOTC_LSB(i),
						IXGBE_PVFGOTC_MSB(i),
						adapter->vfinfo[i].last_vfstats.gotc,
						adapter->vfinfo[i].vfstats.gotc);
			UPDATE_VF_COUNTER_32bit(IXGBE_PVFMPRC(i),
						adapter->vfinfo[i].last_vfstats.mprc,
						adapter->vfinfo[i].vfstats.mprc);
		}
	}
}

/**
 * ixgbe_fdir_reinit_subtask - worker thread to reinit FDIR filter table
 * @adapter: pointer to the device adapter structure
 **/
static void ixgbe_fdir_reinit_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int i;

	if (!(adapter->flags2 & IXGBE_FLAG2_FDIR_REQUIRES_REINIT))
		return;

	adapter->flags2 &= ~IXGBE_FLAG2_FDIR_REQUIRES_REINIT;

	/* if interface is down do nothing */
	if (test_bit(__IXGBE_DOWN, &adapter->state))
		return;

	/* do nothing if we are not using signature filters */
	if (!(adapter->flags & IXGBE_FLAG_FDIR_HASH_CAPABLE))
		return;

	adapter->fdir_overflow++;

	if (ixgbe_reinit_fdir_tables_82599(hw) == 0) {
		for (i = 0; i < adapter->num_tx_queues; i++)
			set_bit(__IXGBE_TX_FDIR_INIT_DONE,
				&(adapter->tx_ring[i]->state));
		for (i = 0; i < adapter->num_xdp_queues; i++)
			set_bit(__IXGBE_TX_FDIR_INIT_DONE,
				&adapter->xdp_ring[i]->state);
		/* re-enable flow director interrupts */
		IXGBE_WRITE_REG(hw, IXGBE_EIMS, IXGBE_EIMS_FLOW_DIR);
	} else {
		e_err(probe, "failed to finish FDIR re-initialization, "
		      "ignored adding FDIR ATR filters\n");
	}
}

/**
 * ixgbe_check_hang_subtask - check for hung queues and dropped interrupts
 * @adapter: pointer to the device adapter structure
 *
 * This function serves two purposes.  First it strobes the interrupt lines
 * in order to make certain interrupts are occurring.  Secondly it sets the
 * bits needed to check for TX hangs.  As a result we should immediately
 * determine if a hang has occurred.
 */
static void ixgbe_check_hang_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u64 eics = 0;
	int i;

	/* If we're down, removing or resetting, just bail */
	if (test_bit(__IXGBE_DOWN, &adapter->state) ||
	    test_bit(__IXGBE_REMOVING, &adapter->state) ||
	    test_bit(__IXGBE_RESETTING, &adapter->state))
		return;

	/* Force detection of hung controller */
	if (netif_carrier_ok(adapter->netdev)) {
		for (i = 0; i < adapter->num_tx_queues; i++)
			set_check_for_tx_hang(adapter->tx_ring[i]);
		for (i = 0; i < adapter->num_xdp_queues; i++)
			set_check_for_tx_hang(adapter->xdp_ring[i]);
	}

	if (!(adapter->flags & IXGBE_FLAG_MSIX_ENABLED)) {
		/*
		 * for legacy and MSI interrupts don't set any bits
		 * that are enabled for EIAM, because this operation
		 * would set *both* EIMS and EICS for any bit in EIAM
		 */
		IXGBE_WRITE_REG(hw, IXGBE_EICS,
			(IXGBE_EICS_TCP_TIMER | IXGBE_EICS_OTHER));
	} else {
		/* get one bit for every active tx/rx interrupt vector */
		for (i = 0; i < adapter->num_q_vectors; i++) {
			struct ixgbe_q_vector *qv = adapter->q_vector[i];
			if (qv->rx.ring || qv->tx.ring)
				eics |= BIT_ULL(i);
		}
	}

	/* Cause software interrupt to ensure rings are cleaned */
	ixgbe_irq_rearm_queues(adapter, eics);
}

/**
 * ixgbe_watchdog_update_link - update the link status
 * @adapter: pointer to the device adapter structure
 **/
static void ixgbe_watchdog_update_link(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 link_speed = adapter->link_speed;
	bool link_up = adapter->link_up;
	bool pfc_en = adapter->dcb_cfg.pfc_mode_enable;

	if (!(adapter->flags & IXGBE_FLAG_NEED_LINK_UPDATE))
		return;

	if (hw->mac.ops.check_link) {
		hw->mac.ops.check_link(hw, &link_speed, &link_up, false);
	} else {
		/* always assume link is up, if no check link function */
		link_speed = IXGBE_LINK_SPEED_10GB_FULL;
		link_up = true;
	}

	if (adapter->ixgbe_ieee_pfc)
		pfc_en |= !!(adapter->ixgbe_ieee_pfc->pfc_en);

	if (link_up && !((adapter->flags & IXGBE_FLAG_DCB_ENABLED) && pfc_en)) {
		hw->mac.ops.fc_enable(hw);
		ixgbe_set_rx_drop_en(adapter);
	}

	if (link_up ||
	    time_after(jiffies, (adapter->link_check_timeout +
				 IXGBE_TRY_LINK_TIMEOUT))) {
		adapter->flags &= ~IXGBE_FLAG_NEED_LINK_UPDATE;
		IXGBE_WRITE_REG(hw, IXGBE_EIMS, IXGBE_EIMC_LSC);
		IXGBE_WRITE_FLUSH(hw);
	}

	adapter->link_up = link_up;
	adapter->link_speed = link_speed;
}

static void ixgbe_update_default_up(struct ixgbe_adapter *adapter)
{
#ifdef CONFIG_IXGBE_DCB
	struct net_device *netdev = adapter->netdev;
	struct dcb_app app = {
			      .selector = IEEE_8021QAZ_APP_SEL_ETHERTYPE,
			      .protocol = 0,
			     };
	u8 up = 0;

	if (adapter->dcbx_cap & DCB_CAP_DCBX_VER_IEEE)
		up = dcb_ieee_getapp_mask(netdev, &app);

	adapter->default_up = (up > 1) ? (ffs(up) - 1) : 0;
#endif
}

/**
 * ixgbe_watchdog_link_is_up - update netif_carrier status and
 *                             print link up message
 * @adapter: pointer to the device adapter structure
 **/
static void ixgbe_watchdog_link_is_up(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;
	u32 link_speed = adapter->link_speed;
	const char *speed_str;
	bool flow_rx, flow_tx;

	/* only continue if link was previously down */
	if (netif_carrier_ok(netdev))
		return;

	adapter->flags2 &= ~IXGBE_FLAG2_SEARCH_FOR_SFP;

	switch (hw->mac.type) {
	case ixgbe_mac_82598EB: {
		u32 frctl = IXGBE_READ_REG(hw, IXGBE_FCTRL);
		u32 rmcs = IXGBE_READ_REG(hw, IXGBE_RMCS);
		flow_rx = !!(frctl & IXGBE_FCTRL_RFCE);
		flow_tx = !!(rmcs & IXGBE_RMCS_TFCE_802_3X);
	}
		break;
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
	case ixgbe_mac_82599EB: {
		u32 mflcn = IXGBE_READ_REG(hw, IXGBE_MFLCN);
		u32 fccfg = IXGBE_READ_REG(hw, IXGBE_FCCFG);
		flow_rx = !!(mflcn & IXGBE_MFLCN_RFCE);
		flow_tx = !!(fccfg & IXGBE_FCCFG_TFCE_802_3X);
	}
		break;
	default:
		flow_tx = false;
		flow_rx = false;
		break;
	}

	adapter->last_rx_ptp_check = jiffies;

	if (test_bit(__IXGBE_PTP_RUNNING, &adapter->state))
		ixgbe_ptp_start_cyclecounter(adapter);

	switch (link_speed) {
	case IXGBE_LINK_SPEED_10GB_FULL:
		speed_str = "10 Gbps";
		break;
	case IXGBE_LINK_SPEED_5GB_FULL:
		speed_str = "5 Gbps";
		break;
	case IXGBE_LINK_SPEED_2_5GB_FULL:
		speed_str = "2.5 Gbps";
		break;
	case IXGBE_LINK_SPEED_1GB_FULL:
		speed_str = "1 Gbps";
		break;
	case IXGBE_LINK_SPEED_100_FULL:
		speed_str = "100 Mbps";
		break;
	case IXGBE_LINK_SPEED_10_FULL:
		speed_str = "10 Mbps";
		break;
	default:
		speed_str = "unknown speed";
		break;
	}
	e_info(drv, "NIC Link is Up %s, Flow Control: %s\n", speed_str,
	       ((flow_rx && flow_tx) ? "RX/TX" :
	       (flow_rx ? "RX" :
	       (flow_tx ? "TX" : "None"))));

	netif_carrier_on(netdev);
	ixgbe_check_vf_rate_limit(adapter);

	/* enable transmits */
	netif_tx_wake_all_queues(adapter->netdev);

	/* update the default user priority for VFs */
	ixgbe_update_default_up(adapter);

	/* ping all the active vfs to let them know link has changed */
	ixgbe_ping_all_vfs(adapter);
}

/**
 * ixgbe_watchdog_link_is_down - update netif_carrier status and
 *                               print link down message
 * @adapter: pointer to the adapter structure
 **/
static void ixgbe_watchdog_link_is_down(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	struct ixgbe_hw *hw = &adapter->hw;

	adapter->link_up = false;
	adapter->link_speed = 0;

	/* only continue if link was up previously */
	if (!netif_carrier_ok(netdev))
		return;

	/* poll for SFP+ cable when link is down */
	if (ixgbe_is_sfp(hw) && hw->mac.type == ixgbe_mac_82598EB)
		adapter->flags2 |= IXGBE_FLAG2_SEARCH_FOR_SFP;

	if (test_bit(__IXGBE_PTP_RUNNING, &adapter->state))
		ixgbe_ptp_start_cyclecounter(adapter);

	e_info(drv, "NIC Link is Down\n");
	netif_carrier_off(netdev);

	/* ping all the active vfs to let them know link has changed */
	ixgbe_ping_all_vfs(adapter);
}

static bool ixgbe_ring_tx_pending(struct ixgbe_adapter *adapter)
{
	int i;

	for (i = 0; i < adapter->num_tx_queues; i++) {
		struct ixgbe_ring *tx_ring = adapter->tx_ring[i];

		if (tx_ring->next_to_use != tx_ring->next_to_clean)
			return true;
	}

	for (i = 0; i < adapter->num_xdp_queues; i++) {
		struct ixgbe_ring *ring = adapter->xdp_ring[i];

		if (ring->next_to_use != ring->next_to_clean)
			return true;
	}

	return false;
}

static bool ixgbe_vf_tx_pending(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_ring_feature *vmdq = &adapter->ring_feature[RING_F_VMDQ];
	u32 q_per_pool = __ALIGN_MASK(1, ~vmdq->mask);

	int i, j;

	if (!adapter->num_vfs)
		return false;

	/* resetting the PF is only needed for MAC before X550 */
	if (hw->mac.type >= ixgbe_mac_X550)
		return false;

	for (i = 0; i < adapter->num_vfs; i++) {
		for (j = 0; j < q_per_pool; j++) {
			u32 h, t;

			h = IXGBE_READ_REG(hw, IXGBE_PVFTDHN(q_per_pool, i, j));
			t = IXGBE_READ_REG(hw, IXGBE_PVFTDTN(q_per_pool, i, j));

			if (h != t)
				return true;
		}
	}

	return false;
}

/**
 * ixgbe_watchdog_flush_tx - flush queues on link down
 * @adapter: pointer to the device adapter structure
 **/
static void ixgbe_watchdog_flush_tx(struct ixgbe_adapter *adapter)
{
	if (!netif_carrier_ok(adapter->netdev)) {
		if (ixgbe_ring_tx_pending(adapter) ||
		    ixgbe_vf_tx_pending(adapter)) {
			/* We've lost link, so the controller stops DMA,
			 * but we've got queued Tx work that's never going
			 * to get done, so reset controller to flush Tx.
			 * (Do the reset outside of interrupt context).
			 */
			e_warn(drv, "initiating reset to clear Tx work after link loss\n");
			set_bit(__IXGBE_RESET_REQUESTED, &adapter->state);
		}
	}
}

#ifdef CONFIG_PCI_IOV
static void ixgbe_bad_vf_abort(struct ixgbe_adapter *adapter, u32 vf)
{
	struct ixgbe_hw *hw = &adapter->hw;

	if (adapter->hw.mac.type == ixgbe_mac_82599EB &&
	    adapter->flags2 & IXGBE_FLAG2_AUTO_DISABLE_VF) {
		adapter->vfinfo[vf].primary_abort_count++;
		if (adapter->vfinfo[vf].primary_abort_count ==
		    IXGBE_PRIMARY_ABORT_LIMIT) {
			ixgbe_set_vf_link_state(adapter, vf,
						IFLA_VF_LINK_STATE_DISABLE);
			adapter->vfinfo[vf].primary_abort_count = 0;

			e_info(drv,
			       "Malicious Driver Detection event detected on PF %d VF %d MAC: %pM mdd-disable-vf=on",
			       hw->bus.func, vf,
			       adapter->vfinfo[vf].vf_mac_addresses);
		}
	}
}

static void ixgbe_check_for_bad_vf(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct pci_dev *pdev = adapter->pdev;
	unsigned int vf;
	u32 gpc;

	if (!(netif_carrier_ok(adapter->netdev)))
		return;

	gpc = IXGBE_READ_REG(hw, IXGBE_TXDGPC);
	if (gpc) /* If incrementing then no need for the check below */
		return;
	/* Check to see if a bad DMA write target from an errant or
	 * malicious VF has caused a PCIe error.  If so then we can
	 * issue a VFLR to the offending VF(s) and then resume without
	 * requesting a full slot reset.
	 */

	if (!pdev)
		return;

	/* check status reg for all VFs owned by this PF */
	for (vf = 0; vf < adapter->num_vfs; ++vf) {
		struct pci_dev *vfdev = adapter->vfinfo[vf].vfdev;
		u16 status_reg;

		if (!vfdev)
			continue;
		pci_read_config_word(vfdev, PCI_STATUS, &status_reg);
		if (status_reg != IXGBE_FAILED_READ_CFG_WORD &&
		    status_reg & PCI_STATUS_REC_MASTER_ABORT) {
			ixgbe_bad_vf_abort(adapter, vf);
			pcie_flr(vfdev);
		}
	}
}

static void ixgbe_spoof_check(struct ixgbe_adapter *adapter)
{
	u32 ssvpc;

	/* Do not perform spoof check for 82598 or if not in IOV mode */
	if (adapter->hw.mac.type == ixgbe_mac_82598EB ||
	    adapter->num_vfs == 0)
		return;

	ssvpc = IXGBE_READ_REG(&adapter->hw, IXGBE_SSVPC);

	/*
	 * ssvpc register is cleared on read, if zero then no
	 * spoofed packets in the last interval.
	 */
	if (!ssvpc)
		return;

	e_warn(drv, "%u Spoofed packets detected\n", ssvpc);
}
#else
static void ixgbe_spoof_check(struct ixgbe_adapter __always_unused *adapter)
{
}

static void
ixgbe_check_for_bad_vf(struct ixgbe_adapter __always_unused *adapter)
{
}
#endif /* CONFIG_PCI_IOV */


/**
 * ixgbe_watchdog_subtask - check and bring link up
 * @adapter: pointer to the device adapter structure
 **/
static void ixgbe_watchdog_subtask(struct ixgbe_adapter *adapter)
{
	/* if interface is down, removing or resetting, do nothing */
	if (test_bit(__IXGBE_DOWN, &adapter->state) ||
	    test_bit(__IXGBE_REMOVING, &adapter->state) ||
	    test_bit(__IXGBE_RESETTING, &adapter->state))
		return;

	ixgbe_watchdog_update_link(adapter);

	if (adapter->link_up)
		ixgbe_watchdog_link_is_up(adapter);
	else
		ixgbe_watchdog_link_is_down(adapter);

	ixgbe_check_for_bad_vf(adapter);
	ixgbe_spoof_check(adapter);
	ixgbe_update_stats(adapter);

	ixgbe_watchdog_flush_tx(adapter);
}

/**
 * ixgbe_sfp_detection_subtask - poll for SFP+ cable
 * @adapter: the ixgbe adapter structure
 **/
static void ixgbe_sfp_detection_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	int err;

	/* not searching for SFP so there is nothing to do here */
	if (!(adapter->flags2 & IXGBE_FLAG2_SEARCH_FOR_SFP) &&
	    !(adapter->flags2 & IXGBE_FLAG2_SFP_NEEDS_RESET))
		return;

	if (adapter->sfp_poll_time &&
	    time_after(adapter->sfp_poll_time, jiffies))
		return; /* If not yet time to poll for SFP */

	/* someone else is in init, wait until next service event */
	if (test_and_set_bit(__IXGBE_IN_SFP_INIT, &adapter->state))
		return;

	adapter->sfp_poll_time = jiffies + IXGBE_SFP_POLL_JIFFIES - 1;

	err = hw->phy.ops.identify_sfp(hw);
	if (err == -EOPNOTSUPP)
		goto sfp_out;

	if (err == -ENOENT) {
		/* If no cable is present, then we need to reset
		 * the next time we find a good cable. */
		adapter->flags2 |= IXGBE_FLAG2_SFP_NEEDS_RESET;
	}

	/* exit on error */
	if (err)
		goto sfp_out;

	/* exit if reset not needed */
	if (!(adapter->flags2 & IXGBE_FLAG2_SFP_NEEDS_RESET))
		goto sfp_out;

	adapter->flags2 &= ~IXGBE_FLAG2_SFP_NEEDS_RESET;

	/*
	 * A module may be identified correctly, but the EEPROM may not have
	 * support for that module.  setup_sfp() will fail in that case, so
	 * we should not allow that module to load.
	 */
	if (hw->mac.type == ixgbe_mac_82598EB)
		err = hw->phy.ops.reset(hw);
	else
		err = hw->mac.ops.setup_sfp(hw);

	if (err == -EOPNOTSUPP)
		goto sfp_out;

	adapter->flags |= IXGBE_FLAG_NEED_LINK_CONFIG;
	e_info(probe, "detected SFP+: %d\n", hw->phy.sfp_type);

sfp_out:
	clear_bit(__IXGBE_IN_SFP_INIT, &adapter->state);

	if (err == -EOPNOTSUPP &&
	    adapter->netdev->reg_state == NETREG_REGISTERED) {
		e_dev_err("failed to initialize because an unsupported "
			  "SFP+ module type was detected.\n");
		e_dev_err("Reload the driver after installing a "
			  "supported module.\n");
		unregister_netdev(adapter->netdev);
	}
}

/**
 * ixgbe_sfp_link_config_subtask - set up link SFP after module install
 * @adapter: the ixgbe adapter structure
 **/
static void ixgbe_sfp_link_config_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 cap_speed;
	u32 speed;
	bool autoneg = false;

	if (!(adapter->flags & IXGBE_FLAG_NEED_LINK_CONFIG))
		return;

	/* someone else is in init, wait until next service event */
	if (test_and_set_bit(__IXGBE_IN_SFP_INIT, &adapter->state))
		return;

	adapter->flags &= ~IXGBE_FLAG_NEED_LINK_CONFIG;

	hw->mac.ops.get_link_capabilities(hw, &cap_speed, &autoneg);

	/* advertise highest capable link speed */
	if (!autoneg && (cap_speed & IXGBE_LINK_SPEED_10GB_FULL))
		speed = IXGBE_LINK_SPEED_10GB_FULL;
	else
		speed = cap_speed & (IXGBE_LINK_SPEED_10GB_FULL |
				     IXGBE_LINK_SPEED_1GB_FULL);

	if (hw->mac.ops.setup_link)
		hw->mac.ops.setup_link(hw, speed, true);

	adapter->flags |= IXGBE_FLAG_NEED_LINK_UPDATE;
	adapter->link_check_timeout = jiffies;
	clear_bit(__IXGBE_IN_SFP_INIT, &adapter->state);
}

/**
 * ixgbe_service_timer - Timer Call-back
 * @t: pointer to timer_list structure
 **/
static void ixgbe_service_timer(struct timer_list *t)
{
	struct ixgbe_adapter *adapter = from_timer(adapter, t, service_timer);
	unsigned long next_event_offset;

	/* poll faster when waiting for link */
	if (adapter->flags & IXGBE_FLAG_NEED_LINK_UPDATE)
		next_event_offset = HZ / 10;
	else
		next_event_offset = HZ * 2;

	/* Reset the timer */
	mod_timer(&adapter->service_timer, next_event_offset + jiffies);

	ixgbe_service_event_schedule(adapter);
}

static void ixgbe_phy_interrupt_subtask(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	bool overtemp;

	if (!(adapter->flags2 & IXGBE_FLAG2_PHY_INTERRUPT))
		return;

	adapter->flags2 &= ~IXGBE_FLAG2_PHY_INTERRUPT;

	if (!hw->phy.ops.handle_lasi)
		return;

	hw->phy.ops.handle_lasi(&adapter->hw, &overtemp);
	if (overtemp)
		e_crit(drv, "%s\n", ixgbe_overheat_msg);
}

static void ixgbe_reset_subtask(struct ixgbe_adapter *adapter)
{
	if (!test_and_clear_bit(__IXGBE_RESET_REQUESTED, &adapter->state))
		return;

	rtnl_lock();
	/* If we're already down, removing or resetting, just bail */
	if (test_bit(__IXGBE_DOWN, &adapter->state) ||
	    test_bit(__IXGBE_REMOVING, &adapter->state) ||
	    test_bit(__IXGBE_RESETTING, &adapter->state)) {
		rtnl_unlock();
		return;
	}

	ixgbe_dump(adapter);
	netdev_err(adapter->netdev, "Reset adapter\n");
	adapter->tx_timeout_count++;

	ixgbe_reinit_locked(adapter);
	rtnl_unlock();
}

/**
 * ixgbe_check_fw_error - Check firmware for errors
 * @adapter: the adapter private structure
 *
 * Check firmware errors in register FWSM
 */
static bool ixgbe_check_fw_error(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 fwsm;

	/* read fwsm.ext_err_ind register and log errors */
	fwsm = IXGBE_READ_REG(hw, IXGBE_FWSM(hw));

	if (fwsm & IXGBE_FWSM_EXT_ERR_IND_MASK ||
	    !(fwsm & IXGBE_FWSM_FW_VAL_BIT))
		e_dev_warn("Warning firmware error detected FWSM: 0x%08X\n",
			   fwsm);

	if (hw->mac.ops.fw_recovery_mode && hw->mac.ops.fw_recovery_mode(hw)) {
		e_dev_err("Firmware recovery mode detected. Limiting functionality. Refer to the Intel(R) Ethernet Adapters and Devices User Guide for details on firmware recovery mode.\n");
		return true;
	}

	return false;
}

/**
 * ixgbe_service_task - manages and runs subtasks
 * @work: pointer to work_struct containing our data
 **/
static void ixgbe_service_task(struct work_struct *work)
{
	struct ixgbe_adapter *adapter = container_of(work,
						     struct ixgbe_adapter,
						     service_task);
	if (ixgbe_removed(adapter->hw.hw_addr)) {
		if (!test_bit(__IXGBE_DOWN, &adapter->state)) {
			rtnl_lock();
			ixgbe_down(adapter);
			rtnl_unlock();
		}
		ixgbe_service_event_complete(adapter);
		return;
	}
	if (ixgbe_check_fw_error(adapter)) {
		if (!test_bit(__IXGBE_DOWN, &adapter->state))
			unregister_netdev(adapter->netdev);
		ixgbe_service_event_complete(adapter);
		return;
	}
	if (adapter->hw.mac.type == ixgbe_mac_e610) {
		if (adapter->flags2 & IXGBE_FLAG2_FW_ASYNC_EVENT)
			ixgbe_handle_fw_event(adapter);
		ixgbe_check_media_subtask(adapter);
	}
	ixgbe_reset_subtask(adapter);
	ixgbe_phy_interrupt_subtask(adapter);
	ixgbe_sfp_detection_subtask(adapter);
	ixgbe_sfp_link_config_subtask(adapter);
	ixgbe_check_overtemp_subtask(adapter);
	ixgbe_watchdog_subtask(adapter);
	ixgbe_fdir_reinit_subtask(adapter);
	ixgbe_check_hang_subtask(adapter);

	if (test_bit(__IXGBE_PTP_RUNNING, &adapter->state)) {
		ixgbe_ptp_overflow_check(adapter);
		if (adapter->flags & IXGBE_FLAG_RX_HWTSTAMP_IN_REGISTER)
			ixgbe_ptp_rx_hang(adapter);
		ixgbe_ptp_tx_hang(adapter);
	}

	ixgbe_service_event_complete(adapter);
}

static int ixgbe_tso(struct ixgbe_ring *tx_ring,
		     struct ixgbe_tx_buffer *first,
		     u8 *hdr_len,
		     struct ixgbe_ipsec_tx_data *itd)
{
	u32 vlan_macip_lens, type_tucmd, mss_l4len_idx;
	struct sk_buff *skb = first->skb;
	union {
		struct iphdr *v4;
		struct ipv6hdr *v6;
		unsigned char *hdr;
	} ip;
	union {
		struct tcphdr *tcp;
		struct udphdr *udp;
		unsigned char *hdr;
	} l4;
	u32 paylen, l4_offset;
	u32 fceof_saidx = 0;
	int err;

	if (skb->ip_summed != CHECKSUM_PARTIAL)
		return 0;

	if (!skb_is_gso(skb))
		return 0;

	err = skb_cow_head(skb, 0);
	if (err < 0)
		return err;

	if (eth_p_mpls(first->protocol))
		ip.hdr = skb_inner_network_header(skb);
	else
		ip.hdr = skb_network_header(skb);
	l4.hdr = skb_checksum_start(skb);

	/* ADV DTYP TUCMD MKRLOC/ISCSIHEDLEN */
	type_tucmd = (skb_shinfo(skb)->gso_type & SKB_GSO_UDP_L4) ?
		      IXGBE_ADVTXD_TUCMD_L4T_UDP : IXGBE_ADVTXD_TUCMD_L4T_TCP;

	/* initialize outer IP header fields */
	if (ip.v4->version == 4) {
		unsigned char *csum_start = skb_checksum_start(skb);
		unsigned char *trans_start = ip.hdr + (ip.v4->ihl * 4);
		int len = csum_start - trans_start;

		/* IP header will have to cancel out any data that
		 * is not a part of the outer IP header, so set to
		 * a reverse csum if needed, else init check to 0.
		 */
		ip.v4->check = (skb_shinfo(skb)->gso_type & SKB_GSO_PARTIAL) ?
					   csum_fold(csum_partial(trans_start,
								  len, 0)) : 0;
		type_tucmd |= IXGBE_ADVTXD_TUCMD_IPV4;

		ip.v4->tot_len = 0;
		first->tx_flags |= IXGBE_TX_FLAGS_TSO |
				   IXGBE_TX_FLAGS_CSUM |
				   IXGBE_TX_FLAGS_IPV4;
	} else {
		ip.v6->payload_len = 0;
		first->tx_flags |= IXGBE_TX_FLAGS_TSO |
				   IXGBE_TX_FLAGS_CSUM;
	}

	/* determine offset of inner transport header */
	l4_offset = l4.hdr - skb->data;

	/* remove payload length from inner checksum */
	paylen = skb->len - l4_offset;

	if (type_tucmd & IXGBE_ADVTXD_TUCMD_L4T_TCP) {
		/* compute length of segmentation header */
		*hdr_len = (l4.tcp->doff * 4) + l4_offset;
		csum_replace_by_diff(&l4.tcp->check,
				     (__force __wsum)htonl(paylen));
	} else {
		/* compute length of segmentation header */
		*hdr_len = sizeof(*l4.udp) + l4_offset;
		csum_replace_by_diff(&l4.udp->check,
				     (__force __wsum)htonl(paylen));
	}

	/* update gso size and bytecount with header size */
	first->gso_segs = skb_shinfo(skb)->gso_segs;
	first->bytecount += (first->gso_segs - 1) * *hdr_len;

	/* mss_l4len_id: use 0 as index for TSO */
	mss_l4len_idx = (*hdr_len - l4_offset) << IXGBE_ADVTXD_L4LEN_SHIFT;
	mss_l4len_idx |= skb_shinfo(skb)->gso_size << IXGBE_ADVTXD_MSS_SHIFT;

	fceof_saidx |= itd->sa_idx;
	type_tucmd |= itd->flags | itd->trailer_len;

	/* vlan_macip_lens: HEADLEN, MACLEN, VLAN tag */
	vlan_macip_lens = l4.hdr - ip.hdr;
	vlan_macip_lens |= (ip.hdr - skb->data) << IXGBE_ADVTXD_MACLEN_SHIFT;
	vlan_macip_lens |= first->tx_flags & IXGBE_TX_FLAGS_VLAN_MASK;

	ixgbe_tx_ctxtdesc(tx_ring, vlan_macip_lens, fceof_saidx, type_tucmd,
			  mss_l4len_idx);

	return 1;
}

static void ixgbe_tx_csum(struct ixgbe_ring *tx_ring,
			  struct ixgbe_tx_buffer *first,
			  struct ixgbe_ipsec_tx_data *itd)
{
	struct sk_buff *skb = first->skb;
	u32 vlan_macip_lens = 0;
	u32 fceof_saidx = 0;
	u32 type_tucmd = 0;

	if (skb->ip_summed != CHECKSUM_PARTIAL) {
csum_failed:
		if (!(first->tx_flags & (IXGBE_TX_FLAGS_HW_VLAN |
					 IXGBE_TX_FLAGS_CC)))
			return;
		goto no_csum;
	}

	switch (skb->csum_offset) {
	case offsetof(struct tcphdr, check):
		type_tucmd = IXGBE_ADVTXD_TUCMD_L4T_TCP;
		fallthrough;
	case offsetof(struct udphdr, check):
		break;
	case offsetof(struct sctphdr, checksum):
		/* validate that this is actually an SCTP request */
		if (skb_csum_is_sctp(skb)) {
			type_tucmd = IXGBE_ADVTXD_TUCMD_L4T_SCTP;
			break;
		}
		fallthrough;
	default:
		skb_checksum_help(skb);
		goto csum_failed;
	}

	/* update TX checksum flag */
	first->tx_flags |= IXGBE_TX_FLAGS_CSUM;
	vlan_macip_lens = skb_checksum_start_offset(skb) -
			  skb_network_offset(skb);
no_csum:
	/* vlan_macip_lens: MACLEN, VLAN tag */
	vlan_macip_lens |= skb_network_offset(skb) << IXGBE_ADVTXD_MACLEN_SHIFT;
	vlan_macip_lens |= first->tx_flags & IXGBE_TX_FLAGS_VLAN_MASK;

	fceof_saidx |= itd->sa_idx;
	type_tucmd |= itd->flags | itd->trailer_len;

	ixgbe_tx_ctxtdesc(tx_ring, vlan_macip_lens, fceof_saidx, type_tucmd, 0);
}

static inline __maybe_unused
u32 ixgbe_tx_cmd_type(const struct sk_buff *skb, u32 tx_flags)
{
	u32 cmd = IXGBE_ADVTXD_DTYP_DATA |
	IXGBE_ADVTXD_DCMD_DEXT |
	IXGBE_ADVTXD_DCMD_IFCS;

	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_HW_VLAN,
						  IXGBE_ADVTXD_DCMD_VLE);
	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_TSO,
						  IXGBE_ADVTXD_DCMD_TSE);
	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_TSTAMP,
						  IXGBE_ADVTXD_MAC_TSTAMP);

	if (skb->no_fcs)
		cmd &= ~IXGBE_ADVTXD_DCMD_IFCS;

	return cmd;
}

static __maybe_unused void ixgbe_tx_olinfo_status(union ixgbe_adv_tx_desc *tx_desc,
				   u32 tx_flags, unsigned int paylen)
{
	u32 olinfo_status = paylen << IXGBE_ADVTXD_PAYLEN_SHIFT;

	/* enable L4 checksum for TSO and TX checksum offload */
	olinfo_status |= IXGBE_SET_FLAG(tx_flags,
					IXGBE_TX_FLAGS_CSUM,
					IXGBE_ADVTXD_POPTS_TXSM);

	/* enable IPv4 checksum for TSO */
	olinfo_status |= IXGBE_SET_FLAG(tx_flags,
					IXGBE_TX_FLAGS_IPV4,
					IXGBE_ADVTXD_POPTS_IXSM);

	/* enable IPsec */
	olinfo_status |= IXGBE_SET_FLAG(tx_flags,
					IXGBE_TX_FLAGS_IPSEC,
					IXGBE_ADVTXD_POPTS_IPSEC);

	/*
	 * Check Context must be set if Tx switch is enabled, which it
	 * always is for case where virtual functions are running
	 */
	olinfo_status |= IXGBE_SET_FLAG(tx_flags,
					IXGBE_TX_FLAGS_CC,
					IXGBE_ADVTXD_CC);

	tx_desc->read.olinfo_status = cpu_to_le32(olinfo_status);
}

static int __ixgbe_maybe_stop_tx(struct ixgbe_ring *tx_ring, u16 need)
{
	if (likely(ixgbe_desc_unused(tx_ring) >= need))
		return 0;

	if (netif_subqueue_try_stop(tx_ring->netdev,
		tx_ring->queue_index,
		ixgbe_desc_unused(tx_ring), need))
	{
		tx_ring->tx_stats.restart_queue++;
		return -EBUSY;
	}

	return 0;
}

static inline u32 ixgbe_tx_desc_cmd_type(const struct sk_buff *skb,
										 u32 tx_flags)
{
	u32 cmd = IXGBE_ADVTXD_DTYP_DATA |
	IXGBE_ADVTXD_DCMD_DEXT |
	IXGBE_ADVTXD_DCMD_IFCS;

	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_HW_VLAN,
						  IXGBE_ADVTXD_DCMD_VLE);
	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_TSO,
						  IXGBE_ADVTXD_DCMD_TSE);
	cmd |= IXGBE_SET_FLAG(tx_flags, IXGBE_TX_FLAGS_TSTAMP,
						  IXGBE_ADVTXD_MAC_TSTAMP);

	if (skb->no_fcs)
		cmd &= ~IXGBE_ADVTXD_DCMD_IFCS;

	return cmd;
}

static inline int ixgbe_maybe_stop_tx(struct ixgbe_ring *tx_ring, u16 size)
{
	if (likely(ixgbe_desc_unused(tx_ring) >= size))
		return 0;

	return __ixgbe_maybe_stop_tx(tx_ring, size);
}

/* --------------------------------------------------------------- */
/* DMA helpers – fall back if RELAXED is not available             */
/* --------------------------------------------------------------- */
#ifndef DMA_ATTR_RELAXED
#ifdef DMA_ATTR_WEAK_ORDERING
#define DMA_ATTR_RELAXED DMA_ATTR_WEAK_ORDERING
#endif
#endif

#ifdef DMA_ATTR_RELAXED
#define IXGBE_MAP_SINGLE(dev, buf, len) \
dma_map_single_attrs((dev), (buf), (len), DMA_TO_DEVICE, \
DMA_ATTR_RELAXED)
#define IXGBE_MAP_PAGE(dev, page, off, len) \
dma_map_page_attrs((dev), (page), (off), (len), \
DMA_TO_DEVICE, DMA_ATTR_RELAXED)
#define IXGBE_UNMAP_SINGLE(dev, addr, len) \
dma_unmap_single_attrs((dev), (addr), (len), \
DMA_TO_DEVICE, DMA_ATTR_RELAXED)
#define IXGBE_UNMAP_PAGE(dev, addr, len) \
dma_unmap_page_attrs((dev), (addr), (len), \
DMA_TO_DEVICE, DMA_ATTR_RELAXED)
#else
#define IXGBE_MAP_SINGLE(dev, buf, len) \
dma_map_single((dev), (buf), (len), DMA_TO_DEVICE)
#define IXGBE_MAP_PAGE(dev, page, off, len) \
dma_map_page((dev), (page), (off), (len), DMA_TO_DEVICE)
#define IXGBE_UNMAP_SINGLE(dev, addr, len) \
dma_unmap_single((dev), (addr), (len), DMA_TO_DEVICE)
#define IXGBE_UNMAP_PAGE(dev, addr, len) \
dma_unmap_page((dev), (addr), (len), DMA_TO_DEVICE)
#endif

/**
 * ixgbe_tx_map - map an skb onto Tx descriptors
 * @tx_ring : ring to push onto
 * @first   : first tx_buffer slot (skb, flags, bytecount already set)
 * @hdr_len : L2-L4 header length used for PAYLEN in olinfo
 *
 * Returns 0 on success, -1 on DMA error (skb freed, ring rolled back).
 */
static int ixgbe_tx_map(struct ixgbe_ring *tx_ring,
                        struct ixgbe_tx_buffer *first,
                        const u8 hdr_len) {
    struct sk_buff *skb = first->skb;
    union ixgbe_adv_tx_desc *txd;
    struct ixgbe_tx_buffer *txb;
    dma_addr_t dma;
    u32 cmd_base;
    unsigned int seg_len;
    u16 i, first_idx, last_idx;

    cmd_base = ixgbe_tx_desc_cmd_type(skb, first->tx_flags);
    i = tx_ring->next_to_use;
    first_idx = i;
    last_idx = i;

    txd = IXGBE_TX_DESC(tx_ring, i);
    txb = first;

    ixgbe_tx_olinfo_status(txd, first->tx_flags, skb->len - hdr_len);

    /* ---------- linear data ------------------------------------- */
    seg_len = skb_headlen(skb);
#ifdef IXGBE_FCOE
    if (first->tx_flags & IXGBE_TX_FLAGS_FCOE &&
        skb->data_len < sizeof(struct fcoe_crc_eof)) {
        seg_len = max(0, (int)seg_len - (int)(sizeof(struct fcoe_crc_eof) - skb->data_len));  /* Clamp to prevent underflow */
    }
#endif
    if (seg_len) {
        dma = ixgbe_dma_map_single(tx_ring->dev, skb->data, seg_len, DMA_TO_DEVICE);
        if (unlikely(dma_mapping_error(tx_ring->dev, dma))) {  /* Rare in gaming */
            goto dma_err;
        }

        dma_unmap_len_set(txb, len, seg_len);
        dma_unmap_addr_set(txb, dma, dma);

        do {
            u32 dlen = min(seg_len, IXGBE_MAX_DATA_PER_TXD);

            txd->read.buffer_addr = cpu_to_le64(dma);
            txd->read.cmd_type_len = cpu_to_le32(cmd_base | dlen);

            last_idx = i;
            seg_len -= dlen;
            if (!seg_len) {
                break;
            }

            dma += dlen;
            if (++i == tx_ring->count) {
                i = 0;
            }
            txd = IXGBE_TX_DESC(tx_ring, i);
            txb = &tx_ring->tx_buffer_info[i];
            *txb = (struct ixgbe_tx_buffer){0};
            txd->read.olinfo_status = 0;
            dma_unmap_len_set(txb, len, 0);
        } while (1);
    }

    /* ---------- fragments --------------------------------------- */
    for (u16 fn = 0; fn < skb_shinfo(skb)->nr_frags; fn++) {
        skb_frag_t *frag = &skb_shinfo(skb)->frags[fn];
        seg_len = skb_frag_size(frag);

        if (++i == tx_ring->count) {
            i = 0;
        }
        txd = IXGBE_TX_DESC(tx_ring, i);
        txb = &tx_ring->tx_buffer_info[i];
        *txb = (struct ixgbe_tx_buffer){0};
        txd->read.olinfo_status = 0;

        dma = ixgbe_dma_map_page(tx_ring->dev, skb_frag_page(frag),
                                 skb_frag_off(frag), seg_len, DMA_TO_DEVICE);
        if (unlikely(dma_mapping_error(tx_ring->dev, dma))) {  /* Rare in gaming */
            goto dma_err;
        }

        dma_unmap_len_set(txb, len, seg_len);
        dma_unmap_addr_set(txb, dma, dma);

        do {
            u32 dlen = min(seg_len, IXGBE_MAX_DATA_PER_TXD);

            txd->read.buffer_addr = cpu_to_le64(dma);
            txd->read.cmd_type_len = cpu_to_le32(cmd_base | dlen);

            last_idx = i;
            seg_len -= dlen;
            if (!seg_len) {
                break;
            }

            dma += dlen;
            if (++i == tx_ring->count) {
                i = 0;
            }
            txd = IXGBE_TX_DESC(tx_ring, i);
            txb = &tx_ring->tx_buffer_info[i];
            *txb = (struct ixgbe_tx_buffer){0};
            txd->read.olinfo_status = 0;
            dma_unmap_len_set(txb, len, 0);
        } while (1);
    }

    /* mark last descriptor */
    txd->read.cmd_type_len |= cpu_to_le32(IXGBE_TXD_CMD);
    first->next_to_watch = txd;

    /* look-ahead prefetch */
    {
        u16 look = last_idx + 1;
        if (look == tx_ring->count) {
            look = 0;
        }
        u16 pre = look + IXGBE_TX_PREFETCH_OFFSET;
        if (pre >= tx_ring->count) {
            pre -= tx_ring->count;
        }
        prefetchw(&tx_ring->tx_buffer_info[pre]);
        prefetch(IXGBE_TX_DESC(tx_ring, pre));
    }

    dma_wmb();  /* descriptors before tail (optimized for x540 PCIe) */

    if (++i == tx_ring->count) {
        i = 0;
    }
    tx_ring->next_to_use = i;

    netdev_tx_sent_queue(txring_txq(tx_ring), first->bytecount);

    ixgbe_maybe_stop_tx(tx_ring, DESC_NEEDED);
    if (netif_xmit_stopped(txring_txq(tx_ring)) || !netdev_xmit_more()) {
        writel(i, tx_ring->tail);
    }

    first->time_stamp = jiffies;
    skb_tx_timestamp(skb);

    return 0;

/* ---------- error unwind ---------------------------------------- */
dma_err:
    dev_err(tx_ring->dev, "ixgbe: TX DMA map failed\n");
    for (i = first_idx;; ) {
        txb = &tx_ring->tx_buffer_info[i];
        if (dma_unmap_len(txb, len)) {
            if (txb->skb) {
                ixgbe_dma_unmap_single(tx_ring->dev, dma_unmap_addr(txb, dma),
                                       dma_unmap_len(txb, len), DMA_TO_DEVICE);
            } else {
                ixgbe_dma_unmap_page(tx_ring->dev, dma_unmap_addr(txb, dma),
                                     dma_unmap_len(txb, len), DMA_TO_DEVICE);
            }
            dma_unmap_len_set(txb, len, 0);
        }
        if (i == last_idx) {
            break;
        }
        if (++i == tx_ring->count) {
            i = 0;
        }
    }
    tx_ring->next_to_use = first_idx;
    dev_kfree_skb_any(first->skb);
    first->skb = NULL;
    first->next_to_watch = NULL;
    return -1;
}

static void ixgbe_setup_gaming_atr(struct ixgbe_adapter *adapter) {
    struct ixgbe_hw *hw = &adapter->hw;
    char *ranges_copy;
    char *token;
    int total_ports = 0;
    int idx = 0;
    u16 soft_id = 0;  /* Incremental soft ID for FDIR to avoid collisions */
    struct { u16 start; u16 end; } port_ranges[8];  /* Max 8 ranges to avoid FDIR exhaustion */

    if (!gaming_atr_ranges || !*gaming_atr_ranges) {
        return;  /* Disabled if empty */
    }

    /* Safe copy to prevent param modification (avoids UAF) */
    ranges_copy = kstrdup(gaming_atr_ranges, GFP_KERNEL);
    if (!ranges_copy) {
        e_warn(drv, "Failed to allocate memory for gaming ATR ranges\n");
        return;
    }

    spin_lock(&adapter->fdir_perfect_lock);  /* Lock for concurrent safety (e.g., module reload) */

    /* Parse comma-separated ranges (e.g., "27000-27050,7777-7999") */
    while ((token = strsep(&ranges_copy, ",")) && idx < ARRAY_SIZE(port_ranges)) {
        char *dash = strchr(token, '-');
        u16 start, end;
        if (dash && !kstrtou16(token, 10, &start) && !kstrtou16(dash + 1, 10, &end) &&
            start < end && start > 0 && end <= 65535) {  /* Safe parsing, bounds check */
            port_ranges[idx].start = start;
            port_ranges[idx].end = end;
            total_ports += (end - start + 1);
            if (total_ports > 512) {  /* Cap to prevent FDIR exhaustion on x540 (8192 limit) */
                e_warn(drv, "Gaming ATR: Too many ports (%d), capping at 512\n", total_ports);
                break;
            }
            idx++;
        }
    }

    /* Program FDIR filters for UDP dst ports to low-latency queue (e.g., 0) */
    for (int i = 0; i < idx; i++) {
        for (u16 port = port_ranges[i].start; port <= port_ranges[i].end; port++) {
            union ixgbe_atr_input atr_input = {0};
            union ixgbe_atr_input mask = {0};

            atr_input.formatted.dst_port = htons(port);  /* Big-endian for hw */
            mask.formatted.dst_port = 0xFFFF;  /* Mask to match only dst_port */

            /* IPv4 UDP */
            atr_input.formatted.flow_type = IXGBE_ATR_FLOW_TYPE_UDPV4;
            ixgbe_atr_compute_perfect_hash_82599(&atr_input, &mask);
            ixgbe_fdir_write_perfect_filter_82599(hw, &atr_input, soft_id++, 0 /* gaming queue */);

            /* IPv6 UDP (for dual-stack games) */
            atr_input.formatted.flow_type = IXGBE_ATR_FLOW_TYPE_UDPV6;
            ixgbe_atr_compute_perfect_hash_82599(&atr_input, &mask);
            ixgbe_fdir_write_perfect_filter_82599(hw, &atr_input, soft_id++, 0 /* gaming queue */);
        }
    }

    spin_unlock(&adapter->fdir_perfect_lock);
    kfree(ranges_copy);  /* Safe free, no UAF */
}

static void ixgbe_atr(struct ixgbe_ring *ring,
                      struct ixgbe_tx_buffer *first) {
    struct ixgbe_q_vector *q_vector = ring->q_vector;
    union ixgbe_atr_hash_dword input = { .dword = 0 };
    union ixgbe_atr_hash_dword common = { .dword = 0 };
    union {
        unsigned char *network;
        struct iphdr *ipv4;
        struct ipv6hdr *ipv6;
    } hdr;
    struct tcphdr *th;
    unsigned int hlen;
    struct sk_buff *skb;
    const unsigned char *tail;  /* Opt #5: cache once */
    __be16 vlan_id;
    int l4_proto;

    /* Opt #5: likely() hints for dominant exits */
    if (unlikely(!q_vector)) {
        return;
    }

    if (unlikely(!ring->atr_sample_rate)) {
        return;
    }

    ring->atr_count++;

    /* Opt #5: likely() hints for dominant exits */
    if (unlikely((first->protocol != htons(ETH_P_IP)) &&
                 (first->protocol != htons(ETH_P_IPV6)))) {
        return;
    }

    skb = first->skb;
    hdr.network = skb_network_header(skb);
    /* Opt #5: One-shot skb_tail_pointer cache */
    tail = skb_tail_pointer(skb);

    if (unlikely(hdr.network <= skb->data)) {
        return;
    }

    /* Opt #5: Early header prefetch */
    prefetch(hdr.network);

    if (unlikely(skb->encapsulation &&  /* Rare in desktop/gaming */
                 first->protocol == htons(ETH_P_IP) &&
                 hdr.ipv4->protocol == IPPROTO_UDP)) {  /* Check for valid header access */
        struct ixgbe_adapter *adapter = q_vector->adapter;

        /* Use cached 'tail' */
        if (unlikely(tail < hdr.network + vxlan_headroom(0))) {
            return;
        }

        if (adapter->vxlan_port &&
            udp_hdr(skb)->dest == adapter->vxlan_port) {
            hdr.network = skb_inner_network_header(skb);
        }

        if (adapter->geneve_port &&
            udp_hdr(skb)->dest == adapter->geneve_port) {
            hdr.network = skb_inner_network_header(skb);
        }
    }

    /* Use cached 'tail' */
    if (unlikely(tail < hdr.network + 40)) {
        return;
    }

    switch (hdr.ipv4->version) {  /* Check for valid header access */
    case IPVERSION:
        hlen = (hdr.network[0] & 0x0F) << 2;
        l4_proto = hdr.ipv4->protocol;
        break;
    case 6:
        hlen = hdr.network - skb->data;
        l4_proto = ipv6_find_hdr(skb, &hlen, IPPROTO_TCP, NULL, NULL);
        hlen -= hdr.network - skb->data;
        break;
    default:
        return;
    }

    if (unlikely(l4_proto != IPPROTO_TCP)) {  /* Rare non-TCP in gaming */
        /* Gaming extension: UDP handled at init via FDIR; skip here for perf */
        return;
    }

    /* Use cached 'tail' */
    if (unlikely(tail < hdr.network + hlen + sizeof(struct tcphdr))) {
        return;
    }

    th = (struct tcphdr *)(hdr.network + hlen);

    if (unlikely(th->fin)) {  /* Rare in ongoing gaming flows */
        return;
    }

    if (!th->syn && (ring->atr_count < ring->atr_sample_rate)) {
        return;
    }

    ring->atr_count = 0;

    vlan_id = htons(first->tx_flags >> IXGBE_TX_FLAGS_VLAN_SHIFT);

    input.formatted.vlan_id = vlan_id;

    if (first->tx_flags & (IXGBE_TX_FLAGS_SW_VLAN | IXGBE_TX_FLAGS_HW_VLAN)) {
        common.port.src ^= th->dest ^ htons(ETH_P_8021Q);
    } else {
        common.port.src ^= th->dest ^ first->protocol;
    }
    common.port.dst ^= th->source;

    switch (hdr.ipv4->version) {  /* Check for valid header access */
    case IPVERSION:
        input.formatted.flow_type = IXGBE_ATR_FLOW_TYPE_TCPV4;
        common.ip ^= hdr.ipv4->saddr ^ hdr.ipv4->daddr;
        break;
    case 6:
        input.formatted.flow_type = IXGBE_ATR_FLOW_TYPE_TCPV6;
        common.ip ^= hdr.ipv6->saddr.s6_addr32[0] ^
                     hdr.ipv6->saddr.s6_addr32[1] ^
                     hdr.ipv6->saddr.s6_addr32[2] ^
                     hdr.ipv6->saddr.s6_addr32[3] ^
                     hdr.ipv6->daddr.s6_addr32[0] ^
                     hdr.ipv6->daddr.s6_addr32[1] ^
                     hdr.ipv6->daddr.s6_addr32[2] ^
                     hdr.ipv6->daddr.s6_addr32[3];
        break;
    }

    if (hdr.network != skb_network_header(skb)) {
        input.formatted.flow_type |= IXGBE_ATR_L4TYPE_TUNNEL_MASK;
    }

    ixgbe_fdir_add_signature_filter_82599(&q_vector->adapter->hw,
                                          input, common, ring->queue_index);
}

#ifdef IXGBE_FCOE
static u16 ixgbe_select_queue(struct net_device *dev, struct sk_buff *skb,
			      struct net_device *sb_dev)
{
	struct ixgbe_adapter *adapter;
	struct ixgbe_ring_feature *f;
	int txq;

	if (sb_dev) {
		u8 tc = netdev_get_prio_tc_map(dev, skb->priority);
		struct net_device *vdev = sb_dev;

		txq = vdev->tc_to_txq[tc].offset;
		txq += reciprocal_scale(skb_get_hash(skb),
					vdev->tc_to_txq[tc].count);

		return txq;
	}

	/*
	 * only execute the code below if protocol is FCoE
	 * or FIP and we have FCoE enabled on the adapter
	 */
	switch (vlan_get_protocol(skb)) {
	case htons(ETH_P_FCOE):
	case htons(ETH_P_FIP):
		adapter = netdev_priv(dev);

		if (!sb_dev && (adapter->flags & IXGBE_FLAG_FCOE_ENABLED))
			break;
		fallthrough;
	default:
		return netdev_pick_tx(dev, skb, sb_dev);
	}

	f = &adapter->ring_feature[RING_F_FCOE];

	txq = skb_rx_queue_recorded(skb) ? skb_get_rx_queue(skb) :
					   smp_processor_id();

	while (txq >= f->indices)
		txq -= f->indices;

	return txq + f->offset;
}

#endif
int ixgbe_xmit_xdp_ring(struct ixgbe_ring *ring,
						struct xdp_frame *xdpf)
{
	struct skb_shared_info *sinfo = xdp_get_shared_info_from_frame(xdpf);
	u8 nr_frags = unlikely(xdp_frame_has_frags(xdpf)) ?
	sinfo->nr_frags : 0;
	u16 i = 0, index = ring->next_to_use;
	struct ixgbe_tx_buffer *tx_head = &ring->tx_buffer_info[index];
	struct ixgbe_tx_buffer *tx_buff = tx_head;
	union ixgbe_adv_tx_desc *tx_desc = IXGBE_TX_DESC(ring, index);
	u32 cmd_type, len = xdpf->len;
	void *data = xdpf->data;
	u16 ntu_lookahead;

	/* Opt #3: Look-ahead prefetch for XDP Tx */
	ntu_lookahead = (index + IXGBE_TX_PREFETCH_OFFSET) & (ring->count - 1);
	prefetchw(&ring->tx_buffer_info[ntu_lookahead]);
	prefetch(IXGBE_TX_DESC(ring, ntu_lookahead));

	if (unlikely(ixgbe_desc_unused(ring) < 1 + nr_frags))
		return IXGBE_XDP_CONSUMED;

	tx_head->bytecount = xdp_get_frame_len(xdpf);
	tx_head->gso_segs  = 1;
	tx_head->xdpf      = xdpf;

	tx_desc->read.olinfo_status =
	cpu_to_le32(tx_head->bytecount << IXGBE_ADVTXD_PAYLEN_SHIFT);

	for (;;) {
		dma_addr_t dma = dma_map_single(ring->dev, data, len,
										DMA_TO_DEVICE);
		if (dma_mapping_error(ring->dev, dma))
			goto unmap;

		dma_unmap_len_set(tx_buff, len,  len);
		dma_unmap_addr_set(tx_buff, dma, dma);

		cmd_type = IXGBE_ADVTXD_DTYP_DATA | IXGBE_ADVTXD_DCMD_DEXT |
		IXGBE_ADVTXD_DCMD_IFCS | len;
		tx_desc->read.cmd_type_len = cpu_to_le32(cmd_type);
		tx_desc->read.buffer_addr  = cpu_to_le64(dma);
		tx_buff->protocol = 0;

		if (++index == ring->count)
			index = 0;

		if (i == nr_frags)
			break;

		tx_buff = &ring->tx_buffer_info[index];
		tx_desc = IXGBE_TX_DESC(ring, index);
		tx_desc->read.olinfo_status = 0;

		data = skb_frag_address(&sinfo->frags[i]);
		len  = skb_frag_size(&sinfo->frags[i]);
		i++;
	}

	tx_desc->read.cmd_type_len |= cpu_to_le32(IXGBE_TXD_CMD);

	smp_wmb();

	tx_head->next_to_watch = tx_desc;
	ring->next_to_use      = index;

	return IXGBE_XDP_TX;

	unmap:
	for (;;) {
		tx_buff = &ring->tx_buffer_info[index];
		if (dma_unmap_len(tx_buff, len))
			dma_unmap_page(ring->dev,
						   dma_unmap_addr(tx_buff, dma),
						   dma_unmap_len(tx_buff, len),
						   DMA_TO_DEVICE);
			dma_unmap_len_set(tx_buff, len, 0);
		if (tx_buff == tx_head)
			break;

		if (!index)
			index += ring->count;
		index--;
	}

	return IXGBE_XDP_CONSUMED;
}

netdev_tx_t ixgbe_xmit_frame_ring(struct sk_buff *skb,
			  struct ixgbe_adapter *adapter,
			  struct ixgbe_ring *tx_ring)
{
	struct ixgbe_tx_buffer *first;
	int tso;
	u32 tx_flags = 0;
	unsigned short f;
	u16 count = TXD_USE_COUNT(skb_headlen(skb));
	struct ixgbe_ipsec_tx_data ipsec_tx = { 0 };
	__be16 protocol = skb->protocol;
	u8 hdr_len = 0;

	/*
	 * need: 1 descriptor per page * PAGE_SIZE/IXGBE_MAX_DATA_PER_TXD,
	 *       + 1 desc for skb_headlen/IXGBE_MAX_DATA_PER_TXD,
	 *       + 2 desc gap to keep tail from touching head,
	 *       + 1 desc for context descriptor,
	 * otherwise try next time
	 */
	for (f = 0; f < skb_shinfo(skb)->nr_frags; f++)
		count += TXD_USE_COUNT(skb_frag_size(
						&skb_shinfo(skb)->frags[f]));

	if (ixgbe_maybe_stop_tx(tx_ring, count + 3)) {
		tx_ring->tx_stats.tx_busy++;
		return NETDEV_TX_BUSY;
	}

	/* record the location of the first descriptor for this packet */
	first = &tx_ring->tx_buffer_info[tx_ring->next_to_use];
	first->skb = skb;
	first->bytecount = skb->len;
	first->gso_segs = 1;

	/* if we have a HW VLAN tag being added default to the HW one */
	if (skb_vlan_tag_present(skb)) {
		tx_flags |= skb_vlan_tag_get(skb) << IXGBE_TX_FLAGS_VLAN_SHIFT;
		tx_flags |= IXGBE_TX_FLAGS_HW_VLAN;
	/* else if it is a SW VLAN check the next protocol and store the tag */
	} else if (protocol == htons(ETH_P_8021Q)) {
		struct vlan_hdr *vhdr, _vhdr;
		vhdr = skb_header_pointer(skb, ETH_HLEN, sizeof(_vhdr), &_vhdr);
		if (!vhdr)
			goto out_drop;

		tx_flags |= ntohs(vhdr->h_vlan_TCI) <<
				  IXGBE_TX_FLAGS_VLAN_SHIFT;
		tx_flags |= IXGBE_TX_FLAGS_SW_VLAN;
	}
	protocol = vlan_get_protocol(skb);

	if (unlikely(skb_shinfo(skb)->tx_flags & SKBTX_HW_TSTAMP) &&
	    adapter->ptp_clock) {
		if (adapter->tstamp_config.tx_type == HWTSTAMP_TX_ON &&
		    !test_and_set_bit_lock(__IXGBE_PTP_TX_IN_PROGRESS,
					   &adapter->state)) {
			skb_shinfo(skb)->tx_flags |= SKBTX_IN_PROGRESS;
			tx_flags |= IXGBE_TX_FLAGS_TSTAMP;

			/* schedule check for Tx timestamp */
			adapter->ptp_tx_skb = skb_get(skb);
			adapter->ptp_tx_start = jiffies;
			schedule_work(&adapter->ptp_tx_work);
		} else {
			adapter->tx_hwtstamp_skipped++;
		}
	}

#ifdef CONFIG_PCI_IOV
	/*
	 * Use the l2switch_enable flag - would be false if the DMA
	 * Tx switch had been disabled.
	 */
	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)
		tx_flags |= IXGBE_TX_FLAGS_CC;

#endif
	/* DCB maps skb priorities 0-7 onto 3 bit PCP of VLAN tag. */
	if ((adapter->flags & IXGBE_FLAG_DCB_ENABLED) &&
	    ((tx_flags & (IXGBE_TX_FLAGS_HW_VLAN | IXGBE_TX_FLAGS_SW_VLAN)) ||
	     (skb->priority != TC_PRIO_CONTROL))) {
		tx_flags &= ~IXGBE_TX_FLAGS_VLAN_PRIO_MASK;
		tx_flags |= (skb->priority & 0x7) <<
					IXGBE_TX_FLAGS_VLAN_PRIO_SHIFT;
		if (tx_flags & IXGBE_TX_FLAGS_SW_VLAN) {
			struct vlan_ethhdr *vhdr;

			if (skb_cow_head(skb, 0))
				goto out_drop;
			vhdr = skb_vlan_eth_hdr(skb);
			vhdr->h_vlan_TCI = htons(tx_flags >>
						 IXGBE_TX_FLAGS_VLAN_SHIFT);
		} else {
			tx_flags |= IXGBE_TX_FLAGS_HW_VLAN;
		}
	}

	/* record initial flags and protocol */
	first->tx_flags = tx_flags;
	first->protocol = protocol;

#ifdef IXGBE_FCOE
	/* setup tx offload for FCoE */
	if ((protocol == htons(ETH_P_FCOE)) &&
	    (tx_ring->netdev->features & (NETIF_F_FSO | NETIF_F_FCOE_CRC))) {
		tso = ixgbe_fso(tx_ring, first, &hdr_len);
		if (tso < 0)
			goto out_drop;

		goto xmit_fcoe;
	}

#endif /* IXGBE_FCOE */

#ifdef CONFIG_IXGBE_IPSEC
	if (xfrm_offload(skb) &&
	    !ixgbe_ipsec_tx(tx_ring, first, &ipsec_tx))
		goto out_drop;
#endif
	tso = ixgbe_tso(tx_ring, first, &hdr_len, &ipsec_tx);
	if (tso < 0)
		goto out_drop;
	else if (!tso)
		ixgbe_tx_csum(tx_ring, first, &ipsec_tx);

	/* add the ATR filter if ATR is on */
	if (test_bit(__IXGBE_TX_FDIR_INIT_DONE, &tx_ring->state))
		ixgbe_atr(tx_ring, first);

#ifdef IXGBE_FCOE
xmit_fcoe:
#endif /* IXGBE_FCOE */
	if (ixgbe_tx_map(tx_ring, first, hdr_len))
		goto cleanup_tx_timestamp;

	return NETDEV_TX_OK;

out_drop:
	dev_kfree_skb_any(first->skb);
	first->skb = NULL;
cleanup_tx_timestamp:
	if (unlikely(tx_flags & IXGBE_TX_FLAGS_TSTAMP)) {
		dev_kfree_skb_any(adapter->ptp_tx_skb);
		adapter->ptp_tx_skb = NULL;
		cancel_work_sync(&adapter->ptp_tx_work);
		clear_bit_unlock(__IXGBE_PTP_TX_IN_PROGRESS, &adapter->state);
	}

	return NETDEV_TX_OK;
}

static netdev_tx_t __ixgbe_xmit_frame(struct sk_buff *skb,
				      struct net_device *netdev,
				      struct ixgbe_ring *ring)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_ring *tx_ring;

	/*
	 * The minimum packet size for olinfo paylen is 17 so pad the skb
	 * in order to meet this minimum size requirement.
	 */
	if (skb_put_padto(skb, 17))
		return NETDEV_TX_OK;

	tx_ring = ring ? ring : adapter->tx_ring[skb_get_queue_mapping(skb)];
	if (unlikely(test_bit(__IXGBE_TX_DISABLED, &tx_ring->state)))
		return NETDEV_TX_BUSY;

	return ixgbe_xmit_frame_ring(skb, adapter, tx_ring);
}

static netdev_tx_t ixgbe_xmit_frame(struct sk_buff *skb,
				    struct net_device *netdev)
{
	return __ixgbe_xmit_frame(skb, netdev, NULL);
}

/**
 * ixgbe_set_mac - Change the Ethernet Address of the NIC
 * @netdev: network interface device structure
 * @p: pointer to an address structure
 *
 * Returns 0 on success, negative on failure
 **/
static int ixgbe_set_mac(struct net_device *netdev, void *p)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;
	struct sockaddr *addr = p;

	if (!is_valid_ether_addr(addr->sa_data))
		return -EADDRNOTAVAIL;

	eth_hw_addr_set(netdev, addr->sa_data);
	memcpy(hw->mac.addr, addr->sa_data, netdev->addr_len);

	ixgbe_mac_set_default_filter(adapter);

	return 0;
}

static int
ixgbe_mdio_read(struct net_device *netdev, int prtad, int devad, u16 addr)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;
	u16 value;
	int rc;

	if (adapter->mii_bus) {
		int regnum = addr;

		if (devad != MDIO_DEVAD_NONE)
			return mdiobus_c45_read(adapter->mii_bus, prtad,
						devad, regnum);

		return mdiobus_read(adapter->mii_bus, prtad, regnum);
	}

	if (prtad != hw->phy.mdio.prtad)
		return -EINVAL;
	rc = hw->phy.ops.read_reg(hw, addr, devad, &value);
	if (!rc)
		rc = value;
	return rc;
}

static int ixgbe_mdio_write(struct net_device *netdev, int prtad, int devad,
			    u16 addr, u16 value)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	struct ixgbe_hw *hw = &adapter->hw;

	if (adapter->mii_bus) {
		int regnum = addr;

		if (devad != MDIO_DEVAD_NONE)
			return mdiobus_c45_write(adapter->mii_bus, prtad, devad,
						 regnum, value);

		return mdiobus_write(adapter->mii_bus, prtad, regnum, value);
	}

	if (prtad != hw->phy.mdio.prtad)
		return -EINVAL;
	return hw->phy.ops.write_reg(hw, addr, devad, value);
}

static int ixgbe_ioctl(struct net_device *netdev, struct ifreq *req, int cmd)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	switch (cmd) {
	case SIOCSHWTSTAMP:
		return ixgbe_ptp_set_ts_config(adapter, req);
	case SIOCGHWTSTAMP:
		return ixgbe_ptp_get_ts_config(adapter, req);
	case SIOCGMIIPHY:
		if (!adapter->hw.phy.ops.read_reg)
			return -EOPNOTSUPP;
		fallthrough;
	default:
		return mdio_mii_ioctl(&adapter->hw.phy.mdio, if_mii(req), cmd);
	}
}

/**
 * ixgbe_add_sanmac_netdev - Add the SAN MAC address to the corresponding
 * netdev->dev_addrs
 * @dev: network interface device structure
 *
 * Returns non-zero on failure
 **/
static int ixgbe_add_sanmac_netdev(struct net_device *dev)
{
	int err = 0;
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct ixgbe_hw *hw = &adapter->hw;

	if (is_valid_ether_addr(hw->mac.san_addr)) {
		rtnl_lock();
		err = dev_addr_add(dev, hw->mac.san_addr, NETDEV_HW_ADDR_T_SAN);
		rtnl_unlock();

		/* update SAN MAC vmdq pool selection */
		hw->mac.ops.set_vmdq_san_mac(hw, VMDQ_P(0));
	}
	return err;
}

/**
 * ixgbe_del_sanmac_netdev - Removes the SAN MAC address to the corresponding
 * netdev->dev_addrs
 * @dev: network interface device structure
 *
 * Returns non-zero on failure
 **/
static int ixgbe_del_sanmac_netdev(struct net_device *dev)
{
	int err = 0;
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct ixgbe_mac_info *mac = &adapter->hw.mac;

	if (is_valid_ether_addr(mac->san_addr)) {
		rtnl_lock();
		err = dev_addr_del(dev, mac->san_addr, NETDEV_HW_ADDR_T_SAN);
		rtnl_unlock();
	}
	return err;
}

static void ixgbe_get_ring_stats64(struct rtnl_link_stats64 *stats,
				   struct ixgbe_ring *ring)
{
	u64 bytes, packets;
	unsigned int start;

	if (ring) {
		do {
			start = u64_stats_fetch_begin(&ring->syncp);
			packets = ring->stats.packets;
			bytes   = ring->stats.bytes;
		} while (u64_stats_fetch_retry(&ring->syncp, start));
		stats->tx_packets += packets;
		stats->tx_bytes   += bytes;
	}
}

static void ixgbe_get_stats64(struct net_device *netdev,
			      struct rtnl_link_stats64 *stats)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	int i;

	rcu_read_lock();
	for (i = 0; i < adapter->num_rx_queues; i++) {
		struct ixgbe_ring *ring = READ_ONCE(adapter->rx_ring[i]);
		u64 bytes, packets;
		unsigned int start;

		if (ring) {
			do {
				start = u64_stats_fetch_begin(&ring->syncp);
				packets = ring->stats.packets;
				bytes   = ring->stats.bytes;
			} while (u64_stats_fetch_retry(&ring->syncp, start));
			stats->rx_packets += packets;
			stats->rx_bytes   += bytes;
		}
	}

	for (i = 0; i < adapter->num_tx_queues; i++) {
		struct ixgbe_ring *ring = READ_ONCE(adapter->tx_ring[i]);

		ixgbe_get_ring_stats64(stats, ring);
	}
	for (i = 0; i < adapter->num_xdp_queues; i++) {
		struct ixgbe_ring *ring = READ_ONCE(adapter->xdp_ring[i]);

		ixgbe_get_ring_stats64(stats, ring);
	}
	rcu_read_unlock();

	/* following stats updated by ixgbe_watchdog_task() */
	stats->multicast	= netdev->stats.multicast;
	stats->rx_errors	= netdev->stats.rx_errors;
	stats->rx_length_errors	= netdev->stats.rx_length_errors;
	stats->rx_crc_errors	= netdev->stats.rx_crc_errors;
	stats->rx_missed_errors	= netdev->stats.rx_missed_errors;
}

static int ixgbe_ndo_get_vf_stats(struct net_device *netdev, int vf,
				  struct ifla_vf_stats *vf_stats)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	if (vf < 0 || vf >= adapter->num_vfs)
		return -EINVAL;

	vf_stats->rx_packets = adapter->vfinfo[vf].vfstats.gprc;
	vf_stats->rx_bytes   = adapter->vfinfo[vf].vfstats.gorc;
	vf_stats->tx_packets = adapter->vfinfo[vf].vfstats.gptc;
	vf_stats->tx_bytes   = adapter->vfinfo[vf].vfstats.gotc;
	vf_stats->multicast  = adapter->vfinfo[vf].vfstats.mprc;

	return 0;
}

#ifdef CONFIG_IXGBE_DCB
/**
 * ixgbe_validate_rtr - verify 802.1Qp to Rx packet buffer mapping is valid.
 * @adapter: pointer to ixgbe_adapter
 * @tc: number of traffic classes currently enabled
 *
 * Configure a valid 802.1Qp to Rx packet buffer mapping ie confirm
 * 802.1Q priority maps to a packet buffer that exists.
 */
static void ixgbe_validate_rtr(struct ixgbe_adapter *adapter, u8 tc)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u32 reg, rsave;
	int i;

	/* 82598 have a static priority to TC mapping that can not
	 * be changed so no validation is needed.
	 */
	if (hw->mac.type == ixgbe_mac_82598EB)
		return;

	reg = IXGBE_READ_REG(hw, IXGBE_RTRUP2TC);
	rsave = reg;

	for (i = 0; i < MAX_TRAFFIC_CLASS; i++) {
		u8 up2tc = reg >> (i * IXGBE_RTRUP2TC_UP_SHIFT);

		/* If up2tc is out of bounds default to zero */
		if (up2tc > tc)
			reg &= ~(0x7 << IXGBE_RTRUP2TC_UP_SHIFT);
	}

	if (reg != rsave)
		IXGBE_WRITE_REG(hw, IXGBE_RTRUP2TC, reg);

	return;
}

/**
 * ixgbe_set_prio_tc_map - Configure netdev prio tc map
 * @adapter: Pointer to adapter struct
 *
 * Populate the netdev user priority to tc map
 */
static void ixgbe_set_prio_tc_map(struct ixgbe_adapter *adapter)
{
	struct net_device *dev = adapter->netdev;
	struct ixgbe_dcb_config *dcb_cfg = &adapter->dcb_cfg;
	struct ieee_ets *ets = adapter->ixgbe_ieee_ets;
	u8 prio;

	for (prio = 0; prio < MAX_USER_PRIORITY; prio++) {
		u8 tc = 0;

		if (adapter->dcbx_cap & DCB_CAP_DCBX_VER_CEE)
			tc = ixgbe_dcb_get_tc_from_up(dcb_cfg, 0, prio);
		else if (ets)
			tc = ets->prio_tc[prio];

		netdev_set_prio_tc_map(dev, prio, tc);
	}
}

#endif /* CONFIG_IXGBE_DCB */
static int ixgbe_reassign_macvlan_pool(struct net_device *vdev,
				       struct netdev_nested_priv *priv)
{
	struct ixgbe_adapter *adapter = (struct ixgbe_adapter *)priv->data;
	struct ixgbe_fwd_adapter *accel;
	int pool;

	/* we only care about macvlans... */
	if (!netif_is_macvlan(vdev))
		return 0;

	/* that have hardware offload enabled... */
	accel = macvlan_accel_priv(vdev);
	if (!accel)
		return 0;

	/* If we can relocate to a different bit do so */
	pool = find_first_zero_bit(adapter->fwd_bitmask, adapter->num_rx_pools);
	if (pool < adapter->num_rx_pools) {
		set_bit(pool, adapter->fwd_bitmask);
		accel->pool = pool;
		return 0;
	}

	/* if we cannot find a free pool then disable the offload */
	netdev_err(vdev, "L2FW offload disabled due to lack of queue resources\n");
	macvlan_release_l2fw_offload(vdev);

	/* unbind the queues and drop the subordinate channel config */
	netdev_unbind_sb_channel(adapter->netdev, vdev);
	netdev_set_sb_channel(vdev, 0);

	kfree(accel);

	return 0;
}

static void ixgbe_defrag_macvlan_pools(struct net_device *dev)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct netdev_nested_priv priv = {
		.data = (void *)adapter,
	};

	/* flush any stale bits out of the fwd bitmask */
	bitmap_clear(adapter->fwd_bitmask, 1, 63);

	/* walk through upper devices reassigning pools */
	netdev_walk_all_upper_dev_rcu(dev, ixgbe_reassign_macvlan_pool,
				      &priv);
}

/**
 * ixgbe_setup_tc - configure net_device for multiple traffic classes
 *
 * @dev: net device to configure
 * @tc: number of traffic classes to enable
 */
int ixgbe_setup_tc(struct net_device *dev, u8 tc)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct ixgbe_hw *hw = &adapter->hw;

	/* Hardware supports up to 8 traffic classes */
	if (tc > adapter->dcb_cfg.num_tcs.pg_tcs)
		return -EINVAL;

	if (hw->mac.type == ixgbe_mac_82598EB && tc && tc < MAX_TRAFFIC_CLASS)
		return -EINVAL;

	/* Hardware has to reinitialize queues and interrupts to
	 * match packet buffer alignment. Unfortunately, the
	 * hardware is not flexible enough to do this dynamically.
	 */
	if (netif_running(dev))
		ixgbe_close(dev);
	else
		ixgbe_reset(adapter);

	ixgbe_clear_interrupt_scheme(adapter);

#ifdef CONFIG_IXGBE_DCB
	if (tc) {
		if (adapter->xdp_prog) {
			e_warn(probe, "DCB is not supported with XDP\n");

			ixgbe_init_interrupt_scheme(adapter);
			if (netif_running(dev))
				ixgbe_open(dev);
			return -EINVAL;
		}

		netdev_set_num_tc(dev, tc);
		ixgbe_set_prio_tc_map(adapter);

		adapter->hw_tcs = tc;
		adapter->flags |= IXGBE_FLAG_DCB_ENABLED;

		if (adapter->hw.mac.type == ixgbe_mac_82598EB) {
			adapter->last_lfc_mode = adapter->hw.fc.requested_mode;
			adapter->hw.fc.requested_mode = ixgbe_fc_none;
		}
	} else {
		netdev_reset_tc(dev);

		if (adapter->hw.mac.type == ixgbe_mac_82598EB)
			adapter->hw.fc.requested_mode = adapter->last_lfc_mode;

		adapter->flags &= ~IXGBE_FLAG_DCB_ENABLED;
		adapter->hw_tcs = tc;

		adapter->temp_dcb_cfg.pfc_mode_enable = false;
		adapter->dcb_cfg.pfc_mode_enable = false;
	}

	ixgbe_validate_rtr(adapter, tc);

#endif /* CONFIG_IXGBE_DCB */
	ixgbe_init_interrupt_scheme(adapter);

	ixgbe_defrag_macvlan_pools(dev);

	if (netif_running(dev))
		return ixgbe_open(dev);

	return 0;
}

static int ixgbe_delete_clsu32(struct ixgbe_adapter *adapter,
			       struct tc_cls_u32_offload *cls)
{
	u32 hdl = cls->knode.handle;
	u32 uhtid = TC_U32_USERHTID(cls->knode.handle);
	u32 loc = cls->knode.handle & 0xfffff;
	int err = 0, i, j;
	struct ixgbe_jump_table *jump = NULL;

	if (loc > IXGBE_MAX_HW_ENTRIES)
		return -EINVAL;

	if ((uhtid != 0x800) && (uhtid >= IXGBE_MAX_LINK_HANDLE))
		return -EINVAL;

	/* Clear this filter in the link data it is associated with */
	if (uhtid != 0x800) {
		jump = adapter->jump_tables[uhtid];
		if (!jump)
			return -EINVAL;
		if (!test_bit(loc - 1, jump->child_loc_map))
			return -EINVAL;
		clear_bit(loc - 1, jump->child_loc_map);
	}

	/* Check if the filter being deleted is a link */
	for (i = 1; i < IXGBE_MAX_LINK_HANDLE; i++) {
		jump = adapter->jump_tables[i];
		if (jump && jump->link_hdl == hdl) {
			/* Delete filters in the hardware in the child hash
			 * table associated with this link
			 */
			for (j = 0; j < IXGBE_MAX_HW_ENTRIES; j++) {
				if (!test_bit(j, jump->child_loc_map))
					continue;
				spin_lock(&adapter->fdir_perfect_lock);
				err = ixgbe_update_ethtool_fdir_entry(adapter,
								      NULL,
								      j + 1);
				spin_unlock(&adapter->fdir_perfect_lock);
				clear_bit(j, jump->child_loc_map);
			}
			/* Remove resources for this link */
			kfree(jump->input);
			kfree(jump->mask);
			kfree(jump);
			adapter->jump_tables[i] = NULL;
			return err;
		}
	}

	spin_lock(&adapter->fdir_perfect_lock);
	err = ixgbe_update_ethtool_fdir_entry(adapter, NULL, loc);
	spin_unlock(&adapter->fdir_perfect_lock);
	return err;
}

static int ixgbe_configure_clsu32_add_hnode(struct ixgbe_adapter *adapter,
					    struct tc_cls_u32_offload *cls)
{
	u32 uhtid = TC_U32_USERHTID(cls->hnode.handle);

	if (uhtid >= IXGBE_MAX_LINK_HANDLE)
		return -EINVAL;

	/* This ixgbe devices do not support hash tables at the moment
	 * so abort when given hash tables.
	 */
	if (cls->hnode.divisor > 0)
		return -EINVAL;

	set_bit(uhtid - 1, &adapter->tables);
	return 0;
}

static int ixgbe_configure_clsu32_del_hnode(struct ixgbe_adapter *adapter,
					    struct tc_cls_u32_offload *cls)
{
	u32 uhtid = TC_U32_USERHTID(cls->hnode.handle);

	if (uhtid >= IXGBE_MAX_LINK_HANDLE)
		return -EINVAL;

	clear_bit(uhtid - 1, &adapter->tables);
	return 0;
}

#ifdef CONFIG_NET_CLS_ACT
struct upper_walk_data {
	struct ixgbe_adapter *adapter;
	u64 action;
	int ifindex;
	u8 queue;
};

static int get_macvlan_queue(struct net_device *upper,
			     struct netdev_nested_priv *priv)
{
	if (netif_is_macvlan(upper)) {
		struct ixgbe_fwd_adapter *vadapter = macvlan_accel_priv(upper);
		struct ixgbe_adapter *adapter;
		struct upper_walk_data *data;
		int ifindex;

		data = (struct upper_walk_data *)priv->data;
		ifindex = data->ifindex;
		adapter = data->adapter;
		if (vadapter && upper->ifindex == ifindex) {
			data->queue = adapter->rx_ring[vadapter->rx_base_queue]->reg_idx;
			data->action = data->queue;
			return 1;
		}
	}

	return 0;
}

static int handle_redirect_action(struct ixgbe_adapter *adapter, int ifindex,
				  u8 *queue, u64 *action)
{
	struct ixgbe_ring_feature *vmdq = &adapter->ring_feature[RING_F_VMDQ];
	unsigned int num_vfs = adapter->num_vfs, vf;
	struct netdev_nested_priv priv;
	struct upper_walk_data data;
	struct net_device *upper;

	/* redirect to a SRIOV VF */
	for (vf = 0; vf < num_vfs; ++vf) {
		upper = pci_get_drvdata(adapter->vfinfo[vf].vfdev);
		if (upper->ifindex == ifindex) {
			*queue = vf * __ALIGN_MASK(1, ~vmdq->mask);
			*action = vf + 1;
			*action <<= ETHTOOL_RX_FLOW_SPEC_RING_VF_OFF;
			return 0;
		}
	}

	/* redirect to a offloaded macvlan netdev */
	data.adapter = adapter;
	data.ifindex = ifindex;
	data.action = 0;
	data.queue = 0;
	priv.data = (void *)&data;
	if (netdev_walk_all_upper_dev_rcu(adapter->netdev,
					  get_macvlan_queue, &priv)) {
		*action = data.action;
		*queue = data.queue;

		return 0;
	}

	return -EINVAL;
}

static int parse_tc_actions(struct ixgbe_adapter *adapter,
			    struct tcf_exts *exts, u64 *action, u8 *queue)
{
	const struct tc_action *a;
	int i;

	if (!tcf_exts_has_actions(exts))
		return -EINVAL;

	tcf_exts_for_each_action(i, a, exts) {
		/* Drop action */
		if (is_tcf_gact_shot(a)) {
			*action = IXGBE_FDIR_DROP_QUEUE;
			*queue = IXGBE_FDIR_DROP_QUEUE;
			return 0;
		}

		/* Redirect to a VF or a offloaded macvlan */
		if (is_tcf_mirred_egress_redirect(a)) {
			struct net_device *dev = tcf_mirred_dev(a);

			if (!dev)
				return -EINVAL;
			return handle_redirect_action(adapter, dev->ifindex,
						      queue, action);
		}

		return -EINVAL;
	}

	return -EINVAL;
}
#else
static int parse_tc_actions(struct ixgbe_adapter *adapter,
			    struct tcf_exts *exts, u64 *action, u8 *queue)
{
	return -EINVAL;
}
#endif /* CONFIG_NET_CLS_ACT */

static int ixgbe_clsu32_build_input(struct ixgbe_fdir_filter *input,
				    union ixgbe_atr_input *mask,
				    struct tc_cls_u32_offload *cls,
				    struct ixgbe_mat_field *field_ptr,
				    struct ixgbe_nexthdr *nexthdr)
{
	int i, j, off;
	__be32 val, m;
	bool found_entry = false, found_jump_field = false;

	for (i = 0; i < cls->knode.sel->nkeys; i++) {
		off = cls->knode.sel->keys[i].off;
		val = cls->knode.sel->keys[i].val;
		m = cls->knode.sel->keys[i].mask;

		for (j = 0; field_ptr[j].val; j++) {
			if (field_ptr[j].off == off) {
				field_ptr[j].val(input, mask, (__force u32)val,
						 (__force u32)m);
				input->filter.formatted.flow_type |=
					field_ptr[j].type;
				found_entry = true;
				break;
			}
		}
		if (nexthdr) {
			if (nexthdr->off == cls->knode.sel->keys[i].off &&
			    nexthdr->val ==
			    (__force u32)cls->knode.sel->keys[i].val &&
			    nexthdr->mask ==
			    (__force u32)cls->knode.sel->keys[i].mask)
				found_jump_field = true;
			else
				continue;
		}
	}

	if (nexthdr && !found_jump_field)
		return -EINVAL;

	if (!found_entry)
		return 0;

	mask->formatted.flow_type = IXGBE_ATR_L4TYPE_IPV6_MASK |
				    IXGBE_ATR_L4TYPE_MASK;

	if (input->filter.formatted.flow_type == IXGBE_ATR_FLOW_TYPE_IPV4)
		mask->formatted.flow_type &= IXGBE_ATR_L4TYPE_IPV6_MASK;

	return 0;
}

static int ixgbe_configure_clsu32(struct ixgbe_adapter *adapter,
				  struct tc_cls_u32_offload *cls)
{
	__be16 protocol = cls->common.protocol;
	u32 loc = cls->knode.handle & 0xfffff;
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_mat_field *field_ptr;
	struct ixgbe_fdir_filter *input = NULL;
	union ixgbe_atr_input *mask = NULL;
	struct ixgbe_jump_table *jump = NULL;
	int i, err = -EINVAL;
	u8 queue;
	u32 uhtid, link_uhtid;

	uhtid = TC_U32_USERHTID(cls->knode.handle);
	link_uhtid = TC_U32_USERHTID(cls->knode.link_handle);

	/* At the moment cls_u32 jumps to network layer and skips past
	 * L2 headers. The canonical method to match L2 frames is to use
	 * negative values. However this is error prone at best but really
	 * just broken because there is no way to "know" what sort of hdr
	 * is in front of the network layer. Fix cls_u32 to support L2
	 * headers when needed.
	 */
	if (protocol != htons(ETH_P_IP))
		return err;

	if (loc >= ((1024 << adapter->fdir_pballoc) - 2)) {
		e_err(drv, "Location out of range\n");
		return err;
	}

	/* cls u32 is a graph starting at root node 0x800. The driver tracks
	 * links and also the fields used to advance the parser across each
	 * link (e.g. nexthdr/eat parameters from 'tc'). This way we can map
	 * the u32 graph onto the hardware parse graph denoted in ixgbe_model.h
	 * To add support for new nodes update ixgbe_model.h parse structures
	 * this function _should_ be generic try not to hardcode values here.
	 */
	if (uhtid == 0x800) {
		field_ptr = (adapter->jump_tables[0])->mat;
	} else {
		if (uhtid >= IXGBE_MAX_LINK_HANDLE)
			return err;
		if (!adapter->jump_tables[uhtid])
			return err;
		field_ptr = (adapter->jump_tables[uhtid])->mat;
	}

	if (!field_ptr)
		return err;

	/* At this point we know the field_ptr is valid and need to either
	 * build cls_u32 link or attach filter. Because adding a link to
	 * a handle that does not exist is invalid and the same for adding
	 * rules to handles that don't exist.
	 */

	if (link_uhtid) {
		struct ixgbe_nexthdr *nexthdr = ixgbe_ipv4_jumps;

		if (link_uhtid >= IXGBE_MAX_LINK_HANDLE)
			return err;

		if (!test_bit(link_uhtid - 1, &adapter->tables))
			return err;

		/* Multiple filters as links to the same hash table are not
		 * supported. To add a new filter with the same next header
		 * but different match/jump conditions, create a new hash table
		 * and link to it.
		 */
		if (adapter->jump_tables[link_uhtid] &&
		    (adapter->jump_tables[link_uhtid])->link_hdl) {
			e_err(drv, "Link filter exists for link: %x\n",
			      link_uhtid);
			return err;
		}

		for (i = 0; nexthdr[i].jump; i++) {
			if (nexthdr[i].o != cls->knode.sel->offoff ||
			    nexthdr[i].s != cls->knode.sel->offshift ||
			    nexthdr[i].m !=
			    (__force u32)cls->knode.sel->offmask)
				return err;

			jump = kzalloc(sizeof(*jump), GFP_KERNEL);
			if (!jump)
				return -ENOMEM;
			input = kzalloc(sizeof(*input), GFP_KERNEL);
			if (!input) {
				err = -ENOMEM;
				goto free_jump;
			}
			mask = kzalloc(sizeof(*mask), GFP_KERNEL);
			if (!mask) {
				err = -ENOMEM;
				goto free_input;
			}
			jump->input = input;
			jump->mask = mask;
			jump->link_hdl = cls->knode.handle;

			err = ixgbe_clsu32_build_input(input, mask, cls,
						       field_ptr, &nexthdr[i]);
			if (!err) {
				jump->mat = nexthdr[i].jump;
				adapter->jump_tables[link_uhtid] = jump;
				break;
			} else {
				kfree(mask);
				kfree(input);
				kfree(jump);
			}
		}
		return 0;
	}

	input = kzalloc(sizeof(*input), GFP_KERNEL);
	if (!input)
		return -ENOMEM;
	mask = kzalloc(sizeof(*mask), GFP_KERNEL);
	if (!mask) {
		err = -ENOMEM;
		goto free_input;
	}

	if ((uhtid != 0x800) && (adapter->jump_tables[uhtid])) {
		if ((adapter->jump_tables[uhtid])->input)
			memcpy(input, (adapter->jump_tables[uhtid])->input,
			       sizeof(*input));
		if ((adapter->jump_tables[uhtid])->mask)
			memcpy(mask, (adapter->jump_tables[uhtid])->mask,
			       sizeof(*mask));

		/* Lookup in all child hash tables if this location is already
		 * filled with a filter
		 */
		for (i = 1; i < IXGBE_MAX_LINK_HANDLE; i++) {
			struct ixgbe_jump_table *link = adapter->jump_tables[i];

			if (link && (test_bit(loc - 1, link->child_loc_map))) {
				e_err(drv, "Filter exists in location: %x\n",
				      loc);
				err = -EINVAL;
				goto err_out;
			}
		}
	}
	err = ixgbe_clsu32_build_input(input, mask, cls, field_ptr, NULL);
	if (err)
		goto err_out;

	err = parse_tc_actions(adapter, cls->knode.exts, &input->action,
			       &queue);
	if (err < 0)
		goto err_out;

	input->sw_idx = loc;

	spin_lock(&adapter->fdir_perfect_lock);

	if (hlist_empty(&adapter->fdir_filter_list)) {
		memcpy(&adapter->fdir_mask, mask, sizeof(*mask));
		err = ixgbe_fdir_set_input_mask_82599(hw, mask);
		if (err)
			goto err_out_w_lock;
	} else if (memcmp(&adapter->fdir_mask, mask, sizeof(*mask))) {
		err = -EINVAL;
		goto err_out_w_lock;
	}

	ixgbe_atr_compute_perfect_hash_82599(&input->filter, mask);
	err = ixgbe_fdir_write_perfect_filter_82599(hw, &input->filter,
						    input->sw_idx, queue);
	if (err)
		goto err_out_w_lock;

	ixgbe_update_ethtool_fdir_entry(adapter, input, input->sw_idx);
	spin_unlock(&adapter->fdir_perfect_lock);

	if ((uhtid != 0x800) && (adapter->jump_tables[uhtid]))
		set_bit(loc - 1, (adapter->jump_tables[uhtid])->child_loc_map);

	kfree(mask);
	return err;
err_out_w_lock:
	spin_unlock(&adapter->fdir_perfect_lock);
err_out:
	kfree(mask);
free_input:
	kfree(input);
free_jump:
	kfree(jump);
	return err;
}

static int ixgbe_setup_tc_cls_u32(struct ixgbe_adapter *adapter,
				  struct tc_cls_u32_offload *cls_u32)
{
	switch (cls_u32->command) {
	case TC_CLSU32_NEW_KNODE:
	case TC_CLSU32_REPLACE_KNODE:
		return ixgbe_configure_clsu32(adapter, cls_u32);
	case TC_CLSU32_DELETE_KNODE:
		return ixgbe_delete_clsu32(adapter, cls_u32);
	case TC_CLSU32_NEW_HNODE:
	case TC_CLSU32_REPLACE_HNODE:
		return ixgbe_configure_clsu32_add_hnode(adapter, cls_u32);
	case TC_CLSU32_DELETE_HNODE:
		return ixgbe_configure_clsu32_del_hnode(adapter, cls_u32);
	default:
		return -EOPNOTSUPP;
	}
}

static int ixgbe_setup_tc_block_cb(enum tc_setup_type type, void *type_data,
				   void *cb_priv)
{
	struct ixgbe_adapter *adapter = cb_priv;

	if (!tc_cls_can_offload_and_chain0(adapter->netdev, type_data))
		return -EOPNOTSUPP;

	switch (type) {
	case TC_SETUP_CLSU32:
		return ixgbe_setup_tc_cls_u32(adapter, type_data);
	default:
		return -EOPNOTSUPP;
	}
}

static int ixgbe_setup_tc_mqprio(struct net_device *dev,
				 struct tc_mqprio_qopt *mqprio)
{
	mqprio->hw = TC_MQPRIO_HW_OFFLOAD_TCS;
	return ixgbe_setup_tc(dev, mqprio->num_tc);
}

static LIST_HEAD(ixgbe_block_cb_list);

static int __ixgbe_setup_tc(struct net_device *dev, enum tc_setup_type type,
			    void *type_data)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);

	switch (type) {
	case TC_SETUP_BLOCK:
		return flow_block_cb_setup_simple(type_data,
						  &ixgbe_block_cb_list,
						  ixgbe_setup_tc_block_cb,
						  adapter, adapter, true);
	case TC_SETUP_QDISC_MQPRIO:
		return ixgbe_setup_tc_mqprio(dev, type_data);
	default:
		return -EOPNOTSUPP;
	}
}

#ifdef CONFIG_PCI_IOV
void ixgbe_sriov_reinit(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;

	rtnl_lock();
	ixgbe_setup_tc(netdev, adapter->hw_tcs);
	rtnl_unlock();
}

#endif
void ixgbe_do_reset(struct net_device *netdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	if (netif_running(netdev))
		ixgbe_reinit_locked(adapter);
	else
		ixgbe_reset(adapter);
}

static netdev_features_t ixgbe_fix_features(struct net_device *netdev,
					    netdev_features_t features)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);

	/* If Rx checksum is disabled, then RSC/LRO should also be disabled */
	if (!(features & NETIF_F_RXCSUM))
		features &= ~NETIF_F_LRO;

	/* Turn off LRO if not RSC capable */
	if (!(adapter->flags2 & IXGBE_FLAG2_RSC_CAPABLE))
		features &= ~NETIF_F_LRO;

	if (adapter->xdp_prog && (features & NETIF_F_LRO)) {
		e_dev_err("LRO is not supported with XDP\n");
		features &= ~NETIF_F_LRO;
	}

	return features;
}

static void ixgbe_reset_l2fw_offload(struct ixgbe_adapter *adapter)
{
	int rss = min_t(int, ixgbe_max_rss_indices(adapter),
			num_online_cpus());

	/* go back to full RSS if we're not running SR-IOV */
	if (!adapter->ring_feature[RING_F_VMDQ].offset)
		adapter->flags &= ~(IXGBE_FLAG_VMDQ_ENABLED |
				    IXGBE_FLAG_SRIOV_ENABLED);

	adapter->ring_feature[RING_F_RSS].limit = rss;
	adapter->ring_feature[RING_F_VMDQ].limit = 1;

	ixgbe_setup_tc(adapter->netdev, adapter->hw_tcs);
}

static int ixgbe_set_features(struct net_device *netdev,
			      netdev_features_t features)
{
	struct ixgbe_adapter *adapter = netdev_priv(netdev);
	netdev_features_t changed = netdev->features ^ features;
	bool need_reset = false;

	/* Make sure RSC matches LRO, reset if change */
	if (!(features & NETIF_F_LRO)) {
		if (adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED)
			need_reset = true;
		adapter->flags2 &= ~IXGBE_FLAG2_RSC_ENABLED;
	} else if ((adapter->flags2 & IXGBE_FLAG2_RSC_CAPABLE) &&
		   !(adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED)) {
		if (adapter->rx_itr_setting == 1 ||
		    adapter->rx_itr_setting > IXGBE_MIN_RSC_ITR) {
			adapter->flags2 |= IXGBE_FLAG2_RSC_ENABLED;
			need_reset = true;
		} else if ((changed ^ features) & NETIF_F_LRO) {
			e_info(probe, "rx-usecs set too low, "
			       "disabling RSC\n");
		}
	}

	/*
	 * Check if Flow Director n-tuple support or hw_tc support was
	 * enabled or disabled.  If the state changed, we need to reset.
	 */
	if ((features & NETIF_F_NTUPLE) || (features & NETIF_F_HW_TC)) {
		/* turn off ATR, enable perfect filters and reset */
		if (!(adapter->flags & IXGBE_FLAG_FDIR_PERFECT_CAPABLE))
			need_reset = true;

		adapter->flags &= ~IXGBE_FLAG_FDIR_HASH_CAPABLE;
		adapter->flags |= IXGBE_FLAG_FDIR_PERFECT_CAPABLE;
	} else {
		/* turn off perfect filters, enable ATR and reset */
		if (adapter->flags & IXGBE_FLAG_FDIR_PERFECT_CAPABLE)
			need_reset = true;

		adapter->flags &= ~IXGBE_FLAG_FDIR_PERFECT_CAPABLE;

		/* We cannot enable ATR if SR-IOV is enabled */
		if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED ||
		    /* We cannot enable ATR if we have 2 or more tcs */
		    (adapter->hw_tcs > 1) ||
		    /* We cannot enable ATR if RSS is disabled */
		    (adapter->ring_feature[RING_F_RSS].limit <= 1) ||
		    /* A sample rate of 0 indicates ATR disabled */
		    (!adapter->atr_sample_rate))
			; /* do nothing not supported */
		else /* otherwise supported and set the flag */
			adapter->flags |= IXGBE_FLAG_FDIR_HASH_CAPABLE;
	}

	if (changed & NETIF_F_RXALL)
		need_reset = true;

	netdev->features = features;

	if ((changed & NETIF_F_HW_L2FW_DOFFLOAD) && adapter->num_rx_pools > 1)
		ixgbe_reset_l2fw_offload(adapter);
	else if (need_reset)
		ixgbe_do_reset(netdev);
	else if (changed & (NETIF_F_HW_VLAN_CTAG_RX |
			    NETIF_F_HW_VLAN_CTAG_FILTER))
		ixgbe_set_rx_mode(netdev);

	return 1;
}

static int ixgbe_ndo_fdb_add(struct ndmsg *ndm, struct nlattr *tb[],
			     struct net_device *dev,
			     const unsigned char *addr, u16 vid,
			     u16 flags, bool *notified,
			     struct netlink_ext_ack *extack)
{
	/* guarantee we can provide a unique filter for the unicast address */
	if (is_unicast_ether_addr(addr) || is_link_local_ether_addr(addr)) {
		struct ixgbe_adapter *adapter = netdev_priv(dev);
		u16 pool = VMDQ_P(0);

		if (netdev_uc_count(dev) >= ixgbe_available_rars(adapter, pool))
			return -ENOMEM;
	}

	return ndo_dflt_fdb_add(ndm, tb, dev, addr, vid, flags);
}

/**
 * ixgbe_configure_bridge_mode - set various bridge modes
 * @adapter: the private structure
 * @mode: requested bridge mode
 *
 * Configure some settings require for various bridge modes.
 **/
static int ixgbe_configure_bridge_mode(struct ixgbe_adapter *adapter,
				       __u16 mode)
{
	struct ixgbe_hw *hw = &adapter->hw;
	unsigned int p, num_pools;
	u32 vmdctl;

	switch (mode) {
	case BRIDGE_MODE_VEPA:
		/* disable Tx loopback, rely on switch hairpin mode */
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_PFDTXGSWC, 0);

		/* must enable Rx switching replication to allow multicast
		 * packet reception on all VFs, and to enable source address
		 * pruning.
		 */
		vmdctl = IXGBE_READ_REG(hw, IXGBE_VMD_CTL);
		vmdctl |= IXGBE_VT_CTL_REPLEN;
		IXGBE_WRITE_REG(hw, IXGBE_VMD_CTL, vmdctl);

		/* enable Rx source address pruning. Note, this requires
		 * replication to be enabled or else it does nothing.
		 */
		num_pools = adapter->num_vfs + adapter->num_rx_pools;
		for (p = 0; p < num_pools; p++) {
			if (hw->mac.ops.set_source_address_pruning)
				hw->mac.ops.set_source_address_pruning(hw,
								       true,
								       p);
		}
		break;
	case BRIDGE_MODE_VEB:
		/* enable Tx loopback for internal VF/PF communication */
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_PFDTXGSWC,
				IXGBE_PFDTXGSWC_VT_LBEN);

		/* disable Rx switching replication unless we have SR-IOV
		 * virtual functions
		 */
		vmdctl = IXGBE_READ_REG(hw, IXGBE_VMD_CTL);
		if (!adapter->num_vfs)
			vmdctl &= ~IXGBE_VT_CTL_REPLEN;
		IXGBE_WRITE_REG(hw, IXGBE_VMD_CTL, vmdctl);

		/* disable Rx source address pruning, since we don't expect to
		 * be receiving external loopback of our transmitted frames.
		 */
		num_pools = adapter->num_vfs + adapter->num_rx_pools;
		for (p = 0; p < num_pools; p++) {
			if (hw->mac.ops.set_source_address_pruning)
				hw->mac.ops.set_source_address_pruning(hw,
								       false,
								       p);
		}
		break;
	default:
		return -EINVAL;
	}

	adapter->bridge_mode = mode;

	e_info(drv, "enabling bridge mode: %s\n",
	       mode == BRIDGE_MODE_VEPA ? "VEPA" : "VEB");

	return 0;
}

static int ixgbe_ndo_bridge_setlink(struct net_device *dev,
				    struct nlmsghdr *nlh, u16 flags,
				    struct netlink_ext_ack *extack)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct nlattr *attr, *br_spec;
	int rem;

	if (!(adapter->flags & IXGBE_FLAG_SRIOV_ENABLED))
		return -EOPNOTSUPP;

	br_spec = nlmsg_find_attr(nlh, sizeof(struct ifinfomsg), IFLA_AF_SPEC);
	if (!br_spec)
		return -EINVAL;

	nla_for_each_nested_type(attr, IFLA_BRIDGE_MODE, br_spec, rem) {
		__u16 mode = nla_get_u16(attr);
		int status = ixgbe_configure_bridge_mode(adapter, mode);

		if (status)
			return status;

		break;
	}

	return 0;
}

static int ixgbe_ndo_bridge_getlink(struct sk_buff *skb, u32 pid, u32 seq,
				    struct net_device *dev,
				    u32 filter_mask, int nlflags)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);

	if (!(adapter->flags & IXGBE_FLAG_SRIOV_ENABLED))
		return 0;

	return ndo_dflt_bridge_getlink(skb, pid, seq, dev,
				       adapter->bridge_mode, 0, 0, nlflags,
				       filter_mask, NULL);
}

static void *ixgbe_fwd_add(struct net_device *pdev, struct net_device *vdev)
{
	struct ixgbe_adapter *adapter = netdev_priv(pdev);
	struct ixgbe_fwd_adapter *accel;
	int tcs = adapter->hw_tcs ? : 1;
	int pool, err;

	if (adapter->xdp_prog) {
		e_warn(probe, "L2FW offload is not supported with XDP\n");
		return ERR_PTR(-EINVAL);
	}

	/* The hardware supported by ixgbe only filters on the destination MAC
	 * address. In order to avoid issues we only support offloading modes
	 * where the hardware can actually provide the functionality.
	 */
	if (!macvlan_supports_dest_filter(vdev))
		return ERR_PTR(-EMEDIUMTYPE);

	/* We need to lock down the macvlan to be a single queue device so that
	 * we can reuse the tc_to_txq field in the macvlan netdev to represent
	 * the queue mapping to our netdev.
	 */
	if (netif_is_multiqueue(vdev))
		return ERR_PTR(-ERANGE);

	pool = find_first_zero_bit(adapter->fwd_bitmask, adapter->num_rx_pools);
	if (pool == adapter->num_rx_pools) {
		u16 used_pools = adapter->num_vfs + adapter->num_rx_pools;
		u16 reserved_pools;

		if (((adapter->flags & IXGBE_FLAG_DCB_ENABLED) &&
		     adapter->num_rx_pools >= (MAX_TX_QUEUES / tcs)) ||
		    adapter->num_rx_pools > IXGBE_MAX_MACVLANS)
			return ERR_PTR(-EBUSY);

		/* Hardware has a limited number of available pools. Each VF,
		 * and the PF require a pool. Check to ensure we don't
		 * attempt to use more then the available number of pools.
		 */
		if (used_pools >= IXGBE_MAX_VF_FUNCTIONS)
			return ERR_PTR(-EBUSY);

		/* Enable VMDq flag so device will be set in VM mode */
		adapter->flags |= IXGBE_FLAG_VMDQ_ENABLED |
				  IXGBE_FLAG_SRIOV_ENABLED;

		/* Try to reserve as many queues per pool as possible,
		 * we start with the configurations that support 4 queues
		 * per pools, followed by 2, and then by just 1 per pool.
		 */
		if (used_pools < 32 && adapter->num_rx_pools < 16)
			reserved_pools = min_t(u16,
					       32 - used_pools,
					       16 - adapter->num_rx_pools);
		else if (adapter->num_rx_pools < 32)
			reserved_pools = min_t(u16,
					       64 - used_pools,
					       32 - adapter->num_rx_pools);
		else
			reserved_pools = 64 - used_pools;


		if (!reserved_pools)
			return ERR_PTR(-EBUSY);

		adapter->ring_feature[RING_F_VMDQ].limit += reserved_pools;

		/* Force reinit of ring allocation with VMDQ enabled */
		err = ixgbe_setup_tc(pdev, adapter->hw_tcs);
		if (err)
			return ERR_PTR(err);

		if (pool >= adapter->num_rx_pools)
			return ERR_PTR(-ENOMEM);
	}

	accel = kzalloc(sizeof(*accel), GFP_KERNEL);
	if (!accel)
		return ERR_PTR(-ENOMEM);

	set_bit(pool, adapter->fwd_bitmask);
	netdev_set_sb_channel(vdev, pool);
	accel->pool = pool;
	accel->netdev = vdev;

	if (!netif_running(pdev))
		return accel;

	err = ixgbe_fwd_ring_up(adapter, accel);
	if (err)
		return ERR_PTR(err);

	return accel;
}

static void ixgbe_fwd_del(struct net_device *pdev, void *priv)
{
	struct ixgbe_fwd_adapter *accel = priv;
	struct ixgbe_adapter *adapter = netdev_priv(pdev);
	unsigned int rxbase = accel->rx_base_queue;
	unsigned int i;

	/* delete unicast filter associated with offloaded interface */
	ixgbe_del_mac_filter(adapter, accel->netdev->dev_addr,
			     VMDQ_P(accel->pool));

	/* Allow remaining Rx packets to get flushed out of the
	 * Rx FIFO before we drop the netdev for the ring.
	 */
	usleep_range(10000, 20000);

	for (i = 0; i < adapter->num_rx_queues_per_pool; i++) {
		struct ixgbe_ring *ring = adapter->rx_ring[rxbase + i];
		struct ixgbe_q_vector *qv = ring->q_vector;

		/* Make sure we aren't processing any packets and clear
		 * netdev to shut down the ring.
		 */
		if (netif_running(adapter->netdev))
			napi_synchronize(&qv->napi);
		ring->netdev = NULL;
	}

	/* unbind the queues and drop the subordinate channel config */
	netdev_unbind_sb_channel(pdev, accel->netdev);
	netdev_set_sb_channel(accel->netdev, 0);

	clear_bit(accel->pool, adapter->fwd_bitmask);
	kfree(accel);
}

#define IXGBE_MAX_MAC_HDR_LEN		127
#define IXGBE_MAX_NETWORK_HDR_LEN	511

static netdev_features_t
ixgbe_features_check(struct sk_buff *skb, struct net_device *dev,
		     netdev_features_t features)
{
	unsigned int network_hdr_len, mac_hdr_len;

	/* Make certain the headers can be described by a context descriptor */
	mac_hdr_len = skb_network_offset(skb);
	if (unlikely(mac_hdr_len > IXGBE_MAX_MAC_HDR_LEN))
		return features & ~(NETIF_F_HW_CSUM |
				    NETIF_F_SCTP_CRC |
				    NETIF_F_GSO_UDP_L4 |
				    NETIF_F_HW_VLAN_CTAG_TX |
				    NETIF_F_TSO |
				    NETIF_F_TSO6);

	network_hdr_len = skb_checksum_start(skb) - skb_network_header(skb);
	if (unlikely(network_hdr_len >  IXGBE_MAX_NETWORK_HDR_LEN))
		return features & ~(NETIF_F_HW_CSUM |
				    NETIF_F_SCTP_CRC |
				    NETIF_F_GSO_UDP_L4 |
				    NETIF_F_TSO |
				    NETIF_F_TSO6);

	/* We can only support IPV4 TSO in tunnels if we can mangle the
	 * inner IP ID field, so strip TSO if MANGLEID is not supported.
	 * IPsec offoad sets skb->encapsulation but still can handle
	 * the TSO, so it's the exception.
	 */
	if (skb->encapsulation && !(features & NETIF_F_TSO_MANGLEID)) {
#ifdef CONFIG_IXGBE_IPSEC
		if (!secpath_exists(skb))
#endif
			features &= ~NETIF_F_TSO;
	}

	return features;
}

static int ixgbe_xdp_setup(struct net_device *dev, struct bpf_prog *prog)
{
	int i, frame_size = dev->mtu + ETH_HLEN + ETH_FCS_LEN + VLAN_HLEN;
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct bpf_prog *old_prog;
	bool need_reset;
	int num_queues;

	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED)
		return -EINVAL;

	if (adapter->flags & IXGBE_FLAG_DCB_ENABLED)
		return -EINVAL;

	/* verify ixgbe ring attributes are sufficient for XDP */
	for (i = 0; i < adapter->num_rx_queues; i++) {
		struct ixgbe_ring *ring = adapter->rx_ring[i];

		if (ring_is_rsc_enabled(ring))
			return -EINVAL;

		if (frame_size > ixgbe_rx_bufsz(ring))
			return -EINVAL;
	}

	/* if the number of cpus is much larger than the maximum of queues,
	 * we should stop it and then return with ENOMEM like before.
	 */
	if (nr_cpu_ids > IXGBE_MAX_XDP_QS * 2)
		return -ENOMEM;

	old_prog = xchg(&adapter->xdp_prog, prog);
	need_reset = (!!prog != !!old_prog);

	/* If transitioning XDP modes reconfigure rings */
	if (need_reset) {
		int err;

		if (!prog)
			/* Wait until ndo_xsk_wakeup completes. */
			synchronize_rcu();
		err = ixgbe_setup_tc(dev, adapter->hw_tcs);

		if (err)
			return -EINVAL;
		if (!prog)
			xdp_features_clear_redirect_target(dev);
	} else {
		for (i = 0; i < adapter->num_rx_queues; i++) {
			WRITE_ONCE(adapter->rx_ring[i]->xdp_prog,
				   adapter->xdp_prog);
		}
	}

	if (old_prog)
		bpf_prog_put(old_prog);

	/* Kick start the NAPI context if there is an AF_XDP socket open
	 * on that queue id. This so that receiving will start.
	 */
	if (need_reset && prog) {
		num_queues = min_t(int, adapter->num_rx_queues,
				   adapter->num_xdp_queues);
		for (i = 0; i < num_queues; i++)
			if (adapter->xdp_ring[i]->xsk_pool)
				(void)ixgbe_xsk_wakeup(adapter->netdev, i,
						       XDP_WAKEUP_RX);
		xdp_features_set_redirect_target(dev, true);
	}

	return 0;
}

static int ixgbe_xdp(struct net_device *dev, struct netdev_bpf *xdp)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);

	switch (xdp->command) {
	case XDP_SETUP_PROG:
		return ixgbe_xdp_setup(dev, xdp->prog);
	case XDP_SETUP_XSK_POOL:
		return ixgbe_xsk_pool_setup(adapter, xdp->xsk.pool,
					    xdp->xsk.queue_id);

	default:
		return -EINVAL;
	}
}

void ixgbe_xdp_ring_update_tail(struct ixgbe_ring *ring)
{
	/* Force memory writes to complete before letting h/w know there
	 * are new descriptors to fetch.
	 */
	wmb();
	writel(ring->next_to_use, ring->tail);
}

void ixgbe_xdp_ring_update_tail_locked(struct ixgbe_ring *ring)
{
	if (static_branch_unlikely(&ixgbe_xdp_locking_key))
		spin_lock(&ring->tx_lock);
	ixgbe_xdp_ring_update_tail(ring);
	if (static_branch_unlikely(&ixgbe_xdp_locking_key))
		spin_unlock(&ring->tx_lock);
}

static int ixgbe_xdp_xmit(struct net_device *dev, int n,
			  struct xdp_frame **frames, u32 flags)
{
	struct ixgbe_adapter *adapter = netdev_priv(dev);
	struct ixgbe_ring *ring;
	int nxmit = 0;
	int i;

	if (unlikely(test_bit(__IXGBE_DOWN, &adapter->state)))
		return -ENETDOWN;

	if (unlikely(flags & ~XDP_XMIT_FLAGS_MASK))
		return -EINVAL;

	/* During program transitions its possible adapter->xdp_prog is assigned
	 * but ring has not been configured yet. In this case simply abort xmit.
	 */
	ring = adapter->xdp_prog ? ixgbe_determine_xdp_ring(adapter) : NULL;
	if (unlikely(!ring))
		return -ENXIO;

	if (unlikely(test_bit(__IXGBE_TX_DISABLED, &ring->state)))
		return -ENXIO;

	if (static_branch_unlikely(&ixgbe_xdp_locking_key))
		spin_lock(&ring->tx_lock);

	for (i = 0; i < n; i++) {
		struct xdp_frame *xdpf = frames[i];
		int err;

		err = ixgbe_xmit_xdp_ring(ring, xdpf);
		if (err != IXGBE_XDP_TX)
			break;
		nxmit++;
	}

	if (unlikely(flags & XDP_XMIT_FLUSH))
		ixgbe_xdp_ring_update_tail(ring);

	if (static_branch_unlikely(&ixgbe_xdp_locking_key))
		spin_unlock(&ring->tx_lock);

	return nxmit;
}

static const struct net_device_ops ixgbe_netdev_ops = {
	.ndo_open		= ixgbe_open,
	.ndo_stop		= ixgbe_close,
	.ndo_start_xmit		= ixgbe_xmit_frame,
	.ndo_set_rx_mode	= ixgbe_set_rx_mode,
	.ndo_validate_addr	= eth_validate_addr,
	.ndo_set_mac_address	= ixgbe_set_mac,
	.ndo_change_mtu		= ixgbe_change_mtu,
	.ndo_tx_timeout		= ixgbe_tx_timeout,
	.ndo_set_tx_maxrate	= ixgbe_tx_maxrate,
	.ndo_vlan_rx_add_vid	= ixgbe_vlan_rx_add_vid,
	.ndo_vlan_rx_kill_vid	= ixgbe_vlan_rx_kill_vid,
	.ndo_eth_ioctl		= ixgbe_ioctl,
	.ndo_set_vf_mac		= ixgbe_ndo_set_vf_mac,
	.ndo_set_vf_vlan	= ixgbe_ndo_set_vf_vlan,
	.ndo_set_vf_rate	= ixgbe_ndo_set_vf_bw,
	.ndo_set_vf_spoofchk	= ixgbe_ndo_set_vf_spoofchk,
	.ndo_set_vf_link_state	= ixgbe_ndo_set_vf_link_state,
	.ndo_set_vf_rss_query_en = ixgbe_ndo_set_vf_rss_query_en,
	.ndo_set_vf_trust	= ixgbe_ndo_set_vf_trust,
	.ndo_get_vf_config	= ixgbe_ndo_get_vf_config,
	.ndo_get_vf_stats	= ixgbe_ndo_get_vf_stats,
	.ndo_get_stats64	= ixgbe_get_stats64,
	.ndo_setup_tc		= __ixgbe_setup_tc,
#ifdef IXGBE_FCOE
	.ndo_select_queue	= ixgbe_select_queue,
	.ndo_fcoe_ddp_setup = ixgbe_fcoe_ddp_get,
	.ndo_fcoe_ddp_target = ixgbe_fcoe_ddp_target,
	.ndo_fcoe_ddp_done = ixgbe_fcoe_ddp_put,
	.ndo_fcoe_enable = ixgbe_fcoe_enable,
	.ndo_fcoe_disable = ixgbe_fcoe_disable,
	.ndo_fcoe_get_wwn = ixgbe_fcoe_get_wwn,
	.ndo_fcoe_get_hbainfo = ixgbe_fcoe_get_hbainfo,
#endif /* IXGBE_FCOE */
	.ndo_set_features = ixgbe_set_features,
	.ndo_fix_features = ixgbe_fix_features,
	.ndo_fdb_add		= ixgbe_ndo_fdb_add,
	.ndo_bridge_setlink	= ixgbe_ndo_bridge_setlink,
	.ndo_bridge_getlink	= ixgbe_ndo_bridge_getlink,
	.ndo_dfwd_add_station	= ixgbe_fwd_add,
	.ndo_dfwd_del_station	= ixgbe_fwd_del,
	.ndo_features_check	= ixgbe_features_check,
	.ndo_bpf		= ixgbe_xdp,
	.ndo_xdp_xmit		= ixgbe_xdp_xmit,
	.ndo_xsk_wakeup         = ixgbe_xsk_wakeup,
};

static void ixgbe_disable_txr_hw(struct ixgbe_adapter *adapter,
				 struct ixgbe_ring *tx_ring)
{
	unsigned long wait_delay, delay_interval;
	struct ixgbe_hw *hw = &adapter->hw;
	u8 reg_idx = tx_ring->reg_idx;
	int wait_loop;
	u32 txdctl;

	IXGBE_WRITE_REG(hw, IXGBE_TXDCTL(reg_idx), IXGBE_TXDCTL_SWFLSH);

	/* delay mechanism from ixgbe_disable_tx */
	delay_interval = ixgbe_get_completion_timeout(adapter) / 100;

	wait_loop = IXGBE_MAX_RX_DESC_POLL;
	wait_delay = delay_interval;

	while (wait_loop--) {
		usleep_range(wait_delay, wait_delay + 10);
		wait_delay += delay_interval * 2;
		txdctl = IXGBE_READ_REG(hw, IXGBE_TXDCTL(reg_idx));

		if (!(txdctl & IXGBE_TXDCTL_ENABLE))
			return;
	}

	e_err(drv, "TXDCTL.ENABLE not cleared within the polling period\n");
}

static void ixgbe_disable_txr(struct ixgbe_adapter *adapter,
			      struct ixgbe_ring *tx_ring)
{
	set_bit(__IXGBE_TX_DISABLED, &tx_ring->state);
	ixgbe_disable_txr_hw(adapter, tx_ring);
}

static void ixgbe_disable_rxr_hw(struct ixgbe_adapter *adapter,
				 struct ixgbe_ring *rx_ring)
{
	unsigned long wait_delay, delay_interval;
	struct ixgbe_hw *hw = &adapter->hw;
	u8 reg_idx = rx_ring->reg_idx;
	int wait_loop;
	u32 rxdctl;

	rxdctl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg_idx));
	rxdctl &= ~IXGBE_RXDCTL_ENABLE;
	rxdctl |= IXGBE_RXDCTL_SWFLSH;

	/* write value back with RXDCTL.ENABLE bit cleared */
	IXGBE_WRITE_REG(hw, IXGBE_RXDCTL(reg_idx), rxdctl);

	/* RXDCTL.EN may not change on 82598 if link is down, so skip it */
	if (hw->mac.type == ixgbe_mac_82598EB &&
	    !(IXGBE_READ_REG(hw, IXGBE_LINKS) & IXGBE_LINKS_UP))
		return;

	/* delay mechanism from ixgbe_disable_rx */
	delay_interval = ixgbe_get_completion_timeout(adapter) / 100;

	wait_loop = IXGBE_MAX_RX_DESC_POLL;
	wait_delay = delay_interval;

	while (wait_loop--) {
		usleep_range(wait_delay, wait_delay + 10);
		wait_delay += delay_interval * 2;
		rxdctl = IXGBE_READ_REG(hw, IXGBE_RXDCTL(reg_idx));

		if (!(rxdctl & IXGBE_RXDCTL_ENABLE))
			return;
	}

	e_err(drv, "RXDCTL.ENABLE not cleared within the polling period\n");
}

static void ixgbe_reset_txr_stats(struct ixgbe_ring *tx_ring)
{
	memset(&tx_ring->stats, 0, sizeof(tx_ring->stats));
	memset(&tx_ring->tx_stats, 0, sizeof(tx_ring->tx_stats));
}

static void ixgbe_reset_rxr_stats(struct ixgbe_ring *rx_ring)
{
	memset(&rx_ring->stats, 0, sizeof(rx_ring->stats));
	memset(&rx_ring->rx_stats, 0, sizeof(rx_ring->rx_stats));
}

/**
 * ixgbe_irq_disable_single - Disable single IRQ vector
 * @adapter: adapter structure
 * @ring: ring index
 **/
static void ixgbe_irq_disable_single(struct ixgbe_adapter *adapter, u32 ring)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u64 qmask = BIT_ULL(ring);
	u32 mask;

	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82598EB:
		mask = qmask & IXGBE_EIMC_RTX_QUEUE;
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_EIMC, mask);
		break;
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
		mask = (qmask & 0xFFFFFFFF);
		if (mask)
			IXGBE_WRITE_REG(hw, IXGBE_EIMS_EX(0), mask);
		mask = (qmask >> 32);
		if (mask)
			IXGBE_WRITE_REG(hw, IXGBE_EIMS_EX(1), mask);
		break;
	default:
		break;
	}
	IXGBE_WRITE_FLUSH(&adapter->hw);
	if (adapter->flags & IXGBE_FLAG_MSIX_ENABLED)
		synchronize_irq(adapter->msix_entries[ring].vector);
	else
		synchronize_irq(adapter->pdev->irq);
}

/**
 * ixgbe_txrx_ring_disable - Disable Rx/Tx/XDP Tx rings
 * @adapter: adapter structure
 * @ring: ring index
 *
 * This function disables a certain Rx/Tx/XDP Tx ring. The function
 * assumes that the netdev is running.
 **/
void ixgbe_txrx_ring_disable(struct ixgbe_adapter *adapter, int ring)
{
	struct ixgbe_ring *rx_ring, *tx_ring, *xdp_ring;

	rx_ring = adapter->rx_ring[ring];
	tx_ring = adapter->tx_ring[ring];
	xdp_ring = adapter->xdp_ring[ring];

	ixgbe_irq_disable_single(adapter, ring);

	/* Rx/Tx/XDP Tx share the same napi context. */
	napi_disable(&rx_ring->q_vector->napi);

	ixgbe_disable_txr(adapter, tx_ring);
	if (xdp_ring)
		ixgbe_disable_txr(adapter, xdp_ring);
	ixgbe_disable_rxr_hw(adapter, rx_ring);

	if (xdp_ring)
		synchronize_rcu();

	ixgbe_clean_tx_ring(tx_ring);
	if (xdp_ring)
		ixgbe_clean_tx_ring(xdp_ring);
	ixgbe_clean_rx_ring(rx_ring);

	ixgbe_reset_txr_stats(tx_ring);
	if (xdp_ring)
		ixgbe_reset_txr_stats(xdp_ring);
	ixgbe_reset_rxr_stats(rx_ring);
}

/**
 * ixgbe_txrx_ring_enable - Enable Rx/Tx/XDP Tx rings
 * @adapter: adapter structure
 * @ring: ring index
 *
 * This function enables a certain Rx/Tx/XDP Tx ring. The function
 * assumes that the netdev is running.
 **/
void ixgbe_txrx_ring_enable(struct ixgbe_adapter *adapter, int ring)
{
	struct ixgbe_ring *rx_ring, *tx_ring, *xdp_ring;

	rx_ring = adapter->rx_ring[ring];
	tx_ring = adapter->tx_ring[ring];
	xdp_ring = adapter->xdp_ring[ring];

	ixgbe_configure_tx_ring(adapter, tx_ring);
	if (xdp_ring)
		ixgbe_configure_tx_ring(adapter, xdp_ring);
	ixgbe_configure_rx_ring(adapter, rx_ring);

	clear_bit(__IXGBE_TX_DISABLED, &tx_ring->state);
	if (xdp_ring)
		clear_bit(__IXGBE_TX_DISABLED, &xdp_ring->state);

	/* Rx/Tx/XDP Tx share the same napi context. */
	napi_enable(&rx_ring->q_vector->napi);
	ixgbe_irq_enable_queues(adapter, BIT_ULL(ring));
	IXGBE_WRITE_FLUSH(&adapter->hw);
}

/**
 * ixgbe_enumerate_functions - Get the number of ports this device has
 * @adapter: adapter structure
 *
 * This function enumerates the phsyical functions co-located on a single slot,
 * in order to determine how many ports a device has. This is most useful in
 * determining the required GT/s of PCIe bandwidth necessary for optimal
 * performance.
 **/
static inline int ixgbe_enumerate_functions(struct ixgbe_adapter *adapter)
{
	struct pci_dev *entry, *pdev = adapter->pdev;
	int physfns = 0;

	/* Some cards can not use the generic count PCIe functions method,
	 * because they are behind a parent switch, so we hardcode these with
	 * the correct number of functions.
	 */
	if (ixgbe_pcie_from_parent(&adapter->hw))
		physfns = 4;

	list_for_each_entry(entry, &adapter->pdev->bus->devices, bus_list) {
		/* don't count virtual functions */
		if (entry->is_virtfn)
			continue;

		/* When the devices on the bus don't all match our device ID,
		 * we can't reliably determine the correct number of
		 * functions. This can occur if a function has been direct
		 * attached to a virtual machine using VT-d, for example. In
		 * this case, simply return -1 to indicate this.
		 */
		if ((entry->vendor != pdev->vendor) ||
		    (entry->device != pdev->device))
			return -1;

		physfns++;
	}

	return physfns;
}

/**
 * ixgbe_wol_supported - Check whether device supports WoL
 * @adapter: the adapter private structure
 * @device_id: the device ID
 * @subdevice_id: the subsystem device ID
 *
 * This function is used by probe and ethtool to determine
 * which devices have WoL support
 *
 **/
bool ixgbe_wol_supported(struct ixgbe_adapter *adapter, u16 device_id,
			 u16 subdevice_id)
{
	struct ixgbe_hw *hw = &adapter->hw;
	u16 wol_cap = adapter->eeprom_cap & IXGBE_DEVICE_CAPS_WOL_MASK;

	/* WOL not supported on 82598 */
	if (hw->mac.type == ixgbe_mac_82598EB)
		return false;

	/* check eeprom to see if WOL is enabled for X540 and newer */
	if (hw->mac.type >= ixgbe_mac_X540) {
		if ((wol_cap == IXGBE_DEVICE_CAPS_WOL_PORT0_1) ||
		    ((wol_cap == IXGBE_DEVICE_CAPS_WOL_PORT0) &&
		     (hw->bus.func == 0)))
			return true;
	}

	/* WOL is determined based on device IDs for 82599 MACs */
	switch (device_id) {
	case IXGBE_DEV_ID_82599_SFP:
		/* Only these subdevices could supports WOL */
		switch (subdevice_id) {
		case IXGBE_SUBDEV_ID_82599_560FLR:
		case IXGBE_SUBDEV_ID_82599_LOM_SNAP6:
		case IXGBE_SUBDEV_ID_82599_SFP_WOL0:
		case IXGBE_SUBDEV_ID_82599_SFP_2OCP:
			/* only support first port */
			if (hw->bus.func != 0)
				break;
			fallthrough;
		case IXGBE_SUBDEV_ID_82599_SP_560FLR:
		case IXGBE_SUBDEV_ID_82599_SFP:
		case IXGBE_SUBDEV_ID_82599_RNDC:
		case IXGBE_SUBDEV_ID_82599_ECNA_DP:
		case IXGBE_SUBDEV_ID_82599_SFP_1OCP:
		case IXGBE_SUBDEV_ID_82599_SFP_LOM_OEM1:
		case IXGBE_SUBDEV_ID_82599_SFP_LOM_OEM2:
			return true;
		}
		break;
	case IXGBE_DEV_ID_82599EN_SFP:
		/* Only these subdevices support WOL */
		switch (subdevice_id) {
		case IXGBE_SUBDEV_ID_82599EN_SFP_OCP1:
			return true;
		}
		break;
	case IXGBE_DEV_ID_82599_COMBO_BACKPLANE:
		/* All except this subdevice support WOL */
		if (subdevice_id != IXGBE_SUBDEV_ID_82599_KX4_KR_MEZZ)
			return true;
		break;
	case IXGBE_DEV_ID_82599_KX4:
		return  true;
	default:
		break;
	}

	return false;
}

/**
 * ixgbe_set_fw_version_e610 - Set FW version specifically on E610 adapters
 * @adapter: the adapter private structure
 *
 * This function is used by probe and ethtool to determine the FW version to
 * format to display. The FW version is taken from the EEPROM/NVM.
 *
 */
static void ixgbe_set_fw_version_e610(struct ixgbe_adapter *adapter)
{
	struct ixgbe_orom_info *orom = &adapter->hw.flash.orom;
	struct ixgbe_nvm_info *nvm = &adapter->hw.flash.nvm;

	snprintf(adapter->eeprom_id, sizeof(adapter->eeprom_id),
		 "%x.%02x 0x%x %d.%d.%d", nvm->major, nvm->minor,
		 nvm->eetrack, orom->major, orom->build, orom->patch);
}

/**
 * ixgbe_set_fw_version - Set FW version
 * @adapter: the adapter private structure
 *
 * This function is used by probe and ethtool to determine the FW version to
 * format to display. The FW version is taken from the EEPROM/NVM.
 */
static void ixgbe_set_fw_version(struct ixgbe_adapter *adapter)
{
	struct ixgbe_hw *hw = &adapter->hw;
	struct ixgbe_nvm_version nvm_ver;

	if (adapter->hw.mac.type == ixgbe_mac_e610) {
		ixgbe_set_fw_version_e610(adapter);
		return;
	}

	ixgbe_get_oem_prod_version(hw, &nvm_ver);
	if (nvm_ver.oem_valid) {
		snprintf(adapter->eeprom_id, sizeof(adapter->eeprom_id),
			 "%x.%x.%x", nvm_ver.oem_major, nvm_ver.oem_minor,
			 nvm_ver.oem_release);
		return;
	}

	ixgbe_get_etk_id(hw, &nvm_ver);
	ixgbe_get_orom_version(hw, &nvm_ver);

	if (nvm_ver.or_valid) {
		snprintf(adapter->eeprom_id, sizeof(adapter->eeprom_id),
			 "0x%08x, %d.%d.%d", nvm_ver.etk_id, nvm_ver.or_major,
			 nvm_ver.or_build, nvm_ver.or_patch);
		return;
	}

	/* Set ETrack ID format */
	snprintf(adapter->eeprom_id, sizeof(adapter->eeprom_id),
		 "0x%08x", nvm_ver.etk_id);
}

/**
 * ixgbe_probe - Device Initialization Routine
 * @pdev: PCI device information struct
 * @ent: entry in ixgbe_pci_tbl
 *
 * Returns 0 on success, negative on failure
 *
 * ixgbe_probe initializes an adapter identified by a pci_dev structure.
 * The OS initialization, configuring of the adapter private structure,
 * and a hardware reset occur.
 */
/**
 * ixgbe_probe - Device Initialization Routine
 * @pdev: PCI device information struct
 * @ent: entry in ixgbe_pci_tbl
 *
 * Returns 0 on success, negative on failure
 *
 * ixgbe_probe initializes an adapter identified by a pci_dev structure.
 * The OS initialization, configuring of the adapter private structure,
 * and a hardware reset occur.
 **/
static int ixgbe_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
{
	struct net_device *netdev;
	struct ixgbe_adapter *adapter = NULL;
	struct ixgbe_hw *hw;
	const struct ixgbe_info *ii = ixgbe_info_tbl[ent->driver_data];
	unsigned int indices = MAX_TX_QUEUES;
	u8 part_str[IXGBE_PBANUM_LENGTH];
	int i, err, expected_gts;
	bool disable_dev = false;
#ifdef IXGBE_FCOE
	u16 device_caps;
#endif
	u32 eec;

	/* Catch broken hardware that put the wrong VF device ID in
	 * the PCIe SR-IOV capability.
	 */
	if (pdev->is_virtfn) {
		WARN(1, KERN_ERR "%s (%hx:%hx) should not be a VF!\n",
		     pci_name(pdev), pdev->vendor, pdev->device);
		return -EINVAL;
	}

	err = pci_enable_device_mem(pdev);
	if (err)
		return err;

	err = dma_set_mask_and_coherent(&pdev->dev, DMA_BIT_MASK(64));
	if (err) {
		dev_err(&pdev->dev,
			"No usable DMA configuration, aborting\n");
		goto err_dma;
	}

	err = pci_request_mem_regions(pdev, ixgbe_driver_name);
	if (err) {
		dev_err(&pdev->dev,
			"pci_request_selected_regions failed 0x%x\n", err);
		goto err_pci_reg;
	}

	pci_set_master(pdev);
	pci_save_state(pdev);

	if (ii->mac == ixgbe_mac_82598EB) {
#ifdef CONFIG_IXGBE_DCB
		/* 8 TC w/ 4 queues per TC */
		indices = 4 * MAX_TRAFFIC_CLASS;
#else
		indices = IXGBE_MAX_RSS_INDICES;
#endif
	} else if (ii->mac == ixgbe_mac_e610) {
		indices = IXGBE_MAX_RSS_INDICES_X550;
	}

	netdev = alloc_etherdev_mq(sizeof(struct ixgbe_adapter), indices);
	if (!netdev) {
		err = -ENOMEM;
		goto err_alloc_etherdev;
	}

	SET_NETDEV_DEV(netdev, &pdev->dev);

	adapter = netdev_priv(netdev);

	adapter->netdev = netdev;
	adapter->pdev = pdev;
	hw = &adapter->hw;
	hw->back = adapter;
	adapter->msg_enable = netif_msg_init(debug, DEFAULT_MSG_ENABLE);

	hw->hw_addr = ioremap(pci_resource_start(pdev, 0),
			      pci_resource_len(pdev, 0));
	adapter->io_addr = hw->hw_addr;
	if (!hw->hw_addr) {
		err = -EIO;
		goto err_ioremap;
	}

	netdev->netdev_ops = &ixgbe_netdev_ops;
	ixgbe_set_ethtool_ops(netdev);
	netdev->watchdog_timeo = 5 * HZ;
	strscpy(netdev->name, pci_name(pdev), sizeof(netdev->name));

	/* Setup hw api */
	hw->mac.ops   = *ii->mac_ops;
	hw->mac.type  = ii->mac;
	hw->mvals     = ii->mvals;
	if (ii->link_ops)
		hw->link.ops  = *ii->link_ops;

	/* EEPROM */
	hw->eeprom.ops = *ii->eeprom_ops;
	eec = IXGBE_READ_REG(hw, IXGBE_EEC(hw));
	if (ixgbe_removed(hw->hw_addr)) {
		err = -EIO;
		goto err_ioremap;
	}
	/* If EEPROM is valid (bit 8 = 1), use default otherwise use bit bang */
	if (!(eec & BIT(8)))
		hw->eeprom.ops.read = &ixgbe_read_eeprom_bit_bang_generic;

	/* PHY */
	hw->phy.ops = *ii->phy_ops;
	hw->phy.sfp_type = ixgbe_sfp_type_unknown;
	/* ixgbe_identify_phy_generic will set prtad and mmds properly */
	hw->phy.mdio.prtad = MDIO_PRTAD_NONE;
	hw->phy.mdio.mmds = 0;
	hw->phy.mdio.mode_support = MDIO_SUPPORTS_C45 | MDIO_EMULATE_C22;
	hw->phy.mdio.dev = netdev;
	hw->phy.mdio.mdio_read = ixgbe_mdio_read;
	hw->phy.mdio.mdio_write = ixgbe_mdio_write;

	/* setup the private structure */
	err = ixgbe_sw_init(adapter, ii);
	if (err)
		goto err_sw_init;

	if (adapter->hw.mac.type == ixgbe_mac_e610) {
		err = ixgbe_get_caps(&adapter->hw);
		if (err)
			dev_err(&pdev->dev, "ixgbe_get_caps failed %d\n", err);
	}

	if (adapter->hw.mac.type == ixgbe_mac_82599EB)
		adapter->flags2 |= IXGBE_FLAG2_AUTO_DISABLE_VF;

	switch (adapter->hw.mac.type) {
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_e610:
		netdev->udp_tunnel_nic_info = &ixgbe_udp_tunnels_x550;
		break;
	case ixgbe_mac_x550em_a:
		netdev->udp_tunnel_nic_info = &ixgbe_udp_tunnels_x550em_a;
		break;
	default:
		break;
	}

	/* Make sure the SWFW semaphore is in a valid state */
	if (hw->mac.ops.init_swfw_sync)
		hw->mac.ops.init_swfw_sync(hw);

	/* Make it possible the adapter to be woken up via WOL */
	switch (adapter->hw.mac.type) {
	case ixgbe_mac_82599EB:
	case ixgbe_mac_X540:
	case ixgbe_mac_X550:
	case ixgbe_mac_X550EM_x:
	case ixgbe_mac_x550em_a:
	case ixgbe_mac_e610:
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_WUS, ~0);
		break;
	default:
		break;
	}

	/*
	 * If there is a fan on this device and it has failed log the
	 * failure.
	 */
	if (adapter->flags & IXGBE_FLAG_FAN_FAIL_CAPABLE) {
		u32 esdp = IXGBE_READ_REG(hw, IXGBE_ESDP);
		if (esdp & IXGBE_ESDP_SDP1)
			e_crit(probe, "Fan has stopped, replace the adapter\n");
	}

	if (allow_unsupported_sfp)
		hw->allow_unsupported_sfp = allow_unsupported_sfp;

	/* reset_hw fills in the perm_addr as well */
	hw->phy.reset_if_overtemp = true;
	err = hw->mac.ops.reset_hw(hw);
	hw->phy.reset_if_overtemp = false;
	ixgbe_set_eee_capable(adapter);
	if (err == -ENOENT) {
		err = 0;
	} else if (err == -EOPNOTSUPP) {
		e_dev_err("failed to load because an unsupported SFP+ or QSFP module type was detected.\n");
		e_dev_err("Reload the driver after installing a supported module.\n");
		goto err_sw_init;
	} else if (err) {
		e_dev_err("HW Init failed: %d\n", err);
		goto err_sw_init;
	}

#ifdef CONFIG_PCI_IOV
	/* SR-IOV not supported on the 82598 */
	if (adapter->hw.mac.type == ixgbe_mac_82598EB)
		goto skip_sriov;
	/* Mailbox */
	ixgbe_init_mbx_params_pf(hw);
	hw->mbx.ops = ii->mbx_ops;
	pci_sriov_set_totalvfs(pdev, IXGBE_MAX_VFS_DRV_LIMIT);
	ixgbe_enable_sriov(adapter, max_vfs);
skip_sriov:

#endif
	netdev->features = NETIF_F_SG |
			   NETIF_F_TSO |
			   NETIF_F_TSO6 |
			   NETIF_F_RXHASH |
			   NETIF_F_RXCSUM |
			   NETIF_F_HW_CSUM;

#define IXGBE_GSO_PARTIAL_FEATURES (NETIF_F_GSO_GRE | \
				    NETIF_F_GSO_GRE_CSUM | \
				    NETIF_F_GSO_IPXIP4 | \
				    NETIF_F_GSO_IPXIP6 | \
				    NETIF_F_GSO_UDP_TUNNEL | \
				    NETIF_F_GSO_UDP_TUNNEL_CSUM)

	netdev->gso_partial_features = IXGBE_GSO_PARTIAL_FEATURES;
	netdev->features |= NETIF_F_GSO_PARTIAL |
			    IXGBE_GSO_PARTIAL_FEATURES;

	if (hw->mac.type >= ixgbe_mac_82599EB)
		netdev->features |= NETIF_F_SCTP_CRC | NETIF_F_GSO_UDP_L4;

#ifdef CONFIG_IXGBE_IPSEC
#define IXGBE_ESP_FEATURES	(NETIF_F_HW_ESP | \
				 NETIF_F_HW_ESP_TX_CSUM | \
				 NETIF_F_GSO_ESP)

	if (adapter->ipsec)
		netdev->features |= IXGBE_ESP_FEATURES;
#endif
	/* copy netdev features into list of user selectable features */
	netdev->hw_features |= netdev->features |
			       NETIF_F_HW_VLAN_CTAG_FILTER |
			       NETIF_F_HW_VLAN_CTAG_RX |
			       NETIF_F_HW_VLAN_CTAG_TX |
			       NETIF_F_RXALL |
			       NETIF_F_HW_L2FW_DOFFLOAD;

	if (hw->mac.type >= ixgbe_mac_82599EB)
		netdev->hw_features |= NETIF_F_NTUPLE |
				       NETIF_F_HW_TC;

	netdev->features |= NETIF_F_HIGHDMA;

	netdev->vlan_features |= netdev->features | NETIF_F_TSO_MANGLEID;
	netdev->hw_enc_features |= netdev->vlan_features;
	netdev->mpls_features |= NETIF_F_SG |
				 NETIF_F_TSO |
				 NETIF_F_TSO6 |
				 NETIF_F_HW_CSUM;
	netdev->mpls_features |= IXGBE_GSO_PARTIAL_FEATURES;

	/* set this bit last since it cannot be part of vlan_features */
	netdev->features |= NETIF_F_HW_VLAN_CTAG_FILTER |
			    NETIF_F_HW_VLAN_CTAG_RX |
			    NETIF_F_HW_VLAN_CTAG_TX;

	netdev->priv_flags |= IFF_UNICAST_FLT;
	netdev->priv_flags |= IFF_SUPP_NOFCS;

	netdev->xdp_features = NETDEV_XDP_ACT_BASIC | NETDEV_XDP_ACT_REDIRECT |
			       NETDEV_XDP_ACT_XSK_ZEROCOPY;

	/* MTU range: 68 - 9710 */
	netdev->min_mtu = ETH_MIN_MTU;
	netdev->max_mtu = IXGBE_MAX_JUMBO_FRAME_SIZE - (ETH_HLEN + ETH_FCS_LEN);

#ifdef CONFIG_IXGBE_DCB
	if (adapter->flags & IXGBE_FLAG_DCB_CAPABLE)
		netdev->dcbnl_ops = &ixgbe_dcbnl_ops;
#endif

#ifdef IXGBE_FCOE
	if (adapter->flags & IXGBE_FLAG_FCOE_CAPABLE) {
		unsigned int fcoe_l;

		if (hw->mac.ops.get_device_caps) {
			hw->mac.ops.get_device_caps(hw, &device_caps);
			if (device_caps & IXGBE_DEVICE_CAPS_FCOE_OFFLOADS)
				adapter->flags &= ~IXGBE_FLAG_FCOE_CAPABLE;
		}


		fcoe_l = min_t(int, IXGBE_FCRETA_SIZE, num_online_cpus());
		adapter->ring_feature[RING_F_FCOE].limit = fcoe_l;

		netdev->features |= NETIF_F_FSO |
				    NETIF_F_FCOE_CRC;

		netdev->vlan_features |= NETIF_F_FSO |
					 NETIF_F_FCOE_CRC;
	}
#endif /* IXGBE_FCOE */
	if (adapter->flags2 & IXGBE_FLAG2_RSC_CAPABLE)
		netdev->hw_features |= NETIF_F_LRO;
	if (adapter->flags2 & IXGBE_FLAG2_RSC_ENABLED)
		netdev->features |= NETIF_F_LRO;

	if (ixgbe_check_fw_error(adapter)) {
		err = -EIO;
		goto err_sw_init;
	}

	/* make sure the EEPROM is good */
	if (hw->eeprom.ops.validate_checksum(hw, NULL) < 0) {
		e_dev_err("The EEPROM Checksum Is Not Valid\n");
		err = -EIO;
		goto err_sw_init;
	}

	eth_platform_get_mac_address(&adapter->pdev->dev,
				     adapter->hw.mac.perm_addr);

	eth_hw_addr_set(netdev, hw->mac.perm_addr);

	if (!is_valid_ether_addr(netdev->dev_addr)) {
		e_dev_err("invalid MAC address\n");
		err = -EIO;
		goto err_sw_init;
	}

	/* Set hw->mac.addr to permanent MAC address */
	ether_addr_copy(hw->mac.addr, hw->mac.perm_addr);
	ixgbe_mac_set_default_filter(adapter);

	if (hw->mac.type == ixgbe_mac_e610)
		mutex_init(&hw->aci.lock);
	timer_setup(&adapter->service_timer, ixgbe_service_timer, 0);

	if (ixgbe_removed(hw->hw_addr)) {
		err = -EIO;
		goto err_sw_init;
	}
	INIT_WORK(&adapter->service_task, ixgbe_service_task);
	set_bit(__IXGBE_SERVICE_INITED, &adapter->state);
	clear_bit(__IXGBE_SERVICE_SCHED, &adapter->state);

	err = ixgbe_init_interrupt_scheme(adapter);
	if (err)
		goto err_sw_init;

	for (i = 0; i < adapter->num_rx_queues; i++)
		u64_stats_init(&adapter->rx_ring[i]->syncp);
	for (i = 0; i < adapter->num_tx_queues; i++)
		u64_stats_init(&adapter->tx_ring[i]->syncp);
	for (i = 0; i < adapter->num_xdp_queues; i++)
		u64_stats_init(&adapter->xdp_ring[i]->syncp);

	/* WOL not supported for all devices */
	adapter->wol = 0;
	hw->eeprom.ops.read(hw, 0x2c, &adapter->eeprom_cap);
	hw->wol_enabled = ixgbe_wol_supported(adapter, pdev->device,
						pdev->subsystem_device);
	if (hw->wol_enabled)
		adapter->wol = IXGBE_WUFC_MAG;

	device_set_wakeup_enable(&adapter->pdev->dev, adapter->wol);

	/* save off EEPROM version number */
	ixgbe_set_fw_version(adapter);

	/* pick up the PCI bus settings for reporting later */
	if (ixgbe_pcie_from_parent(hw))
		ixgbe_get_parent_bus_info(adapter);
	else
		 hw->mac.ops.get_bus_info(hw);

	/* calculate the expected PCIe bandwidth required for optimal
	 * performance. Note that some older parts will never have enough
	 * bandwidth due to being older generation PCIe parts. We clamp these
	 * parts to ensure no warning is displayed if it can't be fixed.
	 */
	switch (hw->mac.type) {
	case ixgbe_mac_82598EB:
		expected_gts = min(ixgbe_enumerate_functions(adapter) * 10, 16);
		break;
	default:
		expected_gts = ixgbe_enumerate_functions(adapter) * 10;
		break;
	}

	/* don't check link if we failed to enumerate functions */
	if (expected_gts > 0)
		ixgbe_check_minimum_link(adapter, expected_gts);

	err = ixgbe_read_pba_string_generic(hw, part_str, sizeof(part_str));
	if (err)
		strscpy(part_str, "Unknown", sizeof(part_str));
	if (ixgbe_is_sfp(hw) && hw->phy.sfp_type != ixgbe_sfp_type_not_present)
		e_dev_info("MAC: %d, PHY: %d, SFP+: %d, PBA No: %s\n",
			   hw->mac.type, hw->phy.type, hw->phy.sfp_type,
			   part_str);
	else
		e_dev_info("MAC: %d, PHY: %d, PBA No: %s\n",
			   hw->mac.type, hw->phy.type, part_str);

	e_dev_info("%pM\n", netdev->dev_addr);

	/* reset the hardware with the new settings */
	err = hw->mac.ops.start_hw(hw);
	if (err == -EACCES) {
		/* We are running on a pre-production device, log a warning */
		e_dev_warn("This device is a pre-production adapter/LOM. "
			   "Please be aware there may be issues associated "
			   "with your hardware.  If you are experiencing "
			   "problems please contact your Intel or hardware "
			   "representative who provided you with this "
			   "hardware.\n");
	}
	strcpy(netdev->name, "eth%d");
	pci_set_drvdata(pdev, adapter);
	err = register_netdev(netdev);
	if (err)
		goto err_register;


	/* power down the optics for 82599 SFP+ fiber */
	if (hw->mac.ops.disable_tx_laser)
		hw->mac.ops.disable_tx_laser(hw);

	/* carrier off reporting is important to ethtool even BEFORE open */
	netif_carrier_off(netdev);

#ifdef CONFIG_IXGBE_DCA
	if (dca_add_requester(&pdev->dev) == 0) {
		adapter->flags |= IXGBE_FLAG_DCA_ENABLED;
		ixgbe_setup_dca(adapter);
	}
#endif
	if (adapter->flags & IXGBE_FLAG_SRIOV_ENABLED) {
		e_info(probe, "IOV is enabled with %d VFs\n", adapter->num_vfs);
		for (i = 0; i < adapter->num_vfs; i++)
			ixgbe_vf_configuration(pdev, (i | 0x10000000));
	}

	/* firmware requires driver version to be 0xFFFFFFFF
	 * since os does not support feature
	 */
	if (hw->mac.ops.set_fw_drv_ver)
		hw->mac.ops.set_fw_drv_ver(hw, 0xFF, 0xFF, 0xFF, 0xFF,
					   sizeof(UTS_RELEASE) - 1,
					   UTS_RELEASE);

	/* add san mac addr to netdev */
	ixgbe_add_sanmac_netdev(netdev);

	e_dev_info("%s\n", ixgbe_default_device_descr);

#ifdef CONFIG_IXGBE_HWMON
	if (ixgbe_sysfs_init(adapter))
		e_err(probe, "failed to allocate sysfs resources\n");
#endif /* CONFIG_IXGBE_HWMON */

	ixgbe_dbg_adapter_init(adapter);

	/* setup link for SFP devices with MNG FW, else wait for IXGBE_UP */
	if (ixgbe_mng_enabled(hw) && ixgbe_is_sfp(hw) && hw->mac.ops.setup_link)
		hw->mac.ops.setup_link(hw,
			IXGBE_LINK_SPEED_10GB_FULL | IXGBE_LINK_SPEED_1GB_FULL,
			true);

	err = ixgbe_mii_bus_init(hw);
	if (err)
		goto err_netdev;

	return 0;

ixgbe_setup_gaming_atr(adapter);

err_netdev:
	unregister_netdev(netdev);
err_register:
	ixgbe_release_hw_control(adapter);
	ixgbe_clear_interrupt_scheme(adapter);
	if (hw->mac.type == ixgbe_mac_e610)
		mutex_destroy(&adapter->hw.aci.lock);
err_sw_init:
	ixgbe_disable_sriov(adapter);
	adapter->flags2 &= ~IXGBE_FLAG2_SEARCH_FOR_SFP;
	iounmap(adapter->io_addr);
	kfree(adapter->jump_tables[0]);
	kfree(adapter->mac_table);
	kfree(adapter->rss_key);
	bitmap_free(adapter->af_xdp_zc_qps);
err_ioremap:
	disable_dev = !test_and_set_bit(__IXGBE_DISABLED, &adapter->state);
	free_netdev(netdev);
err_alloc_etherdev:
	pci_release_mem_regions(pdev);
err_pci_reg:
err_dma:
	if (!adapter || disable_dev)
		pci_disable_device(pdev);
	return err;
}

/**
 * ixgbe_remove - Device Removal Routine
 * @pdev: PCI device information struct
 *
 * ixgbe_remove is called by the PCI subsystem to alert the driver
 * that it should release a PCI device.  The could be caused by a
 * Hot-Plug event, or because the driver is going to be removed from
 * memory.
 **/
static void ixgbe_remove(struct pci_dev *pdev)
{
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	struct net_device *netdev;
	bool disable_dev;
	int i;

	/* if !adapter then we already cleaned up in probe */
	if (!adapter)
		return;

	netdev  = adapter->netdev;
	ixgbe_dbg_adapter_exit(adapter);

	set_bit(__IXGBE_REMOVING, &adapter->state);
	cancel_work_sync(&adapter->service_task);

	if (adapter->hw.mac.type == ixgbe_mac_e610) {
		ixgbe_disable_link_status_events(adapter);
		mutex_destroy(&adapter->hw.aci.lock);
	}

	if (adapter->mii_bus)
		mdiobus_unregister(adapter->mii_bus);

#ifdef CONFIG_IXGBE_DCA
	if (adapter->flags & IXGBE_FLAG_DCA_ENABLED) {
		adapter->flags &= ~IXGBE_FLAG_DCA_ENABLED;
		dca_remove_requester(&pdev->dev);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_DCA_CTRL,
				IXGBE_DCA_CTRL_DCA_DISABLE);
	}

#endif
#ifdef CONFIG_IXGBE_HWMON
	ixgbe_sysfs_exit(adapter);
#endif /* CONFIG_IXGBE_HWMON */

	/* remove the added san mac */
	ixgbe_del_sanmac_netdev(netdev);

#ifdef CONFIG_PCI_IOV
	ixgbe_disable_sriov(adapter);
#endif
	if (netdev->reg_state == NETREG_REGISTERED)
		unregister_netdev(netdev);

	ixgbe_stop_ipsec_offload(adapter);
	ixgbe_clear_interrupt_scheme(adapter);

	ixgbe_release_hw_control(adapter);

#ifdef CONFIG_DCB
	kfree(adapter->ixgbe_ieee_pfc);
	kfree(adapter->ixgbe_ieee_ets);

#endif
	iounmap(adapter->io_addr);
	pci_release_mem_regions(pdev);

	e_dev_info("complete\n");

	for (i = 0; i < IXGBE_MAX_LINK_HANDLE; i++) {
		if (adapter->jump_tables[i]) {
			kfree(adapter->jump_tables[i]->input);
			kfree(adapter->jump_tables[i]->mask);
		}
		kfree(adapter->jump_tables[i]);
	}

	kfree(adapter->mac_table);
	kfree(adapter->rss_key);
	bitmap_free(adapter->af_xdp_zc_qps);
	disable_dev = !test_and_set_bit(__IXGBE_DISABLED, &adapter->state);
	free_netdev(netdev);

	if (disable_dev)
		pci_disable_device(pdev);
}

/**
 * ixgbe_io_error_detected - called when PCI error is detected
 * @pdev: Pointer to PCI device
 * @state: The current pci connection state
 *
 * This function is called after a PCI bus error affecting
 * this device has been detected.
 */
static pci_ers_result_t ixgbe_io_error_detected(struct pci_dev *pdev,
						pci_channel_state_t state)
{
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	struct net_device *netdev = adapter->netdev;

#ifdef CONFIG_PCI_IOV
	struct ixgbe_hw *hw = &adapter->hw;
	struct pci_dev *bdev, *vfdev;
	u32 dw0, dw1, dw2, dw3;
	int vf, pos;
	u16 req_id, pf_func;

	if (adapter->hw.mac.type == ixgbe_mac_82598EB ||
	    adapter->num_vfs == 0)
		goto skip_bad_vf_detection;

	bdev = pdev->bus->self;
	while (bdev && (pci_pcie_type(bdev) != PCI_EXP_TYPE_ROOT_PORT))
		bdev = bdev->bus->self;

	if (!bdev)
		goto skip_bad_vf_detection;

	pos = pci_find_ext_capability(bdev, PCI_EXT_CAP_ID_ERR);
	if (!pos)
		goto skip_bad_vf_detection;

	dw0 = ixgbe_read_pci_cfg_dword(hw, pos + PCI_ERR_HEADER_LOG);
	dw1 = ixgbe_read_pci_cfg_dword(hw, pos + PCI_ERR_HEADER_LOG + 4);
	dw2 = ixgbe_read_pci_cfg_dword(hw, pos + PCI_ERR_HEADER_LOG + 8);
	dw3 = ixgbe_read_pci_cfg_dword(hw, pos + PCI_ERR_HEADER_LOG + 12);
	if (ixgbe_removed(hw->hw_addr))
		goto skip_bad_vf_detection;

	req_id = dw1 >> 16;
	/* On the 82599 if bit 7 of the requestor ID is set then it's a VF */
	if (!(req_id & 0x0080))
		goto skip_bad_vf_detection;

	pf_func = req_id & 0x01;
	if ((pf_func & 1) == (pdev->devfn & 1)) {
		unsigned int device_id;

		vf = FIELD_GET(0x7F, req_id);
		e_dev_err("VF %d has caused a PCIe error\n", vf);
		e_dev_err("TLP: dw0: %8.8x\tdw1: %8.8x\tdw2: "
				"%8.8x\tdw3: %8.8x\n",
		dw0, dw1, dw2, dw3);
		switch (adapter->hw.mac.type) {
		case ixgbe_mac_82599EB:
			device_id = IXGBE_82599_VF_DEVICE_ID;
			break;
		case ixgbe_mac_X540:
			device_id = IXGBE_X540_VF_DEVICE_ID;
			break;
		case ixgbe_mac_X550:
			device_id = IXGBE_DEV_ID_X550_VF;
			break;
		case ixgbe_mac_X550EM_x:
			device_id = IXGBE_DEV_ID_X550EM_X_VF;
			break;
		case ixgbe_mac_x550em_a:
			device_id = IXGBE_DEV_ID_X550EM_A_VF;
			break;
		case ixgbe_mac_e610:
			device_id = IXGBE_DEV_ID_E610_VF;
			break;
		default:
			device_id = 0;
			break;
		}

		/* Find the pci device of the offending VF */
		vfdev = pci_get_device(PCI_VENDOR_ID_INTEL, device_id, NULL);
		while (vfdev) {
			if (vfdev->devfn == (req_id & 0xFF))
				break;
			vfdev = pci_get_device(PCI_VENDOR_ID_INTEL,
					       device_id, vfdev);
		}
		/*
		 * There's a slim chance the VF could have been hot plugged,
		 * so if it is no longer present we don't need to issue the
		 * VFLR.  Just clean up the AER in that case.
		 */
		if (vfdev) {
			pcie_flr(vfdev);
			/* Free device reference count */
			pci_dev_put(vfdev);
		}
	}

	/*
	 * Even though the error may have occurred on the other port
	 * we still need to increment the vf error reference count for
	 * both ports because the I/O resume function will be called
	 * for both of them.
	 */
	adapter->vferr_refcount++;

	return PCI_ERS_RESULT_RECOVERED;

skip_bad_vf_detection:
#endif /* CONFIG_PCI_IOV */
	if (!test_bit(__IXGBE_SERVICE_INITED, &adapter->state))
		return PCI_ERS_RESULT_DISCONNECT;

	if (!netif_device_present(netdev))
		return PCI_ERS_RESULT_DISCONNECT;

	rtnl_lock();
	netif_device_detach(netdev);

	if (netif_running(netdev))
		ixgbe_close_suspend(adapter);

	if (state == pci_channel_io_perm_failure) {
		rtnl_unlock();
		return PCI_ERS_RESULT_DISCONNECT;
	}

	if (!test_and_set_bit(__IXGBE_DISABLED, &adapter->state))
		pci_disable_device(pdev);
	rtnl_unlock();

	/* Request a slot reset. */
	return PCI_ERS_RESULT_NEED_RESET;
}

/**
 * ixgbe_io_slot_reset - called after the pci bus has been reset.
 * @pdev: Pointer to PCI device
 *
 * Restart the card from scratch, as if from a cold-boot.
 */
static pci_ers_result_t ixgbe_io_slot_reset(struct pci_dev *pdev)
{
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	pci_ers_result_t result;

	if (pci_enable_device_mem(pdev)) {
		e_err(probe, "Cannot re-enable PCI device after reset.\n");
		result = PCI_ERS_RESULT_DISCONNECT;
	} else {
		smp_mb__before_atomic();
		clear_bit(__IXGBE_DISABLED, &adapter->state);
		adapter->hw.hw_addr = adapter->io_addr;
		pci_set_master(pdev);
		pci_restore_state(pdev);
		pci_save_state(pdev);

		pci_wake_from_d3(pdev, false);

		ixgbe_reset(adapter);
		IXGBE_WRITE_REG(&adapter->hw, IXGBE_WUS, ~0);
		result = PCI_ERS_RESULT_RECOVERED;
	}

	return result;
}

/**
 * ixgbe_io_resume - called when traffic can start flowing again.
 * @pdev: Pointer to PCI device
 *
 * This callback is called when the error recovery driver tells us that
 * its OK to resume normal operation.
 */
static void ixgbe_io_resume(struct pci_dev *pdev)
{
	struct ixgbe_adapter *adapter = pci_get_drvdata(pdev);
	struct net_device *netdev = adapter->netdev;

#ifdef CONFIG_PCI_IOV
	if (adapter->vferr_refcount) {
		e_info(drv, "Resuming after VF err\n");
		adapter->vferr_refcount--;
		return;
	}

#endif
	rtnl_lock();
	if (netif_running(netdev))
		ixgbe_open(netdev);

	netif_device_attach(netdev);
	rtnl_unlock();
}

static const struct pci_error_handlers ixgbe_err_handler = {
	.error_detected = ixgbe_io_error_detected,
	.slot_reset = ixgbe_io_slot_reset,
	.resume = ixgbe_io_resume,
};

static DEFINE_SIMPLE_DEV_PM_OPS(ixgbe_pm_ops, ixgbe_suspend, ixgbe_resume);

static struct pci_driver ixgbe_driver = {
	.name      = ixgbe_driver_name,
	.id_table  = ixgbe_pci_tbl,
	.probe     = ixgbe_probe,
	.remove    = ixgbe_remove,
	.driver.pm = pm_sleep_ptr(&ixgbe_pm_ops),
	.shutdown  = ixgbe_shutdown,
	.sriov_configure = ixgbe_pci_sriov_configure,
	.err_handler = &ixgbe_err_handler
};

/**
 * ixgbe_init_module - Driver Registration Routine
 *
 * ixgbe_init_module is the first routine called when the driver is
 * loaded. All it does is register with the PCI subsystem.
 **/
static int __init ixgbe_init_module(void)
{
	int ret;
	pr_info("%s\n", ixgbe_driver_string);
	pr_info("%s\n", ixgbe_copyright);

	ixgbe_wq = create_singlethread_workqueue(ixgbe_driver_name);
	if (!ixgbe_wq) {
		pr_err("%s: Failed to create workqueue\n", ixgbe_driver_name);
		return -ENOMEM;
	}

	ixgbe_dbg_init();

	ret = pci_register_driver(&ixgbe_driver);
	if (ret) {
		destroy_workqueue(ixgbe_wq);
		ixgbe_dbg_exit();
		return ret;
	}

#ifdef CONFIG_IXGBE_DCA
	dca_register_notify(&dca_notifier);
#endif

	return 0;
}

module_init(ixgbe_init_module);

/**
 * ixgbe_exit_module - Driver Exit Cleanup Routine
 *
 * ixgbe_exit_module is called just before the driver is removed
 * from memory.
 **/
static void __exit ixgbe_exit_module(void)
{
#ifdef CONFIG_IXGBE_DCA
	dca_unregister_notify(&dca_notifier);
#endif
	pci_unregister_driver(&ixgbe_driver);

	ixgbe_dbg_exit();
	if (ixgbe_wq) {
		destroy_workqueue(ixgbe_wq);
		ixgbe_wq = NULL;
	}
}

#ifdef CONFIG_IXGBE_DCA
static int ixgbe_notify_dca(struct notifier_block *nb, unsigned long event,
			    void *p)
{
	int ret_val;

	ret_val = driver_for_each_device(&ixgbe_driver.driver, NULL, &event,
					 __ixgbe_notify_dca);

	return ret_val ? NOTIFY_BAD : NOTIFY_DONE;
}

#endif /* CONFIG_IXGBE_DCA */

module_exit(ixgbe_exit_module);

/* ixgbe_main.c */
