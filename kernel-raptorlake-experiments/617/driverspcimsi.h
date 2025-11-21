// SPDX-License-Identifier: GPL-2.0
/*
 * Optimized MSI Header for Low-Latency Gaming
 */

#include <linux/pci.h>
#include <linux/msi.h>

#define msix_table_size(flags)	((flags & PCI_MSIX_FLAGS_QSIZE) + 1)

int pci_msi_setup_msi_irqs(struct pci_dev *dev, int nvec, int type);
void pci_msi_teardown_msi_irqs(struct pci_dev *dev);

/* Mask/unmask helpers */
void pci_msi_update_mask(struct msi_desc *desc, u32 clear, u32 set);

static inline void pci_msi_mask(struct msi_desc *desc, u32 mask)
{
	pci_msi_update_mask(desc, 0, mask);
}

static inline void pci_msi_unmask(struct msi_desc *desc, u32 mask)
{
	pci_msi_update_mask(desc, mask, 0);
}

static inline void __iomem *pci_msix_desc_addr(struct msi_desc *desc)
{
	return desc->pci.mask_base + desc->msi_index * PCI_MSIX_ENTRY_SIZE;
}

/*
 * Internal function. Does NOT flush writes to the device.
 * Updates cached control value.
 */
static inline void pci_msix_write_vector_ctrl(struct msi_desc *desc, u32 ctrl)
{
	void __iomem *desc_addr = pci_msix_desc_addr(desc);

	if (likely(desc->pci.msi_attrib.can_mask)) {
		writel(ctrl, desc_addr + PCI_MSIX_ENTRY_VECTOR_CTRL);
		desc->pci.msix_ctrl = ctrl;
	}
}

static inline void pci_msix_mask(struct msi_desc *desc)
{
	pci_msix_write_vector_ctrl(desc, desc->pci.msix_ctrl | PCI_MSIX_ENTRY_CTRL_MASKBIT);
	/*
	 * Flush write to device.
	 * Masking must be synchronous to ensure no more IRQs fire
	 * before we proceed to teardown or rebalancing.
	 */
	readl(desc->pci.mask_base);
}

static inline void pci_msix_unmask(struct msi_desc *desc)
{
	/*
	 * OPTIMIZATION: Removed the readl() flush.
	 * On x86 (Raptor Lake), stores are ordered. We do not need to stall
	 * the CPU waiting for the unmask to round-trip to the PCIe device.
	 * The interrupt will arrive when it arrives.
	 */
	pci_msix_write_vector_ctrl(desc, desc->pci.msix_ctrl & ~PCI_MSIX_ENTRY_CTRL_MASKBIT);
}

static inline void __pci_msi_mask_desc(struct msi_desc *desc, u32 mask)
{
	if (desc->pci.msi_attrib.is_msix)
		pci_msix_mask(desc);
	else
		pci_msi_mask(desc, mask);
}

static inline void __pci_msi_unmask_desc(struct msi_desc *desc, u32 mask)
{
	if (desc->pci.msi_attrib.is_msix)
		pci_msix_unmask(desc);
	else
		pci_msi_unmask(desc, mask);
}

static inline __attribute_const__ u32 msi_multi_mask(struct msi_desc *desc)
{
	if (desc->pci.msi_attrib.multi_cap >= 5)
		return 0xffffffff;
	return (1 << (1 << desc->pci.msi_attrib.multi_cap)) - 1;
}

void msix_prepare_msi_desc(struct pci_dev *dev, struct msi_desc *desc);

extern bool pci_msi_enable;

void pci_msi_shutdown(struct pci_dev *dev);
void pci_msix_shutdown(struct pci_dev *dev);
void pci_free_msi_irqs(struct pci_dev *dev);
int __pci_enable_msi_range(struct pci_dev *dev, int minvec, int maxvec, struct irq_affinity *affd);
int __pci_enable_msix_range(struct pci_dev *dev, struct msix_entry *entries, int minvec,
			    int maxvec,  struct irq_affinity *affd, int flags);
void __pci_restore_msi_state(struct pci_dev *dev);
void __pci_restore_msix_state(struct pci_dev *dev);

enum support_mode {
	ALLOW_LEGACY,
	DENY_LEGACY,
};

bool pci_msi_domain_supports(struct pci_dev *dev, unsigned int feature_mask, enum support_mode mode);
bool pci_setup_msi_device_domain(struct pci_dev *pdev, unsigned int hwsize);
bool pci_setup_msix_device_domain(struct pci_dev *pdev, unsigned int hwsize);

#ifdef CONFIG_PCI_MSI_ARCH_FALLBACKS
int pci_msi_legacy_setup_msi_irqs(struct pci_dev *dev, int nvec, int type);
void pci_msi_legacy_teardown_msi_irqs(struct pci_dev *dev);
#else
static inline int pci_msi_legacy_setup_msi_irqs(struct pci_dev *dev, int nvec, int type)
{
	WARN_ON_ONCE(1);
	return -ENODEV;
}

static inline void pci_msi_legacy_teardown_msi_irqs(struct pci_dev *dev)
{
	WARN_ON_ONCE(1);
}
#endif
