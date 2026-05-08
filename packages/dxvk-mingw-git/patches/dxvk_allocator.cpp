#include <algorithm>
#include <limits>
#include <utility>

#include "dxvk_allocator.h"

#include "../util/util_bit.h"
#include "../util/util_likely.h"

namespace dxvk {

  namespace {

    static uint32_t normalizePageAlignment(uint32_t alignment) {
      if (alignment <= 1u)
        return 1u;

      // Round to next power-of-two since allocator math relies on bit masks.
      alignment -= 1u;
      alignment |= alignment >> 1u;
      alignment |= alignment >> 2u;
      alignment |= alignment >> 4u;
      alignment |= alignment >> 8u;
      alignment |= alignment >> 16u;
      return alignment + 1u;
    }

  }


  DxvkPageAllocator::DxvkPageAllocator() {

  }


  DxvkPageAllocator::~DxvkPageAllocator() {

  }


  int64_t DxvkPageAllocator::alloc(uint64_t size, uint64_t alignment) {
    if (unlikely(!size))
      return -1;

    alignment = std::max<uint64_t>(alignment, 1u);

    uint64_t pageCount64 = (size + PageSize - 1u) / PageSize;
    uint64_t pageAlign64 = (alignment + PageSize - 1u) / PageSize;

    if (unlikely(!pageCount64
             || pageCount64 > uint64_t(ChunkPageMask + 1u)
             || pageCount64 > uint64_t(std::numeric_limits<uint32_t>::max())
             || pageAlign64 > uint64_t(std::numeric_limits<uint32_t>::max())))
      return -1;

    uint32_t pageCount = uint32_t(pageCount64);
    uint32_t pageAlign = normalizePageAlignment(uint32_t(std::max<uint64_t>(pageAlign64, 1u)));

    return std::max<int64_t>(-1, int64_t(allocPages(pageCount, pageAlign)) * int64_t(PageSize));
  }


  int32_t DxvkPageAllocator::allocPages(uint32_t count, uint32_t alignment) {
    if (unlikely(!count || count > ChunkPageMask + 1u))
      return -1;

    alignment = normalizePageAlignment(alignment);

    int32_t index = searchFreeList(count);

    while (index--) {
      PageRange entry = m_freeList[index];

      // The chunk index is the same regardless of alignment.
      // Skip chunk if it does not accept new allocations.
      uint32_t chunkIndex = entry.index >> ChunkPageBits;

      if (unlikely(m_chunks[chunkIndex].disabled))
        continue;

      if (likely(!(entry.index & (alignment - 1u)))) {
        // If the current free range is sufficiently aligned, we can use
        // it as-is and simply modify the remaining free list entry.
        uint32_t pageIndex = entry.index;

        entry.index += count;
        entry.count -= count;

        insertFreeRange(entry, index);

        m_chunks[chunkIndex].pagesUsed += count;
        return int32_t(pageIndex);
      } else {
        // Apply alignment and skip if the free range is too small.
        uint32_t pageIndex = align(entry.index, alignment);

        if (pageIndex + count > entry.index + entry.count)
          continue;

        // Insert free range before the first allocated page,
        // guaranteed to be non-empty at this point.
        PageRange prevRange = { };
        prevRange.index = entry.index;
        prevRange.count = pageIndex - entry.index;

        insertFreeRange(prevRange, index);

        // Insert free range after the last allocated page.
        PageRange nextRange = { };
        nextRange.index = pageIndex + count;
        nextRange.count = entry.index + entry.count - nextRange.index;

        if (nextRange.count)
          insertFreeRange(nextRange, -1);

        m_chunks[chunkIndex].pagesUsed += count;

        return int32_t(pageIndex);
      }
    }

    return -1;
  }


  bool DxvkPageAllocator::free(uint64_t address, uint64_t size) {
    if (unlikely(!size))
      return false;

    uint64_t pageIndex64 = address / PageSize;
    uint64_t pageCount64 = (size + PageSize - 1u) / PageSize;

    if (unlikely(pageIndex64 > uint64_t(std::numeric_limits<uint32_t>::max())
             || !pageCount64
             || pageCount64 > uint64_t(std::numeric_limits<uint32_t>::max())))
      return false;

    return freePages(uint32_t(pageIndex64), uint32_t(pageCount64));
  }


  bool DxvkPageAllocator::freePages(uint32_t index, uint32_t count) {
    if (unlikely(!count || index >= m_freeListLutByPage.size()))
      return false;

    uint32_t chunkIndex = index >> ChunkPageBits;

    if (unlikely(chunkIndex >= m_chunks.size()))
      return false;

    uint32_t chunkOffset = index & ChunkPageMask;
    uint32_t endIndex = index + count;

    if (unlikely(endIndex < index))
      return false;

    const auto& chunk = m_chunks[chunkIndex];

    if (unlikely(uint64_t(chunkOffset) + uint64_t(count) > uint64_t(chunk.pageCount) || count > chunk.pagesUsed))
      return false;

    // Use the lookup table to quickly determine which
    // free ranges we can actually merge with.
    int32_t prevRange = -1;
    int32_t nextRange = -1;

    if (index & ChunkPageMask)
      prevRange = m_freeListLutByPage[index - 1u];

    if ((endIndex & ChunkPageMask) && endIndex < m_freeListLutByPage.size())
      nextRange = m_freeListLutByPage[endIndex];

    if (prevRange < 0) {
      if (nextRange < 0) {
        // No adjacent range, need to insert a new one.
        PageRange range = { };
        range.index = index;
        range.count = count;

        insertFreeRange(range, -1);
      } else {
        // One adjacent range after the current one.
        PageRange range = m_freeList[nextRange];
        range.index = index;
        range.count += count;

        insertFreeRange(range, nextRange);
      }
    } else if (nextRange < 0) {
      // One adjacent range before the current one.
      PageRange range = m_freeList[prevRange];
      range.count += count;

      insertFreeRange(range, prevRange);
    } else {
      // Two adjacent ranges, need to merge with both
      // and replace one while removing the other.
      PageRange prev = m_freeList[prevRange];
      PageRange next = m_freeList[nextRange];

      PageRange mergedRange = { };
      mergedRange.index = prev.index;
      mergedRange.count = next.index + next.count - prev.index;

      PageRange emptyRange = { };

      // Remove the range at the higher index, then replace the one at the
      // lower index with the merged range. The order is important here since
      // having overlapping entries in the free list would cause issues for
      // the look-up table, and using the correct indices is important since
      // the index for the second operation could otherwise be invalidated.
      insertFreeRange(emptyRange, std::max(prevRange, nextRange));
      insertFreeRange(mergedRange, std::min(prevRange, nextRange));
    }

    return !(m_chunks[chunkIndex].pagesUsed -= count);
  }


  uint32_t DxvkPageAllocator::addChunk(uint64_t size) {
    int32_t chunkIndex = m_freeChunk;

    if (chunkIndex < 0) {
      chunkIndex = int32_t(m_chunks.size());

      m_freeListLutByPage.resize((uint32_t(chunkIndex) + 1u) << ChunkPageBits, -1);
      m_chunks.emplace_back();
    }

    auto& chunk = m_chunks[chunkIndex];
    m_freeChunk = chunk.nextChunk;

    // Keep chunk addressing consistent with ChunkPageBits.
    uint64_t chunkPageCount = std::min<uint64_t>(size / PageSize, uint64_t(ChunkPageMask + 1u));

    chunk.pageCount = uint32_t(chunkPageCount);
    chunk.pagesUsed = 0u;
    chunk.nextChunk = -1;
    chunk.disabled = false;

    if (chunk.pageCount) {
      PageRange pageRange = { };
      pageRange.index = uint32_t(chunkIndex) << ChunkPageBits;
      pageRange.count = chunk.pageCount;

      insertFreeRange(pageRange, -1);
    }

    return uint32_t(chunkIndex);
  }


  void DxvkPageAllocator::removeChunk(uint32_t chunkIndex) {
    auto& chunk = m_chunks[chunkIndex];
    chunk.pageCount = 0u;
    chunk.pagesUsed = 0u;
    chunk.nextChunk = std::exchange(m_freeChunk, int32_t(chunkIndex));
    chunk.disabled = true;

    uint32_t pageIndex = chunkIndex << ChunkPageBits;

    PageRange pageRange = { };
    pageRange.index = pageIndex;
    pageRange.count = 0u;

    insertFreeRange(pageRange, m_freeListLutByPage[pageIndex]);
  }


  void DxvkPageAllocator::killChunk(uint32_t chunkIndex) {
    m_chunks[chunkIndex].disabled = true;
  }


  void DxvkPageAllocator::reviveChunk(uint32_t chunkIndex) {
    m_chunks[chunkIndex].disabled = false;
  }


  uint32_t DxvkPageAllocator::reviveChunks() {
    uint32_t count = 0u;

    for (uint32_t i = 0u; i < m_chunks.size(); i++) {
      if (m_chunks[i].pageCount && m_chunks[i].disabled) {
        m_chunks[i].disabled = false;
        count += 1u;
      }
    }

    return count;
  }


  void DxvkPageAllocator::getPageAllocationMask(uint32_t chunkIndex, uint32_t* pageMask) const {
    // Initialize bit mask with all ones.
    const auto& chunk = m_chunks[chunkIndex];

    uint32_t fullCount = chunk.pageCount / 32u;
    uint32_t lastCount = chunk.pageCount % 32u;

    for (uint32_t i = 0u; i < fullCount; i++)
      pageMask[i] = ~0u;

    if (lastCount)
      pageMask[fullCount] = (1u << lastCount) - 1u;

    // Iterate over free list and set all pages included
    // in the current chunk to 0.
    for (PageRange range : m_freeList) {
      if ((range.index >> ChunkPageBits) != chunkIndex)
        continue;

      range.index &= ChunkPageMask;

      uint32_t index = range.index / 32u;
      uint32_t shift = range.index % 32u;

      if (shift + range.count < 32u) {
        // Entire free range fits in one single mask.
        pageMask[index] ^= ((1u << range.count) - 1u) << shift;
      } else {
        if (shift) {
          pageMask[index++] ^= ~0u << shift;
          range.count -= 32u - shift;
        }

        while (range.count >= 32u) {
          pageMask[index++] = 0u;
          range.count -= 32u;
        }

        if (range.count)
          pageMask[index] &= ~0u << range.count;
      }
    }
  }


  int32_t DxvkPageAllocator::searchFreeList(uint32_t count) {
    // Find the insertion index of a free list entry with the given page count.
    // All entries with an index lower than but not equal to the return value
    // will have a page count greater than or equal to count.
    if (unlikely(m_freeList.empty()))
      return 0;

    // Do a binary search, but optimize for the common
    // case where we request a small page count.
    uint32_t lo = 0u;
    uint32_t hi = uint32_t(m_freeList.size());

    if (count <= m_freeList.back().count)
      return int32_t(hi);

    while (lo < hi) {
      uint32_t mid = (lo + hi) / 2u;

      if (count <= m_freeList[mid].count)
        lo = mid + 1u;
      else
        hi = mid;
    }

    return int32_t(lo);
  }


  void DxvkPageAllocator::addLutEntry(const PageRange& range, int32_t index) {
    m_freeListLutByPage[range.index] = index;
    m_freeListLutByPage[range.index + range.count - 1u] = index;
  }


  void DxvkPageAllocator::removeLutEntry(const PageRange& range) {
    m_freeListLutByPage[range.index] = -1;
    m_freeListLutByPage[range.index + range.count - 1u] = -1;
  }


  void DxvkPageAllocator::insertFreeRange(PageRange newRange, int32_t currentIndex) {
    size_t count = m_freeList.size();
    size_t index = size_t(currentIndex);

    if (unlikely(currentIndex < 0)) {
      m_freeList.emplace_back();
      index = count++;
    }

    // Remove old range from the LUT since it gets replaced.
    PageRange oldRange = m_freeList[index];

    if (likely(oldRange.count))
      removeLutEntry(oldRange);

    // Move range within the free list until the proper ordering
    // is restored again and update LUT entries for all ranges we
    // move in the process.
    if (newRange.count < oldRange.count) {
      while (index + 1u < count) {
        PageRange next = m_freeList[index + 1u];

        if (newRange.count >= next.count)
          break;

        addLutEntry(next, int32_t(index));
        m_freeList[index++] = next;
      }
    } else if (newRange.count > oldRange.count) {
      while (index) {
        PageRange prev = m_freeList[index - 1u];

        if (newRange.count <= prev.count)
          break;

        addLutEntry(prev, int32_t(index));
        m_freeList[index--] = prev;
      }
    }

    if (newRange.count) {
      m_freeList[index] = newRange;
      addLutEntry(newRange, int32_t(index));
    } else {
      m_freeList.pop_back();
    }
  }



  DxvkPoolAllocator::DxvkPoolAllocator(DxvkPageAllocator& pageAllocator)
  : m_pageAllocator(&pageAllocator) {

  }


  DxvkPoolAllocator::~DxvkPoolAllocator() {

  }


  int64_t DxvkPoolAllocator::alloc(uint64_t size) {
    if (unlikely(size > MaxSize))
      return -1;

    uint32_t listIndex = computeListIndex(size);
    uint32_t poolCapacity = computePoolCapacity(listIndex);

    // Obtain a page for the size category.
    int32_t pageIndex = m_pageLists[listIndex].head;

    if (likely(pageIndex >= 0)) {
      uint32_t chunkIndex = uint32_t(pageIndex) >> DxvkPageAllocator::ChunkPageBits;

      // If the selected page is from a dead chunk, do not allocate
      // into it anymore so that the chunk can actually be freed.
      if (unlikely(!m_pageAllocator->chunkIsAvailable(chunkIndex))) {
        int32_t nextIndex = pageIndex;

        do {
          // This works because we add pages to the end.
          removePageFromList(uint32_t(nextIndex), listIndex);
          addPageToList(uint32_t(nextIndex), listIndex);

          nextIndex = m_pageLists[listIndex].head;

          if (nextIndex < 0)
            break;

          chunkIndex = uint32_t(nextIndex) >> DxvkPageAllocator::ChunkPageBits;
        } while (nextIndex != pageIndex && !m_pageAllocator->chunkIsAvailable(chunkIndex));

        // Allocate a new page if the entire list is dead.
        pageIndex = nextIndex != pageIndex ? nextIndex : -1;
      }
    }

    if (unlikely(pageIndex < 0)) {
      if ((pageIndex = allocPage(listIndex)) < 0)
        return -1;

      // Initialize suballocator for the page.
      PageInfo& page = m_pageInfos[uint32_t(pageIndex)];

      if (likely(poolCapacity <= MaskBits)) {
        // Initialize free mask with the first item marked as used.
        page.pool = (MaskType(2) << (poolCapacity - 1u)) - MaskType(2);
      } else {
        // This is also going to have its first item used already.
        page.pool = allocPagePool(poolCapacity);
      }

      return computeByteAddress(uint32_t(pageIndex), 0u, listIndex);
    }

    if (likely(poolCapacity <= MaskBits)) {
      // Fast path that uses the pool index as an allocator.
      // Frequent allocations should ideally hit this path.
      PageInfo& page = m_pageInfos[uint32_t(pageIndex)];

      uint32_t itemIndex = bit::tzcnt(page.pool);
      page.pool &= page.pool - MaskType(1);

      if (unlikely(!page.pool))
        removePageFromList(uint32_t(pageIndex), listIndex);

      return computeByteAddress(uint32_t(pageIndex), itemIndex, listIndex);
    } else {
      PageInfo& page = m_pageInfos[uint32_t(pageIndex)];
      PagePool& pool = m_pagePools[uint32_t(page.pool)];

      // Check top-level masks to find which low-level mask to use.
      uint32_t maskIndex = bit::tzcnt(uint32_t(pool.freeMask));
      MaskType maskBit = MaskType(1) << maskIndex;

      pool.usedMask |= uint16_t(maskBit);

      // Allocate item from the selected low-level mask.
      MaskType& mask = pool.subPools[maskIndex];
      uint32_t itemIndex = bit::tzcnt(mask) + maskIndex * MaskBits;

      if (!(mask &= mask - MaskType(1))) {
        pool.freeMask &= uint16_t(~maskBit);

        if (unlikely(!pool.freeMask))
          removePageFromList(uint32_t(pageIndex), listIndex);
      }

      return computeByteAddress(uint32_t(pageIndex), itemIndex, listIndex);
    }
  }


  bool DxvkPoolAllocator::free(uint64_t address, uint64_t size) {
    if (unlikely(size > MaxSize))
      return false;

    uint32_t listIndex = computeListIndex(size);

    uint32_t pageIndex = computePageIndexFromByteAddress(address);
    uint32_t itemIndex = computeItemIndexFromByteAddress(address, listIndex);

    uint32_t poolCapacity = computePoolCapacity(listIndex);

    // Return the allocation to the given pool and add the page back
    // to the free list if it was previously full. If the page is now
    // unused, return it to the allocator.
    if (likely(poolCapacity <= MaskBits)) {
      PageInfo& page = m_pageInfos[pageIndex];

      if (unlikely(!page.pool))
        addPageToList(pageIndex, listIndex);

      page.pool |= MaskType(1) << itemIndex;

      if (unlikely((poolCapacity == MaskBits && page.pool == MaskType(-1))
                || (poolCapacity < MaskBits && bit::tzcnt(page.pool + MaskType(1)) >= poolCapacity)))
        return freePage(pageIndex, listIndex);

      return false;
    } else {
      PageInfo& page = m_pageInfos[pageIndex];
      PagePool& pool = m_pagePools[uint32_t(page.pool)];

      if (unlikely(!pool.freeMask))
        addPageToList(pageIndex, listIndex);

      uint32_t maskIndex = itemIndex / MaskBits;
      MaskType maskBit = MaskType(1) << maskIndex;

      MaskType& mask = pool.subPools[maskIndex];
      mask |= MaskType(1) << (itemIndex % MaskBits);

      pool.freeMask |= uint16_t(maskBit);

      if (!(mask + MaskType(1))) {
        pool.usedMask &= uint16_t(~maskBit);

        if (unlikely(!pool.usedMask)) {
          freePagePool(uint32_t(page.pool));
          return freePage(pageIndex, listIndex);
        }
      }

      return false;
    }
  }


  int32_t DxvkPoolAllocator::allocPage(uint32_t listIndex) {
    int32_t pageIndex = m_pageAllocator->allocPages(1u, 1u);

    if (unlikely(pageIndex < 0))
      return -1;

    if (unlikely(uint32_t(pageIndex) >= m_pageInfos.size())) {
      uint32_t chunkCount = (uint32_t(pageIndex) >> DxvkPageAllocator::ChunkPageBits) + 1u;
      m_pageInfos.resize(chunkCount << DxvkPageAllocator::ChunkPageBits);
    }

    addPageToList(uint32_t(pageIndex), listIndex);
    return pageIndex;
  }


  bool DxvkPoolAllocator::freePage(uint32_t pageIndex, uint32_t listIndex) {
    removePageFromList(pageIndex, listIndex);

    return m_pageAllocator->freePages(pageIndex, 1u);
  }


  void DxvkPoolAllocator::addPageToList(uint32_t pageIndex, uint32_t listIndex) {
    // Add page to the end of the list. Allocations within a single page
    // often have similar lifetimes, so not reusing the page immediately
    // increases the chances of it getting freed.
    PageInfo& page = m_pageInfos[pageIndex];
    page.prev = m_pageLists[listIndex].tail;
    page.next = -1;

    if (page.prev >= 0)
      m_pageInfos[uint32_t(page.prev)].next = int32_t(pageIndex);
    else
      m_pageLists[listIndex].head = int32_t(pageIndex);

    m_pageLists[listIndex].tail = int32_t(pageIndex);
  }


  void DxvkPoolAllocator::removePageFromList(uint32_t pageIndex, uint32_t listIndex) {
    // The list of non-full pages is organized in a double-linked list so
    // that entries can get removed in constant time whenever a page gets
    // filled or removed.
    PageInfo& page = m_pageInfos[pageIndex];

    if (page.prev >= 0)
      m_pageInfos[uint32_t(page.prev)].next = page.next;
    else
      m_pageLists[listIndex].head = page.next;

    if (page.next >= 0)
      m_pageInfos[uint32_t(page.next)].prev = page.prev;
    else
      m_pageLists[listIndex].tail = page.prev;

    page.prev = -1;
    page.next = -1;
  }


  uint32_t DxvkPoolAllocator::allocPagePool(uint32_t capacity) {
    PagePool* pool;
    uint32_t poolIndex;

    if (unlikely(m_freePool < 0)) {
      // Allocate new pool as necessary.
      pool = &m_pagePools.emplace_back();
      pool->subPools.fill(MaskType(-1));

      poolIndex = uint32_t(m_pagePools.size() - 1u);
    } else {
      // Otherwise, just use the free list.
      pool = &m_pagePools[uint32_t(m_freePool)];
      poolIndex = uint32_t(std::exchange(m_freePool, pool->nextPool));
    }

    // Initialize free mask to the correct capacity. Everything
    // else is assumed to be in its default initialized state.
    uint32_t maskCount = capacity / MaskBits;
    pool->freeMask = uint16_t((1u << maskCount) - 1u);
    pool->usedMask = 1u;
    pool->subPools[0] = MaskType(-2);
    return poolIndex;
  }


  void DxvkPoolAllocator::freePagePool(uint32_t poolIndex) {
    PagePool* pool = &m_pagePools[poolIndex];
    pool->nextPool = m_freePool;

    m_freePool = int32_t(poolIndex);
  }


  uint32_t DxvkPoolAllocator::computeListIndex(uint64_t size) {
    size = std::max(size, MinSize);

    // Use leading zero count to determine the size category and
    // basically round up to the next power of two. Pools are
    // ordered by allocation size in descending order.
    return bit::lzcnt(uint32_t(size) - 1u) - (33u - DxvkPageAllocator::PageBits);
  }


  uint32_t DxvkPoolAllocator::computePoolCapacity(uint32_t index) {
    // Number of objects we can allocate in the pool.
    return 2u << index;
  }


  uint64_t DxvkPoolAllocator::computeByteAddress(uint32_t page, uint32_t index, uint32_t list) {
    uint32_t shift = DxvkPageAllocator::PageBits - 1u - list;
    return DxvkPageAllocator::PageSize * uint64_t(page) + (uint64_t(index) << shift);
  }


  uint32_t DxvkPoolAllocator::computePageIndexFromByteAddress(uint64_t address) {
    return uint32_t(address / DxvkPageAllocator::PageSize);
  }


  uint32_t DxvkPoolAllocator::computeItemIndexFromByteAddress(uint64_t address, uint32_t list) {
    uint32_t shift = DxvkPageAllocator::PageBits - 1u - list;
    return uint32_t((address & (DxvkPageAllocator::PageSize - 1u)) >> shift);
  }

}
