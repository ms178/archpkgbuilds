// SPDX-License-Identifier: MIT
/*
 * Copyright © 2021 Intel Corporation
 */

#include <kunit/test-bug.h>

#include <linux/export.h>
#include <linux/kmemleak.h>
#include <linux/module.h>
#include <linux/sizes.h>

#include <drm/drm_buddy.h>

static struct kmem_cache *slab_blocks;

static struct drm_buddy_block *drm_block_alloc(struct drm_buddy *mm,
					       struct drm_buddy_block *parent,
					       unsigned int order,
					       u64 offset)
{
	struct drm_buddy_block *block;

	BUG_ON(order > DRM_BUDDY_MAX_ORDER);

	block = kmem_cache_zalloc(slab_blocks, GFP_KERNEL);
	if (unlikely(!block))
		return NULL;

	block->header = offset | order;
	block->parent = parent;

	RB_CLEAR_NODE(&block->rb);

	BUG_ON(block->header & DRM_BUDDY_HEADER_UNUSED);
	return block;
}

static void drm_block_free(struct drm_buddy *mm __maybe_unused,
			   struct drm_buddy_block *block)
{
	kmem_cache_free(slab_blocks, block);
}

static __always_inline struct rb_root *
__get_root(struct drm_buddy *mm, unsigned int order, enum free_tree tree)
{
	return (tree == CLEAR_TREE) ? &mm->clear_tree[order]
				    : &mm->dirty_tree[order];
}

static __always_inline enum free_tree
__get_tree_for_block(struct drm_buddy_block *block)
{
	return drm_buddy_block_is_clear(block) ? CLEAR_TREE : DIRTY_TREE;
}

static __always_inline enum free_tree
__get_tree_for_flags(unsigned long flags)
{
	return (flags & DRM_BUDDY_CLEAR_ALLOCATION) ? CLEAR_TREE : DIRTY_TREE;
}

static __always_inline struct drm_buddy_block *
rbtree_get_entry(struct rb_node *node)
{
	return node ? rb_entry(node, struct drm_buddy_block, rb) : NULL;
}

static __always_inline struct drm_buddy_block *
rbtree_prev_entry(struct rb_node *node)
{
	return rbtree_get_entry(rb_prev(node));
}

static __always_inline struct drm_buddy_block *
rbtree_first_entry(struct rb_root *root)
{
	return rbtree_get_entry(rb_first(root));
}

static __always_inline struct drm_buddy_block *
rbtree_last_entry(struct rb_root *root)
{
	return rbtree_get_entry(rb_last(root));
}

static __always_inline bool rbtree_is_empty(struct rb_root *root)
{
	return RB_EMPTY_ROOT(root);
}

static void rbtree_insert(struct drm_buddy *mm,
			  struct drm_buddy_block *block,
			  enum free_tree tree)
{
	struct rb_node **link, *parent = NULL;
	struct rb_root *root;
	unsigned int order;
	u64 block_offset;

	order = drm_buddy_block_order(block);
	block_offset = drm_buddy_block_offset(block);

	root = __get_root(mm, order, tree);
	link = &root->rb_node;

	while (*link) {
		struct drm_buddy_block *node;

		parent = *link;
		node = rbtree_get_entry(parent);

		if (block_offset < drm_buddy_block_offset(node))
			link = &parent->rb_left;
		else
			link = &parent->rb_right;
	}

	block->tree = tree;

	rb_link_node(&block->rb, parent, link);
	rb_insert_color(&block->rb, root);
}

static void rbtree_remove(struct drm_buddy *mm,
			  struct drm_buddy_block *block)
{
	const unsigned int order = drm_buddy_block_order(block);
	struct rb_root *root = __get_root(mm, order, block->tree);

	rb_erase(&block->rb, root);
	RB_CLEAR_NODE(&block->rb);
}

static __always_inline void clear_reset(struct drm_buddy_block *block)
{
	block->header &= ~DRM_BUDDY_HEADER_CLEAR;
}

static __always_inline void mark_cleared(struct drm_buddy_block *block)
{
	block->header |= DRM_BUDDY_HEADER_CLEAR;
}

static __always_inline void mark_allocated(struct drm_buddy *mm,
					   struct drm_buddy_block *block)
{
	block->header &= ~DRM_BUDDY_HEADER_STATE;
	block->header |= DRM_BUDDY_ALLOCATED;
	rbtree_remove(mm, block);
}

static __always_inline void mark_free(struct drm_buddy *mm,
				      struct drm_buddy_block *block)
{
	block->header &= ~DRM_BUDDY_HEADER_STATE;
	block->header |= DRM_BUDDY_FREE;
	rbtree_insert(mm, block, __get_tree_for_block(block));
}

static __always_inline void mark_split(struct drm_buddy *mm,
				       struct drm_buddy_block *block)
{
	block->header &= ~DRM_BUDDY_HEADER_STATE;
	block->header |= DRM_BUDDY_SPLIT;
	rbtree_remove(mm, block);
}

static __always_inline bool overlaps(u64 s1, u64 e1, u64 s2, u64 e2)
{
	return s1 <= e2 && e1 >= s2;
}

static __always_inline bool contains(u64 s1, u64 e1, u64 s2, u64 e2)
{
	return s1 <= s2 && e1 >= e2;
}

static struct drm_buddy_block *
__get_buddy(struct drm_buddy_block *block)
{
	struct drm_buddy_block *parent = block->parent;

	if (!parent)
		return NULL;

	return (parent->left == block) ? parent->right : parent->left;
}

static unsigned int __drm_buddy_free(struct drm_buddy *mm,
				     struct drm_buddy_block *block,
				     bool force_merge)
{
	struct drm_buddy_block *parent;

	while ((parent = block->parent)) {
		struct drm_buddy_block *buddy = __get_buddy(block);

		if (!drm_buddy_block_is_free(buddy))
			break;

		if (!force_merge) {
			if (drm_buddy_block_is_clear(block) !=
			    drm_buddy_block_is_clear(buddy))
				break;

			if (drm_buddy_block_is_clear(block))
				mark_cleared(parent);
		}

		rbtree_remove(mm, buddy);
		if (force_merge && drm_buddy_block_is_clear(buddy))
			mm->clear_avail -= drm_buddy_block_size(mm, buddy);

		drm_block_free(mm, block);
		drm_block_free(mm, buddy);

		block = parent;
	}

	mark_free(mm, block);
	return drm_buddy_block_order(block);
}

static int __force_merge(struct drm_buddy *mm,
			 u64 start,
			 u64 end,
			 unsigned int min_order)
{
	unsigned int order;
	int i;

	if (!min_order)
		return -ENOMEM;

	if (min_order > mm->max_order)
		return -EINVAL;

	for_each_free_tree() {
		for (i = (int)min_order - 1; i >= 0; i--) {
			struct rb_root *root = __get_root(mm, (unsigned int)i, tree);
			struct drm_buddy_block *block, *prev_block;

			for_each_rb_entry_reverse_safe(block, prev_block, root, rb) {
				struct drm_buddy_block *buddy;
				u64 block_start, block_end;

				if (RB_EMPTY_NODE(&block->rb))
					break;

				if (!block->parent)
					continue;

				block_start = drm_buddy_block_offset(block);
				block_end = block_start + drm_buddy_block_size(mm, block) - 1;

				if (!contains(start, end, block_start, block_end))
					continue;

				buddy = __get_buddy(block);
				if (!drm_buddy_block_is_free(buddy))
					continue;

				WARN_ON(drm_buddy_block_is_clear(block) ==
					drm_buddy_block_is_clear(buddy));

				if (prev_block == buddy) {
					if (prev_block != rbtree_first_entry(root))
						prev_block = rbtree_prev_entry(&prev_block->rb);
				}

				rbtree_remove(mm, block);
				if (drm_buddy_block_is_clear(block))
					mm->clear_avail -= drm_buddy_block_size(mm, block);

				order = __drm_buddy_free(mm, block, true);
				if (order >= min_order)
					return 0;
			}
		}
	}

	return -ENOMEM;
}

int drm_buddy_init(struct drm_buddy *mm, u64 size, u64 chunk_size)
{
	unsigned int i;
	u64 offset;

	if (size < chunk_size)
		return -EINVAL;

	if (chunk_size < SZ_4K)
		return -EINVAL;

	if (!is_power_of_2(chunk_size))
		return -EINVAL;

	size = round_down(size, chunk_size);

	mm->size = size;
	mm->avail = size;
	mm->clear_avail = 0;
	mm->chunk_size = chunk_size;
	mm->max_order = ilog2(size) - ilog2(chunk_size);

	BUG_ON(mm->max_order > DRM_BUDDY_MAX_ORDER);

	mm->clear_tree = kmalloc_array(mm->max_order + 1,
				       sizeof(struct rb_root),
				       GFP_KERNEL);
	if (unlikely(!mm->clear_tree))
		return -ENOMEM;

	mm->dirty_tree = kmalloc_array(mm->max_order + 1,
				       sizeof(struct rb_root),
				       GFP_KERNEL);
	if (unlikely(!mm->dirty_tree))
		goto out_free_clear_tree;

	for (i = 0; i <= mm->max_order; ++i) {
		mm->clear_tree[i] = RB_ROOT;
		mm->dirty_tree[i] = RB_ROOT;
	}

	mm->n_roots = hweight64(size);

	mm->roots = kmalloc_array(mm->n_roots,
				  sizeof(struct drm_buddy_block *),
				  GFP_KERNEL);
	if (unlikely(!mm->roots))
		goto out_free_dirty_tree;

	offset = 0;
	i = 0;

	do {
		struct drm_buddy_block *root;
		unsigned int order;
		u64 root_size;

		order = ilog2(size) - ilog2(chunk_size);
		root_size = chunk_size << order;

		root = drm_block_alloc(mm, NULL, order, offset);
		if (unlikely(!root))
			goto out_free_roots;

		mark_free(mm, root);

		BUG_ON(i > mm->max_order);
		BUG_ON(drm_buddy_block_size(mm, root) < chunk_size);

		mm->roots[i] = root;

		offset += root_size;
		size -= root_size;
		i++;
	} while (size);

	return 0;

out_free_roots:
	while (i--)
		drm_block_free(mm, mm->roots[i]);
	kfree(mm->roots);
out_free_dirty_tree:
	kfree(mm->dirty_tree);
out_free_clear_tree:
	kfree(mm->clear_tree);
	return -ENOMEM;
}
EXPORT_SYMBOL(drm_buddy_init);

void drm_buddy_fini(struct drm_buddy *mm)
{
	const unsigned int chunk_shift = ilog2(mm->chunk_size);
	u64 root_size, size, start;
	unsigned int i, order;

	size = mm->size;

	for (i = 0; i < mm->n_roots; ++i) {
		order = ilog2(size) - chunk_shift;
		start = drm_buddy_block_offset(mm->roots[i]);
		__force_merge(mm, start, start + size, order);

		if (WARN_ON(!drm_buddy_block_is_free(mm->roots[i])))
			kunit_fail_current_test("buddy_fini() root");

		drm_block_free(mm, mm->roots[i]);

		root_size = mm->chunk_size << order;
		size -= root_size;
	}

	WARN_ON(mm->avail != mm->size);

	kfree(mm->roots);
	kfree(mm->clear_tree);
	kfree(mm->dirty_tree);
}
EXPORT_SYMBOL(drm_buddy_fini);

static int split_block(struct drm_buddy *mm,
		       struct drm_buddy_block *block)
{
	struct drm_buddy_block *left, *right;
	unsigned int block_order;
	u64 left_off, right_off;
	void *objs[2];
	int n;

	BUG_ON(!drm_buddy_block_is_free(block));
	BUG_ON(!drm_buddy_block_order(block));

	block_order = drm_buddy_block_order(block) - 1;
	left_off = drm_buddy_block_offset(block);
	right_off = left_off + (mm->chunk_size << block_order);

	n = kmem_cache_alloc_bulk(slab_blocks, GFP_KERNEL, 2, objs);
	if (likely(n == 2)) {
		left = objs[0];
		right = objs[1];
		memset(left, 0, sizeof(*left));
		memset(right, 0, sizeof(*right));
	} else {
		if (n == 1)
			kmem_cache_free(slab_blocks, objs[0]);

		left = kmem_cache_zalloc(slab_blocks, GFP_KERNEL);
		if (unlikely(!left))
			return -ENOMEM;

		right = kmem_cache_zalloc(slab_blocks, GFP_KERNEL);
		if (unlikely(!right)) {
			kmem_cache_free(slab_blocks, left);
			return -ENOMEM;
		}
	}

	left->header = left_off | block_order;
	right->header = right_off | block_order;
	left->parent = block;
	right->parent = block;

	RB_CLEAR_NODE(&left->rb);
	RB_CLEAR_NODE(&right->rb);

	BUG_ON(left->header & DRM_BUDDY_HEADER_UNUSED);
	BUG_ON(right->header & DRM_BUDDY_HEADER_UNUSED);

	block->left = left;
	block->right = right;

	if (drm_buddy_block_is_clear(block)) {
		mark_cleared(left);
		mark_cleared(right);
		clear_reset(block);
	}

	mark_free(mm, left);
	mark_free(mm, right);
	mark_split(mm, block);

	return 0;
}

struct drm_buddy_block *
drm_get_buddy(struct drm_buddy_block *block)
{
	return __get_buddy(block);
}
EXPORT_SYMBOL(drm_get_buddy);

void drm_buddy_reset_clear(struct drm_buddy *mm, bool is_clear)
{
	const unsigned int chunk_shift = ilog2(mm->chunk_size);
	u64 root_size, size, start;
	unsigned int i, order;

	size = mm->size;
	for (i = 0; i < mm->n_roots; ++i) {
		order = ilog2(size) - chunk_shift;
		start = drm_buddy_block_offset(mm->roots[i]);
		__force_merge(mm, start, start + size, order);

		root_size = mm->chunk_size << order;
		size -= root_size;
	}

	if (is_clear) {
		for (i = 0; i <= mm->max_order; ++i) {
			struct rb_root *root = __get_root(mm, i, DIRTY_TREE);
			struct drm_buddy_block *block, *n;

			for_each_rb_entry_reverse_safe(block, n, root, rb) {
				rbtree_remove(mm, block);
				mark_cleared(block);
				rbtree_insert(mm, block, CLEAR_TREE);
			}
		}
		mm->clear_avail = mm->avail;
	} else {
		for (i = 0; i <= mm->max_order; ++i) {
			struct rb_root *root = __get_root(mm, i, CLEAR_TREE);
			struct drm_buddy_block *block, *n;

			for_each_rb_entry_reverse_safe(block, n, root, rb) {
				rbtree_remove(mm, block);
				clear_reset(block);
				rbtree_insert(mm, block, DIRTY_TREE);
			}
		}
		mm->clear_avail = 0;
	}
}
EXPORT_SYMBOL(drm_buddy_reset_clear);

void drm_buddy_free_block(struct drm_buddy *mm,
			  struct drm_buddy_block *block)
{
	const u64 block_size = drm_buddy_block_size(mm, block);

	BUG_ON(!drm_buddy_block_is_allocated(block));

	mm->avail += block_size;
	if (drm_buddy_block_is_clear(block))
		mm->clear_avail += block_size;

	__drm_buddy_free(mm, block, false);
}
EXPORT_SYMBOL(drm_buddy_free_block);

static void __drm_buddy_free_list(struct drm_buddy *mm,
				  struct list_head *objects,
				  bool mark_clear,
				  bool mark_dirty)
{
	struct drm_buddy_block *block, *on;

	WARN_ON(mark_dirty && mark_clear);

	list_for_each_entry_safe(block, on, objects, link) {
		if (mark_clear)
			mark_cleared(block);
		else if (mark_dirty)
			clear_reset(block);
		drm_buddy_free_block(mm, block);
		cond_resched();
	}
	INIT_LIST_HEAD(objects);
}

static void drm_buddy_free_list_internal(struct drm_buddy *mm,
					 struct list_head *objects)
{
	__drm_buddy_free_list(mm, objects, false, false);
}

void drm_buddy_free_list(struct drm_buddy *mm,
			 struct list_head *objects,
			 unsigned int flags)
{
	bool mark_clear = flags & DRM_BUDDY_CLEARED;

	__drm_buddy_free_list(mm, objects, mark_clear, !mark_clear);
}
EXPORT_SYMBOL(drm_buddy_free_list);

static __always_inline bool
block_incompatible(struct drm_buddy_block *block, unsigned long flags)
{
	bool needs_clear = flags & DRM_BUDDY_CLEAR_ALLOCATION;

	return needs_clear != drm_buddy_block_is_clear(block);
}

static struct drm_buddy_block *
__alloc_range_bias(struct drm_buddy *mm,
		   u64 start, u64 end,
		   unsigned int order,
		   unsigned long flags,
		   bool fallback)
{
	const u64 req_size = mm->chunk_size << order;
	struct drm_buddy_block *block;
	unsigned int i;
	LIST_HEAD(dfs);
	int err;

	end = end - 1;

	for (i = 0; i < mm->n_roots; ++i)
		list_add_tail(&mm->roots[i]->tmp_link, &dfs);

	do {
		u64 block_start;
		u64 block_end;

		block = list_first_entry_or_null(&dfs,
						 struct drm_buddy_block,
						 tmp_link);
		if (!block)
			break;

		list_del(&block->tmp_link);

		if (drm_buddy_block_order(block) < order)
			continue;

		block_start = drm_buddy_block_offset(block);
		block_end = block_start + drm_buddy_block_size(mm, block) - 1;

		if (!overlaps(start, end, block_start, block_end))
			continue;

		if (drm_buddy_block_is_allocated(block))
			continue;

		if (block_start < start || block_end > end) {
			u64 adjusted_start = max(block_start, start);
			u64 adjusted_end = min(block_end, end);

			if (round_down(adjusted_end + 1, req_size) <=
			    round_up(adjusted_start, req_size))
				continue;
		}

		if (!fallback && block_incompatible(block, flags))
			continue;

		if (contains(start, end, block_start, block_end) &&
		    order == drm_buddy_block_order(block)) {
			if (drm_buddy_block_is_free(block))
				return block;
			continue;
		}

		if (!drm_buddy_block_is_split(block)) {
			err = split_block(mm, block);
			if (unlikely(err))
				return ERR_PTR(err);
		}

		list_add(&block->right->tmp_link, &dfs);
		list_add(&block->left->tmp_link, &dfs);
	} while (1);

	return ERR_PTR(-ENOSPC);
}

static struct drm_buddy_block *
__drm_buddy_alloc_range_bias(struct drm_buddy *mm,
			     u64 start, u64 end,
			     unsigned int order,
			     unsigned long flags)
{
	struct drm_buddy_block *block;

	block = __alloc_range_bias(mm, start, end, order, flags, false);
	if (IS_ERR(block))
		return __alloc_range_bias(mm, start, end, order, flags, true);

	return block;
}

static struct drm_buddy_block *
get_maxblock(struct drm_buddy *mm,
	     unsigned int order,
	     enum free_tree tree)
{
	struct drm_buddy_block *max_block = NULL;
	u64 max_offset = 0;
	unsigned int i;

	for (i = order; i <= mm->max_order; ++i) {
		struct rb_root *root = __get_root(mm, i, tree);
		struct drm_buddy_block *block;
		u64 offset;

		if (rbtree_is_empty(root))
			continue;

		block = rbtree_last_entry(root);
		if (unlikely(!block))
			continue;

		offset = drm_buddy_block_offset(block);
		if (!max_block || offset > max_offset) {
			max_block = block;
			max_offset = offset;
		}
	}

	return max_block;
}

static struct drm_buddy_block *
alloc_from_freetree(struct drm_buddy *mm,
		    unsigned int order,
		    unsigned long flags)
{
	struct drm_buddy_block *block = NULL;
	unsigned int alloc_order = order;
	struct rb_root *root;
	enum free_tree tree;

	tree = __get_tree_for_flags(flags);

	if (flags & DRM_BUDDY_TOPDOWN_ALLOCATION) {
		root = __get_root(mm, order, tree);
		if (!rbtree_is_empty(root)) {
			block = rbtree_last_entry(root);
			if (block)
				alloc_order = order;
		}

		if (!block) {
			block = get_maxblock(mm, order, tree);
			if (block)
				alloc_order = drm_buddy_block_order(block);
		}
	} else {
		for (alloc_order = order; alloc_order <= mm->max_order; ++alloc_order) {
			root = __get_root(mm, alloc_order, tree);
			if (!rbtree_is_empty(root)) {
				block = rbtree_last_entry(root);
				if (likely(block))
					break;
			}
		}
	}

	if (unlikely(!block)) {
		tree = (tree == CLEAR_TREE) ? DIRTY_TREE : CLEAR_TREE;

		for (alloc_order = order; alloc_order <= mm->max_order; ++alloc_order) {
			root = __get_root(mm, alloc_order, tree);
			if (!rbtree_is_empty(root)) {
				block = rbtree_last_entry(root);
				if (block)
					break;
			}
		}

		if (unlikely(!block))
			return ERR_PTR(-ENOSPC);
	}

	BUG_ON(!drm_buddy_block_is_free(block));

	while (alloc_order != order) {
		int err = split_block(mm, block);

		if (unlikely(err))
			return ERR_PTR(err);

		block = block->right;
		alloc_order--;
	}

	return block;
}

static int __alloc_range(struct drm_buddy *mm,
			 struct list_head *dfs,
			 u64 start, u64 size,
			 struct list_head *blocks,
			 u64 *total_allocated_on_err)
{
	struct drm_buddy_block *block;
	u64 total_allocated = 0;
	LIST_HEAD(allocated);
	u64 end;
	int err;

	end = start + size - 1;

	do {
		u64 block_start;
		u64 block_end;

		block = list_first_entry_or_null(dfs,
						 struct drm_buddy_block,
						 tmp_link);
		if (unlikely(!block))
			break;

		list_del(&block->tmp_link);

		block_start = drm_buddy_block_offset(block);
		block_end = block_start + drm_buddy_block_size(mm, block) - 1;

		if (!overlaps(start, end, block_start, block_end))
			continue;

		if (unlikely(drm_buddy_block_is_allocated(block))) {
			err = -ENOSPC;
			goto err_free;
		}

		if (likely(contains(start, end, block_start, block_end))) {
			if (likely(drm_buddy_block_is_free(block))) {
				const u64 bsz = drm_buddy_block_size(mm, block);

				mark_allocated(mm, block);
				total_allocated += bsz;
				mm->avail -= bsz;
				if (drm_buddy_block_is_clear(block))
					mm->clear_avail -= bsz;
				list_add_tail(&block->link, &allocated);
				continue;
			} else if (unlikely(!mm->clear_avail)) {
				err = -ENOSPC;
				goto err_free;
			}
		}

		if (likely(!drm_buddy_block_is_split(block))) {
			err = split_block(mm, block);
			if (unlikely(err))
				goto err_free;
		}

		list_add(&block->right->tmp_link, dfs);
		list_add(&block->left->tmp_link, dfs);
	} while (1);

	if (unlikely(total_allocated < size)) {
		err = -ENOSPC;
		goto err_free;
	}

	list_splice_tail(&allocated, blocks);
	return 0;

err_free:
	if (err == -ENOSPC && total_allocated_on_err) {
		list_splice_tail(&allocated, blocks);
		*total_allocated_on_err = total_allocated;
	} else {
		drm_buddy_free_list_internal(mm, &allocated);
	}
	return err;
}

static int __drm_buddy_alloc_range(struct drm_buddy *mm,
				   u64 start,
				   u64 size,
				   u64 *total_allocated_on_err,
				   struct list_head *blocks)
{
	LIST_HEAD(dfs);
	int i;

	for (i = 0; i < mm->n_roots; ++i)
		list_add_tail(&mm->roots[i]->tmp_link, &dfs);

	return __alloc_range(mm, &dfs, start, size,
			     blocks, total_allocated_on_err);
}

static int __alloc_contig_try_harder(struct drm_buddy *mm,
				     u64 size,
				     u64 min_block_size,
				     struct list_head *blocks)
{
	u64 rhs_offset, lhs_offset, lhs_size, filled;
	struct drm_buddy_block *block;
	LIST_HEAD(blocks_lhs);
	unsigned long pages;
	unsigned int order;
	const unsigned int chunk_shift = ilog2(mm->chunk_size);
	u64 modify_size;
	int err;

	modify_size = rounddown_pow_of_two(size);
	pages = modify_size >> chunk_shift;
	order = fls(pages) - 1;
	if (order == 0)
		return -ENOSPC;

	if (rbtree_is_empty(__get_root(mm, order, CLEAR_TREE)) &&
	    rbtree_is_empty(__get_root(mm, order, DIRTY_TREE)))
		return -ENOSPC;

	for_each_free_tree() {
		struct rb_root *root = __get_root(mm, order, tree);

		for_each_rb_entry_reverse(block, root, rb) {
			/* Allocate blocks traversing RHS */
			rhs_offset = drm_buddy_block_offset(block);
			err = __drm_buddy_alloc_range(mm, rhs_offset, size,
						      &filled, blocks);
			if (!err || err != -ENOSPC)
				return err;

			lhs_size = max((size - filled), min_block_size);
			if (!IS_ALIGNED(lhs_size, min_block_size))
				lhs_size = round_up(lhs_size, min_block_size);

			/* Allocate blocks traversing LHS */
			lhs_offset = drm_buddy_block_offset(block) - lhs_size;
			err = __drm_buddy_alloc_range(mm, lhs_offset, lhs_size,
						      NULL, &blocks_lhs);
			if (!err) {
				list_splice(&blocks_lhs, blocks);
				return 0;
			} else if (err != -ENOSPC) {
				drm_buddy_free_list_internal(mm, blocks);
				return err;
			}
			/* Free blocks for the next iteration */
			drm_buddy_free_list_internal(mm, blocks);
		}
	}

	return -ENOSPC;
}

/**
 * drm_buddy_block_trim - free unused pages
 *
 * @mm: DRM buddy manager
 * @start: start address to begin the trimming.
 * @new_size: original size requested
 * @blocks: Input and output list of allocated blocks.
 * MUST contain single block as input to be trimmed.
 * On success will contain the newly allocated blocks
 * making up the @new_size. Blocks always appear in
 * ascending order
 *
 * For contiguous allocation, we round up the size to the nearest
 * power of two value, drivers consume *actual* size, so remaining
 * portions are unused and can be optionally freed with this function
 *
 * Returns:
 * 0 on success, error code on failure.
 */
int drm_buddy_block_trim(struct drm_buddy *mm,
			 u64 *start,
			 u64 new_size,
			 struct list_head *blocks)
{
	struct drm_buddy_block *parent;
	struct drm_buddy_block *block;
	u64 block_start, block_end;
	LIST_HEAD(dfs);
	u64 new_start;
	int err;

	if (!list_is_singular(blocks))
		return -EINVAL;

	block = list_first_entry(blocks,
				 struct drm_buddy_block,
				 link);

	block_start = drm_buddy_block_offset(block);
	block_end = block_start + drm_buddy_block_size(mm, block);

	if (WARN_ON(!drm_buddy_block_is_allocated(block)))
		return -EINVAL;

	if (new_size > drm_buddy_block_size(mm, block))
		return -EINVAL;

	if (!new_size || !IS_ALIGNED(new_size, mm->chunk_size))
		return -EINVAL;

	if (new_size == drm_buddy_block_size(mm, block))
		return 0;

	new_start = block_start;
	if (start) {
		new_start = *start;

		if (new_start < block_start)
			return -EINVAL;

		if (!IS_ALIGNED(new_start, mm->chunk_size))
			return -EINVAL;

		if (range_overflows(new_start, new_size, block_end))
			return -EINVAL;
	}

	list_del(&block->link);
	mark_free(mm, block);
	mm->avail += drm_buddy_block_size(mm, block);
	if (drm_buddy_block_is_clear(block))
		mm->clear_avail += drm_buddy_block_size(mm, block);

	/* Prevent recursively freeing this node */
	parent = block->parent;
	block->parent = NULL;

	list_add(&block->tmp_link, &dfs);
	err =  __alloc_range(mm, &dfs, new_start, new_size, blocks, NULL);
	if (err) {
		mark_allocated(mm, block);
		mm->avail -= drm_buddy_block_size(mm, block);
		if (drm_buddy_block_is_clear(block))
			mm->clear_avail -= drm_buddy_block_size(mm, block);
		list_add(&block->link, blocks);
	}

	block->parent = parent;
	return err;
}
EXPORT_SYMBOL(drm_buddy_block_trim);

static struct drm_buddy_block *
__drm_buddy_alloc_blocks(struct drm_buddy *mm,
			 u64 start, u64 end,
			 unsigned int order,
			 unsigned long flags)
{
	if (flags & DRM_BUDDY_RANGE_ALLOCATION)
		/* Allocate traversing within the range */
		return  __drm_buddy_alloc_range_bias(mm, start, end,
						     order, flags);
	else
		/* Allocate from freetree */
		return alloc_from_freetree(mm, order, flags);
}

/**
 * drm_buddy_alloc_blocks - allocate power-of-two blocks
 *
 * @mm: DRM buddy manager to allocate from
 * @start: start of the allowed range for this block
 * @end: end of the allowed range for this block
 * @size: size of the allocation in bytes
 * @min_block_size: alignment of the allocation
 * @blocks: output list head to add allocated blocks
 * @flags: DRM_BUDDY_*_ALLOCATION flags
 *
 * alloc_range_bias() called on range limitations, which traverses
 * the tree and returns the desired block.
 *
 * alloc_from_freetree() called when *no* range restrictions
 * are enforced, which picks the block from the freetree.
 *
 * Returns:
 * 0 on success, error code on failure.
 */
int drm_buddy_alloc_blocks(struct drm_buddy *mm,
			   u64 start, u64 end, u64 size,
			   u64 min_block_size,
			   struct list_head *blocks,
			   unsigned long flags)
{
	struct drm_buddy_block *block = NULL;
	u64 original_size, original_min_size;
	unsigned int min_order, order;
	const u64 chunk_size = mm->chunk_size;
	const unsigned int chunk_shift = ilog2(chunk_size);
	LIST_HEAD(allocated);
	unsigned long pages;
	int err;

	if (size < chunk_size)
		return -EINVAL;

	if (min_block_size < chunk_size)
		return -EINVAL;

	if (!is_power_of_2(min_block_size))
		return -EINVAL;

	if (!IS_ALIGNED(start | end | size, chunk_size))
		return -EINVAL;

	if (end > mm->size)
		return -EINVAL;

	if (range_overflows(start, size, mm->size))
		return -EINVAL;

	if (start + size == end) {
		if (!IS_ALIGNED(start | end, min_block_size))
			return -EINVAL;

		return __drm_buddy_alloc_range(mm, start, size, NULL, blocks);
	}

	original_size = size;
	original_min_size = min_block_size;

	if (flags & DRM_BUDDY_CONTIGUOUS_ALLOCATION) {
		size = roundup_pow_of_two(size);
		min_block_size = size;
	} else if (!IS_ALIGNED(size, min_block_size)) {
		size = round_up(size, min_block_size);
	}

	pages = size >> chunk_shift;
	order = fls(pages) - 1;
	min_order = ilog2(min_block_size) - chunk_shift;

	do {
		unsigned int hi = fls(pages) - 1;

		order = min(order, hi);
		BUG_ON(order > mm->max_order);
		BUG_ON(order < min_order);

		do {
			block = __drm_buddy_alloc_blocks(mm, start, end,
							 order, flags);
			if (likely(!IS_ERR(block)))
				break;

			if (order-- == min_order) {
				if (mm->clear_avail &&
				    !__force_merge(mm, start, end, min_order)) {
					block = __drm_buddy_alloc_blocks(mm, start,
									 end,
									 min_order,
									 flags);
					if (!IS_ERR(block)) {
						order = min_order;
						break;
					}
				}

				if ((flags & DRM_BUDDY_CONTIGUOUS_ALLOCATION) &&
				    !(flags & DRM_BUDDY_RANGE_ALLOCATION))
					return __alloc_contig_try_harder(mm,
									 original_size,
									 original_min_size,
									 blocks);
				err = -ENOSPC;
				goto err_free;
			}
		} while (1);

		{
			const u64 bsz = chunk_size << order;

			mark_allocated(mm, block);
			mm->avail -= bsz;
			if (drm_buddy_block_is_clear(block))
				mm->clear_avail -= bsz;
			kmemleak_update_trace(block);
			list_add_tail(&block->link, &allocated);
			pages -= BIT(order);
		}

		if (!pages)
			break;
	} while (1);

	if (!(flags & DRM_BUDDY_TRIM_DISABLE) &&
	    original_size != size) {
		struct list_head *trim_list;
		LIST_HEAD(temp);
		u64 trim_size;

		trim_list = &allocated;
		trim_size = original_size;

		if (!list_is_singular(&allocated)) {
			block = list_last_entry(&allocated, typeof(*block), link);
			list_move(&block->link, &temp);
			trim_list = &temp;
			trim_size = (chunk_size << drm_buddy_block_order(block)) -
				(size - original_size);
		}

		drm_buddy_block_trim(mm, NULL, trim_size, trim_list);

		if (!list_empty(&temp))
			list_splice_tail(trim_list, &allocated);
	}

	list_splice_tail(&allocated, blocks);
	return 0;

err_free:
	drm_buddy_free_list_internal(mm, &allocated);
	return err;
}
EXPORT_SYMBOL(drm_buddy_alloc_blocks);

/**
 * drm_buddy_block_print - print block information
 *
 * @mm: DRM buddy manager
 * @block: DRM buddy block
 * @p: DRM printer to use
 */
void drm_buddy_block_print(struct drm_buddy *mm,
			   struct drm_buddy_block *block,
			   struct drm_printer *p)
{
	u64 start = drm_buddy_block_offset(block);
	u64 size = drm_buddy_block_size(mm, block);

	drm_printf(p, "%#018llx-%#018llx: %llu\n", start, start + size, size);
}
EXPORT_SYMBOL(drm_buddy_block_print);

/**
 * drm_buddy_print - print allocator state
 *
 * @mm: DRM buddy manager
 * @p: DRM printer to use
 */
void drm_buddy_print(struct drm_buddy *mm, struct drm_printer *p)
{
	int order;

	drm_printf(p, "chunk_size: %lluKiB, total: %lluMiB, free: %lluMiB, clear_free: %lluMiB\n",
		   mm->chunk_size >> 10, mm->size >> 20, mm->avail >> 20, mm->clear_avail >> 20);

	for (order = mm->max_order; order >= 0; order--) {
		struct drm_buddy_block *block;
		struct rb_root *root;
		u64 count = 0, free;

		for_each_free_tree() {
			root = __get_root(mm, order, tree);

			for_each_rb_entry(block, root, rb) {
				BUG_ON(!drm_buddy_block_is_free(block));
				count++;
			}
		}

		drm_printf(p, "order-%2d ", order);

		free = count * (mm->chunk_size << order);
		if (free < SZ_1M)
			drm_printf(p, "free: %8llu KiB", free >> 10);
		else
			drm_printf(p, "free: %8llu MiB", free >> 20);

		drm_printf(p, ", blocks: %llu\n", count);
	}
}
EXPORT_SYMBOL(drm_buddy_print);

static void drm_buddy_module_exit(void)
{
	kmem_cache_destroy(slab_blocks);
}

static int __init drm_buddy_module_init(void)
{
	slab_blocks = kmem_cache_create("drm_buddy_block",
					sizeof(struct drm_buddy_block),
					0,
					SLAB_HWCACHE_ALIGN | SLAB_RECLAIM_ACCOUNT,
					NULL);
	if (!slab_blocks)
		return -ENOMEM;

	return 0;
}

module_init(drm_buddy_module_init);
module_exit(drm_buddy_module_exit);

MODULE_DESCRIPTION("DRM Buddy Allocator");
MODULE_LICENSE("Dual MIT/GPL");
