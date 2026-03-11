/*
 * Copyright 2017 Józef Kucia for CodeWeavers
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

#define VKD3D_DBG_CHANNEL VKD3D_DBG_CHANNEL_SHADER

#include "vkd3d_shader_private.h"
#include "vkd3d_string.h"
#include "vkd3d_threads.h"

#include "vkd3d_platform.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <inttypes.h>

#include "spirv/unified1/spirv.h"

struct vkd3d_shader_path_cache
{
    bool enabled;
    char path[VKD3D_PATH_MAX];
};

static struct vkd3d_shader_path_cache vkd3d_shader_override_path_cache;
static pthread_once_t vkd3d_shader_override_path_once = PTHREAD_ONCE_INIT;

static void vkd3d_shader_init_override_path(void)
{
    vkd3d_shader_override_path_cache.enabled = vkd3d_get_env_var("VKD3D_SHADER_OVERRIDE",
            vkd3d_shader_override_path_cache.path, sizeof(vkd3d_shader_override_path_cache.path));
}

static struct vkd3d_shader_path_cache vkd3d_shader_dump_path_cache;
static pthread_once_t vkd3d_shader_dump_path_once = PTHREAD_ONCE_INIT;

static void vkd3d_shader_init_dump_path(void)
{
    vkd3d_shader_dump_path_cache.enabled = vkd3d_get_env_var("VKD3D_SHADER_DUMP_PATH",
            vkd3d_shader_dump_path_cache.path, sizeof(vkd3d_shader_dump_path_cache.path));
}

static void vkd3d_shader_dump_blob(const char *path, vkd3d_shader_hash_t hash, const void *data, size_t size, const char *ext)
{
    char filename[1024];
    FILE *f;
    int ret;

    if (!path || !data || !ext)
        return;

    ret = snprintf(filename, ARRAY_SIZE(filename), "%s/%016"PRIx64".%s", path, hash, ext);
    if (ret < 0 || (size_t)ret >= ARRAY_SIZE(filename))
    {
        ERR("Shader dump path is too long.\n");
        return;
    }

    INFO("Dumping blob to %s.\n", filename);

    if ((f = fopen(filename, "wbx")))
    {
        if (fwrite(data, 1, size, f) != size)
            ERR("Failed to write shader to %s.\n", filename);
        if (fclose(f))
            ERR("Failed to close stream %s.\n", filename);
    }
}

static bool vkd3d_shader_replace_path(const char *filename, vkd3d_shader_hash_t hash, const void **data, size_t *size)
{
    void *buffer = NULL;
    FILE *f = NULL;
    size_t len;
    long file_len;

    if (!filename || !data || !size)
        return false;

    if (!(f = fopen(filename, "rb")))
        goto err;

    if (fseek(f, 0, SEEK_END) < 0)
        goto err;

    file_len = ftell(f);
    if (file_len < (long)(5 * sizeof(uint32_t)))
        goto err;

    if ((unsigned long)file_len > SIZE_MAX)
        goto err;

    len = (size_t)file_len;
    if (len & (sizeof(uint32_t) - 1))
        goto err;

    rewind(f);
    buffer = vkd3d_malloc(len);
    if (!buffer)
        goto err;

    if (fread(buffer, 1, len, f) != len)
        goto err;

    if (((const uint32_t *)buffer)[0] != SpvMagicNumber)
        goto err;

    *data = buffer;
    *size = len;
    INFO("Overriding shader hash %016"PRIx64" with alternative SPIR-V module from %s!\n", hash, filename);
    fclose(f);
    return true;

err:
    if (f)
        fclose(f);
    vkd3d_free(buffer);
    return false;
}

bool vkd3d_shader_replace(vkd3d_shader_hash_t hash, const void **data, size_t *size)
{
    char filename[1024];
    int ret;

    if (!data || !size)
        return false;

    pthread_once(&vkd3d_shader_override_path_once, vkd3d_shader_init_override_path);

    if (!vkd3d_shader_override_path_cache.enabled)
        return false;

    ret = snprintf(filename, ARRAY_SIZE(filename), "%s/%016"PRIx64".spv",
            vkd3d_shader_override_path_cache.path, hash);
    if (ret < 0 || (size_t)ret >= ARRAY_SIZE(filename))
        return false;

    return vkd3d_shader_replace_path(filename, hash, data, size);
}

bool vkd3d_shader_replace_export(vkd3d_shader_hash_t hash, const void **data, size_t *size, const char *export)
{
    char filename[1024];
    int ret;

    if (!data || !size || !export)
        return false;

    pthread_once(&vkd3d_shader_override_path_once, vkd3d_shader_init_override_path);

    if (!vkd3d_shader_override_path_cache.enabled)
        return false;

    ret = snprintf(filename, ARRAY_SIZE(filename), "%s/%016"PRIx64".lib.%s.spv",
            vkd3d_shader_override_path_cache.path, hash, export);
    if (ret < 0 || (size_t)ret >= ARRAY_SIZE(filename))
        return false;

    return vkd3d_shader_replace_path(filename, hash, data, size);
}

void vkd3d_shader_dump_shader(vkd3d_shader_hash_t hash, const struct vkd3d_shader_code *shader, const char *ext)
{
    if (!shader || !shader->code || !ext)
        return;

    pthread_once(&vkd3d_shader_dump_path_once, vkd3d_shader_init_dump_path);

    if (!vkd3d_shader_dump_path_cache.enabled)
        return;

    vkd3d_shader_dump_blob(vkd3d_shader_dump_path_cache.path, hash, shader->code, shader->size, ext);
}

void vkd3d_shader_dump_spirv_shader(vkd3d_shader_hash_t hash, const struct vkd3d_shader_code *shader)
{
    if (!shader || !shader->code)
        return;

    pthread_once(&vkd3d_shader_dump_path_once, vkd3d_shader_init_dump_path);

    if (!vkd3d_shader_dump_path_cache.enabled)
        return;

    vkd3d_shader_dump_blob(vkd3d_shader_dump_path_cache.path, hash, shader->code, shader->size, "spv");
}

void vkd3d_shader_dump_spirv_shader_export(vkd3d_shader_hash_t hash, const struct vkd3d_shader_code *shader,
        const char *export)
{
    char tag[1024];
    int ret;

    if (!shader || !shader->code || !export)
        return;

    pthread_once(&vkd3d_shader_dump_path_once, vkd3d_shader_init_dump_path);

    if (!vkd3d_shader_dump_path_cache.enabled)
        return;

    ret = snprintf(tag, sizeof(tag), "lib.%s.spv", export);
    if (ret < 0 || (size_t)ret >= sizeof(tag))
        return;

    vkd3d_shader_dump_blob(vkd3d_shader_dump_path_cache.path, hash, shader->code, shader->size, tag);
}

static int vkd3d_shader_validate_compile_args(const struct vkd3d_shader_compile_arguments *compile_args)
{
    if (!compile_args)
        return VKD3D_OK;

    switch (compile_args->target)
    {
        case VKD3D_SHADER_TARGET_SPIRV_VULKAN_1_0:
            break;
        default:
            WARN("Invalid shader target %#x.\n", compile_args->target);
            return VKD3D_ERROR_INVALID_ARGUMENT;
    }

    return VKD3D_OK;
}

int vkd3d_shader_compile_dxbc(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_shader_code *spirv,
        struct vkd3d_shader_code_debug *spirv_debug,
        unsigned int compiler_options,
        const struct vkd3d_shader_interface_info *shader_interface_info,
        const struct vkd3d_shader_compile_arguments *compile_args)
{
    bool is_dxil;
    int ret;

    TRACE("dxbc {%p, %zu}, spirv %p, compiler_options %#x, shader_interface_info %p, compile_args %p.\n",
            dxbc ? dxbc->code : NULL, dxbc ? dxbc->size : 0, spirv, compiler_options,
            shader_interface_info, compile_args);

    if (!dxbc || !dxbc->code || !dxbc->size || !spirv)
        return VKD3D_ERROR_INVALID_ARGUMENT;

    if ((ret = vkd3d_shader_validate_compile_args(compile_args)) < 0)
        return ret;

    is_dxil = shader_is_dxil(dxbc->code, dxbc->size);
    spirv->meta.hash = 0;
    return vkd3d_shader_compile_dxil(dxbc, spirv, spirv_debug, shader_interface_info, compile_args, is_dxil);
}

void vkd3d_shader_free_shader_code(struct vkd3d_shader_code *shader_code)
{
    if (!shader_code)
        return;

    vkd3d_free((void *)shader_code->code);
}

void vkd3d_shader_free_shader_code_debug(struct vkd3d_shader_code_debug *shader_code)
{
    if (!shader_code)
        return;

    vkd3d_free((void *)shader_code->debug_entry_point_name);
}

static void vkd3d_shader_free_root_signature_v_1_0(struct vkd3d_root_signature_desc *root_signature)
{
    unsigned int i;

    for (i = 0; i < root_signature->parameter_count; ++i)
    {
        const struct vkd3d_root_parameter *parameter = &root_signature->parameters[i];

        if (parameter->parameter_type == VKD3D_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE)
            vkd3d_free((void *)parameter->descriptor_table.descriptor_ranges);
    }
    vkd3d_free((void *)root_signature->parameters);
    vkd3d_free((void *)root_signature->static_samplers);

    memset(root_signature, 0, sizeof(*root_signature));
}

static void vkd3d_shader_free_root_signature_v_1_1(struct vkd3d_root_signature_desc1 *root_signature)
{
    unsigned int i;

    for (i = 0; i < root_signature->parameter_count; ++i)
    {
        const struct vkd3d_root_parameter1 *parameter = &root_signature->parameters[i];

        if (parameter->parameter_type == VKD3D_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE)
            vkd3d_free((void *)parameter->descriptor_table.descriptor_ranges);
    }
    vkd3d_free((void *)root_signature->parameters);
    vkd3d_free((void *)root_signature->static_samplers);

    memset(root_signature, 0, sizeof(*root_signature));
}

static void vkd3d_shader_free_root_signature_v_1_2(struct vkd3d_root_signature_desc2 *root_signature)
{
    unsigned int i;

    for (i = 0; i < root_signature->parameter_count; ++i)
    {
        const struct vkd3d_root_parameter1 *parameter = &root_signature->parameters[i];

        if (parameter->parameter_type == VKD3D_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE)
            vkd3d_free((void *)parameter->descriptor_table.descriptor_ranges);
    }
    vkd3d_free((void *)root_signature->parameters);
    vkd3d_free((void *)root_signature->static_samplers);

    memset(root_signature, 0, sizeof(*root_signature));
}

void vkd3d_shader_free_root_signature(struct vkd3d_versioned_root_signature_desc *desc)
{
    if (!desc)
        return;

    if (desc->version == VKD3D_ROOT_SIGNATURE_VERSION_1_0)
        vkd3d_shader_free_root_signature_v_1_0(&desc->v_1_0);
    else if (desc->version == VKD3D_ROOT_SIGNATURE_VERSION_1_1)
        vkd3d_shader_free_root_signature_v_1_1(&desc->v_1_1);
    else if (desc->version == VKD3D_ROOT_SIGNATURE_VERSION_1_2)
        vkd3d_shader_free_root_signature_v_1_2(&desc->v_1_2);
    else if (desc->version)
    {
        FIXME("Unknown version %#x.\n", desc->version);
        return;
    }

    desc->version = 0;
}

int vkd3d_shader_parse_input_signature(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_shader_signature *signature)
{
    TRACE("dxbc {%p, %zu}, signature %p.\n",
            dxbc ? dxbc->code : NULL, dxbc ? dxbc->size : 0, signature);

    if (!dxbc || !dxbc->code || !signature)
        return VKD3D_ERROR_INVALID_ARGUMENT;

    return shader_parse_input_signature(dxbc->code, dxbc->size, signature);
}

int vkd3d_shader_parse_output_signature(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_shader_signature *signature)
{
    TRACE("dxbc {%p, %zu}, signature %p.\n",
            dxbc ? dxbc->code : NULL, dxbc ? dxbc->size : 0, signature);

    if (!dxbc || !dxbc->code || !signature)
        return VKD3D_ERROR_INVALID_ARGUMENT;

    return shader_parse_output_signature(dxbc->code, dxbc->size, signature);
}

int vkd3d_shader_parse_patch_constant_signature(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_shader_signature *signature)
{
    TRACE("dxbc {%p, %zu}, signature %p.\n",
            dxbc ? dxbc->code : NULL, dxbc ? dxbc->size : 0, signature);

    if (!dxbc || !dxbc->code || !signature)
        return VKD3D_ERROR_INVALID_ARGUMENT;

    return shader_parse_patch_constant_signature(dxbc->code, dxbc->size, signature);
}

struct vkd3d_shader_signature_element *vkd3d_shader_find_signature_element(
        const struct vkd3d_shader_signature *signature, const char *semantic_name,
        unsigned int semantic_index, unsigned int stream_index)
{
    struct vkd3d_shader_signature_element *e;
    unsigned int i;

    TRACE("signature %p, semantic_name %s, semantic_index %u, stream_index %u.\n",
            signature, debugstr_a(semantic_name), semantic_index, stream_index);

    if (!signature || !semantic_name || !signature->elements)
        return NULL;

    e = signature->elements;
    for (i = 0; i < signature->element_count; ++i)
    {
        if (e[i].semantic_index == semantic_index && e[i].stream_index == stream_index &&
                !ascii_strcasecmp(e[i].semantic_name, semantic_name))
            return &e[i];
    }

    return NULL;
}

void vkd3d_shader_free_shader_signature(struct vkd3d_shader_signature *signature)
{
    TRACE("signature %p.\n", signature);

    if (!signature)
        return;

    vkd3d_free(signature->elements);
    signature->elements = NULL;
}

vkd3d_shader_hash_t vkd3d_shader_hash(const struct vkd3d_shader_code *shader)
{
    vkd3d_shader_hash_t h = hash_fnv1_init();
    const uint8_t *code;
    const uint32_t *code32;
    size_t remaining;
    size_t i;

    if (!shader || !shader->code || shader->size == 0)
        return h;

    code = shader->code;
    remaining = shader->size;

    while (remaining && ((uintptr_t)code & 3))
    {
        h = hash_fnv1_iterate_u8(h, *code++);
        --remaining;
    }

    code32 = (const uint32_t *)code;
    for (i = 0; i < remaining / 4; ++i)
        h = hash_fnv1_iterate_u32(h, code32[i]);

    code = (const uint8_t *)&code32[remaining / 4];
    for (remaining &= 3; remaining; --remaining)
        h = hash_fnv1_iterate_u8(h, *code++);

    return h;
}

struct vkd3d_shader_quirk_entry
{
    vkd3d_shader_hash_t lo;
    vkd3d_shader_hash_t hi;
    vkd3d_shader_quirks_t quirks;
};

static struct vkd3d_shader_quirk_entry *vkd3d_shader_quirk_entries;
size_t vkd3d_shader_quirk_entry_count;
static uint64_t vkd3d_shader_revision = 1;

#define ENTRY(x) { #x, VKD3D_SHADER_QUIRK_ ## x }
static const struct vkd3d_shader_quirk_mapping
{
    const char *name;
    vkd3d_shader_quirks_t quirk;
} vkd3d_shader_quirk_mappings[] = {
    ENTRY(FORCE_EXPLICIT_LOD_IN_CONTROL_FLOW),
    ENTRY(FORCE_TGSM_BARRIERS),
    ENTRY(INVARIANT_POSITION),
    ENTRY(FORCE_NOCONTRACT_MATH),
    ENTRY(LIMIT_TESS_FACTORS_32),
    ENTRY(LIMIT_TESS_FACTORS_16),
    ENTRY(LIMIT_TESS_FACTORS_12),
    ENTRY(LIMIT_TESS_FACTORS_8),
    ENTRY(LIMIT_TESS_FACTORS_4),
    ENTRY(FORCE_SUBGROUP_SIZE_1),
    ENTRY(FORCE_MAX_WAVE32),
    ENTRY(FORCE_MIN16_AS_32BIT),
    ENTRY(REWRITE_GRAD_TO_BIAS),
    ENTRY(FORCE_LOOP),
    ENTRY(DESCRIPTOR_HEAP_ROBUSTNESS),
    ENTRY(DISABLE_OPTIMIZATIONS),
    ENTRY(FORCE_NOCONTRACT_MATH_VS),
    ENTRY(FORCE_DEVICE_MEMORY_BARRIER_THREAD_GROUP_COHERENCY),
    ENTRY(ASSUME_BROKEN_SUB_8x8_CUBE_MIPS),
    ENTRY(FORCE_ROBUST_PHYSICAL_CBV_LOAD_FORWARDING),
    ENTRY(AGGRESSIVE_NONUNIFORM),
    ENTRY(HOIST_DERIVATIVES),
    ENTRY(FORCE_MIN_WAVE32),
    ENTRY(PROMOTE_GROUP_TO_DEVICE_MEMORY_BARRIER),
    ENTRY(FORCE_GRAPHICS_BARRIER_BEFORE_RENDER_PASS),
    ENTRY(FIXUP_LOOP_HEADER_UNDEF_PHIS),
    ENTRY(FIXUP_RSQRT_INF_NAN),
};
#undef ENTRY

static void vkd3d_shader_update_revision(void)
{
    vkd3d_shader_hash_t quirk_hash = hash_fnv1_init();
    size_t i;

    if (!vkd3d_shader_quirk_entry_count)
    {
        vkd3d_shader_revision = 1;
        return;
    }

    for (i = 0; i < vkd3d_shader_quirk_entry_count; ++i)
    {
        quirk_hash = hash_fnv1_iterate_u64(quirk_hash, vkd3d_shader_quirk_entries[i].lo);
        quirk_hash = hash_fnv1_iterate_u64(quirk_hash, vkd3d_shader_quirk_entries[i].hi);
        quirk_hash = hash_fnv1_iterate_u64(quirk_hash, vkd3d_shader_quirk_entries[i].quirks);
    }

    vkd3d_shader_revision = quirk_hash ^ 1;
}

static void vkd3d_shader_init_quirk_table(void)
{
    struct vkd3d_shader_quirk_entry entry;
    size_t size = 0;
    char path[VKD3D_PATH_MAX];
    char line[128];
    char *trail;
    FILE *file;
    size_t i;

    if (!vkd3d_get_env_var("VKD3D_SHADER_QUIRKS", path, sizeof(path)))
        return;

    file = fopen(path, "r");
    if (!file)
    {
        INFO("Failed to open VKD3D_SHADER_QUIRKS file \"%s\".\n", path);
        return;
    }

    while (fgets(line, sizeof(line), file))
    {
        entry.quirks = 0;
        if (!vkd3d_shader_hash_range_parse_line(line, &entry.lo, &entry.hi, &trail))
            continue;

        if (*trail == '\0')
            continue;

        for (i = 0; i < ARRAY_SIZE(vkd3d_shader_quirk_mappings); ++i)
        {
            if (strcmp(trail, vkd3d_shader_quirk_mappings[i].name) == 0)
            {
                entry.quirks = vkd3d_shader_quirk_mappings[i].quirk;
                INFO("Parsed shader quirk entry: [%016"PRIx64", %016"PRIx64"] -> %s\n",
                        entry.lo, entry.hi, trail);
                break;
            }
        }

        if (i == ARRAY_SIZE(vkd3d_shader_quirk_mappings))
            continue;

        if (!vkd3d_array_reserve((void **)&vkd3d_shader_quirk_entries, &size,
                vkd3d_shader_quirk_entry_count + 1, sizeof(*vkd3d_shader_quirk_entries)))
            break;

        vkd3d_shader_quirk_entries[vkd3d_shader_quirk_entry_count++] = entry;
    }

    fclose(file);
    vkd3d_shader_update_revision();
}

static pthread_once_t vkd3d_shader_quirk_once = PTHREAD_ONCE_INIT;

vkd3d_shader_quirks_t vkd3d_shader_compile_arguments_select_quirks(
        const struct vkd3d_shader_compile_arguments *compile_args, vkd3d_shader_hash_t shader_hash)
{
    vkd3d_shader_quirks_t quirks = 0;
    size_t i;

    pthread_once(&vkd3d_shader_quirk_once, vkd3d_shader_init_quirk_table);

    for (i = 0; i < vkd3d_shader_quirk_entry_count; ++i)
    {
        if (vkd3d_shader_quirk_entries[i].lo <= shader_hash && vkd3d_shader_quirk_entries[i].hi >= shader_hash)
            quirks |= vkd3d_shader_quirk_entries[i].quirks;
    }

    if (compile_args && compile_args->quirks)
    {
        for (i = 0; i < compile_args->quirks->num_hashes; ++i)
            if (compile_args->quirks->hashes[i].shader_hash == shader_hash)
                return quirks | compile_args->quirks->hashes[i].quirks | compile_args->quirks->global_quirks;
        return quirks | compile_args->quirks->default_quirks | compile_args->quirks->global_quirks;
    }
    return quirks;
}

uint64_t vkd3d_shader_get_revision(void)
{
    pthread_once(&vkd3d_shader_quirk_once, vkd3d_shader_init_quirk_table);
    return vkd3d_shader_revision;
}

struct vkd3d_shader_stage_io_entry *vkd3d_shader_stage_io_map_append(struct vkd3d_shader_stage_io_map *map,
        const char *semantic_name, unsigned int semantic_index)
{
    struct vkd3d_shader_stage_io_entry *e;

    if (!map || !semantic_name)
        return NULL;

    if (vkd3d_shader_stage_io_map_find(map, semantic_name, semantic_index))
        return NULL;

    if (!vkd3d_array_reserve((void **)&map->entries, &map->entries_size,
            map->entry_count + 1, sizeof(*map->entries)))
        return NULL;

    e = &map->entries[map->entry_count++];
    e->semantic_name = vkd3d_strdup(semantic_name);
    e->semantic_index = semantic_index;
    return e;
}

const struct vkd3d_shader_stage_io_entry *vkd3d_shader_stage_io_map_find(const struct vkd3d_shader_stage_io_map *map,
        const char *semantic_name, unsigned int semantic_index)
{
    unsigned int i;

    if (!map || !semantic_name)
        return NULL;

    for (i = 0; i < map->entry_count; ++i)
    {
        const struct vkd3d_shader_stage_io_entry *e = &map->entries[i];
        if (e->semantic_index == semantic_index && !strcmp(e->semantic_name, semantic_name))
            return e;
    }

    return NULL;
}

void vkd3d_shader_stage_io_map_free(struct vkd3d_shader_stage_io_map *map)
{
    unsigned int i;

    if (!map)
        return;

    for (i = 0; i < map->entry_count; ++i)
        vkd3d_free((void *)map->entries[i].semantic_name);

    vkd3d_free(map->entries);
    memset(map, 0, sizeof(*map));
}

static int vkd3d_shader_parse_root_signature_for_version(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_versioned_root_signature_desc *out_desc,
        enum vkd3d_root_signature_version target_version,
        bool raw_payload,
        vkd3d_shader_hash_t *compatibility_hash)
{
    struct vkd3d_versioned_root_signature_desc desc, converted_desc;
    int ret;

    if (!dxbc || !dxbc->code || !out_desc)
        return VKD3D_ERROR_INVALID_ARGUMENT;

    if (raw_payload)
    {
        if ((ret = vkd3d_shader_parse_root_signature_raw(dxbc->code, dxbc->size, &desc, compatibility_hash)) < 0)
            return ret;
    }
    else
    {
        if ((ret = vkd3d_shader_parse_root_signature(dxbc, &desc, compatibility_hash)) < 0)
            return ret;
    }

    if (desc.version == target_version)
    {
        *out_desc = desc;
        return ret;
    }

    ret = vkd3d_shader_convert_root_signature(&converted_desc, target_version, &desc);
    vkd3d_shader_free_root_signature(&desc);
    if (ret < 0)
        return ret;

    *out_desc = converted_desc;
    return ret;
}

int vkd3d_shader_parse_root_signature_v_1_0(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_versioned_root_signature_desc *out_desc,
        vkd3d_shader_hash_t *compatibility_hash)
{
    return vkd3d_shader_parse_root_signature_for_version(dxbc, out_desc, VKD3D_ROOT_SIGNATURE_VERSION_1_0, false,
            compatibility_hash);
}

int vkd3d_shader_parse_root_signature_v_1_2(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_versioned_root_signature_desc *out_desc,
        vkd3d_shader_hash_t *compatibility_hash)
{
    return vkd3d_shader_parse_root_signature_for_version(dxbc, out_desc, VKD3D_ROOT_SIGNATURE_VERSION_1_2, false,
            compatibility_hash);
}

int vkd3d_shader_parse_root_signature_v_1_2_from_raw_payload(const struct vkd3d_shader_code *dxbc,
        struct vkd3d_versioned_root_signature_desc *out_desc,
        vkd3d_shader_hash_t *compatibility_hash)
{
    return vkd3d_shader_parse_root_signature_for_version(dxbc, out_desc, VKD3D_ROOT_SIGNATURE_VERSION_1_2, true,
            compatibility_hash);
}

vkd3d_shader_hash_t vkd3d_root_signature_v_1_2_compute_layout_compat_hash(
        const struct vkd3d_root_signature_desc2 *desc)
{
    vkd3d_shader_hash_t hash = hash_fnv1_init();
    uint32_t i;

    if (!desc)
        return hash;

    hash = hash_fnv1_iterate_u32(hash, desc->static_sampler_count);
    hash = hash_fnv1_iterate_u32(hash, desc->parameter_count);
    hash = hash_fnv1_iterate_u32(hash, desc->flags & D3D12_ROOT_SIGNATURE_FLAG_LOCAL_ROOT_SIGNATURE);

    for (i = 0; i < desc->parameter_count; i++)
    {
        hash = hash_fnv1_iterate_u32(hash, desc->parameters[i].parameter_type);
        if (desc->parameters[i].parameter_type == VKD3D_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS)
            hash = hash_fnv1_iterate_u32(hash, desc->parameters[i].constants.value_count);
        else
            hash = hash_fnv1_iterate_u32(hash, 0);
    }

    for (i = 0; i < desc->static_sampler_count; i++)
    {
        const struct vkd3d_static_sampler_desc1 *sampler = &desc->static_samplers[i];
        hash = hash_fnv1_iterate_u32(hash, sampler->flags);
        hash = hash_fnv1_iterate_u32(hash, sampler->shader_visibility);
        hash = hash_fnv1_iterate_u32(hash, sampler->max_anisotropy);
        hash = hash_fnv1_iterate_u32(hash, sampler->border_color);
        hash = hash_fnv1_iterate_u32(hash, sampler->comparison_func);
        hash = hash_fnv1_iterate_u32(hash, sampler->address_u);
        hash = hash_fnv1_iterate_u32(hash, sampler->address_v);
        hash = hash_fnv1_iterate_u32(hash, sampler->address_w);
        hash = hash_fnv1_iterate_u32(hash, sampler->filter);
        hash = hash_fnv1_iterate_f32(hash, sampler->min_lod);
        hash = hash_fnv1_iterate_f32(hash, sampler->max_lod);
        hash = hash_fnv1_iterate_f32(hash, sampler->mip_lod_bias);
    }

    return hash;
}

bool vkd3d_shader_hash_range_parse_line(char *line,
        vkd3d_shader_hash_t *lo, vkd3d_shader_hash_t *hi,
        char **trail)
{
    vkd3d_shader_hash_t lo_hash, hi_hash;
    char *end_ptr, *old_end_ptr;

    if (!line || !lo || !hi || !trail)
        return false;

    if (!isxdigit((unsigned char)*line))
        return false;

    errno = 0;
    lo_hash = strtoull(line, &end_ptr, 16);
    if (errno == ERANGE || end_ptr == line)
        return false;

    while (*end_ptr && !isalnum((unsigned char)*end_ptr))
        ++end_ptr;

    old_end_ptr = end_ptr;
    hi_hash = 0;

    if (*end_ptr)
    {
        errno = 0;
        hi_hash = strtoull(end_ptr, &end_ptr, 16);
        if (errno == ERANGE || end_ptr == old_end_ptr)
            hi_hash = 0;
    }

    while (*end_ptr && !isalpha((unsigned char)*end_ptr))
        ++end_ptr;

    if (!hi_hash)
        hi_hash = lo_hash;

    *lo = lo_hash;
    *hi = hi_hash;
    *trail = end_ptr;

    while (*end_ptr == '\n' || *end_ptr == '\r')
        *end_ptr++ = '\0';

    return true;
}

void vkd3d_shader_extract_feature_meta(struct vkd3d_shader_code *code)
{
    SpvExecutionModel execution_model = SpvExecutionModelMax;
    size_t spirv_words;
    const uint32_t *spirv;
    uint32_t tracked_sample_mask_ids[2] = {0};
    unsigned int tracked_count = 0;
    size_t offset = 5;
    uint32_t meta = 0;

    if (!code || !code->code || code->size < 20)
        return;

    spirv = code->code;
    spirv_words = code->size / sizeof(uint32_t);
    code->meta.gs_input_topology = 0;   /* prevent stale data across re-use */

    while (offset < spirv_words)
    {
        unsigned count = (spirv[offset] >> 16) & 0xffff;
        SpvOp op = spirv[offset] & 0xffff;

        if (count == 0 || offset + count > spirv_words)
            break;

        switch (op)
        {
        case SpvOpCapability:
            if (count == 2)
            {
                uint32_t cap = spirv[offset + 1];
                /* Most frequent first for branch predictor (Intel Golden Cove, Agner Fog) */
                if (cap == SpvCapabilityGroupNonUniform || cap == SpvCapabilityGroupNonUniformVote ||
                    cap == SpvCapabilityGroupNonUniformArithmetic || cap == SpvCapabilityGroupNonUniformBallot ||
                    cap == SpvCapabilityGroupNonUniformShuffle || cap == SpvCapabilityGroupNonUniformShuffleRelative ||
                    cap == SpvCapabilityGroupNonUniformClustered || cap == SpvCapabilityGroupNonUniformQuad ||
                    cap == SpvCapabilityCooperativeMatrixKHR)
                {
                    meta |= VKD3D_SHADER_META_FLAG_USES_SUBGROUP_OPERATIONS;
                }
                else if (cap == SpvCapabilityStorageUniform16 || cap == SpvCapabilityStorageUniformBufferBlock16 ||
                         cap == SpvCapabilityStorageInputOutput16 || cap == SpvCapabilityFloat16)
                    meta |= VKD3D_SHADER_META_FLAG_USES_NATIVE_16BIT_OPERATIONS;
                else if (cap == SpvCapabilityShaderViewportIndexLayerEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_SHADER_VIEWPORT_INDEX_LAYER;
                else if (cap == SpvCapabilitySparseResidency)
                    meta |= VKD3D_SHADER_META_FLAG_USES_SPARSE_RESIDENCY;
                else if (cap == SpvCapabilityFragmentFullyCoveredEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_FRAGMENT_FULLY_COVERED;
                else if (cap == SpvCapabilityInt64)
                    meta |= VKD3D_SHADER_META_FLAG_USES_INT64;
                else if (cap == SpvCapabilityStencilExportEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_STENCIL_EXPORT;
                else if (cap == SpvCapabilityFloat64)
                    meta |= VKD3D_SHADER_META_FLAG_USES_FP64;
                else if (cap == SpvCapabilityInt64Atomics)
                    meta |= VKD3D_SHADER_META_FLAG_USES_INT64_ATOMICS;
                else if (cap == SpvCapabilityInt64ImageEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_INT64_ATOMICS_IMAGE;
                else if (cap == SpvCapabilityFragmentBarycentricKHR)
                    meta |= VKD3D_SHADER_META_FLAG_USES_FRAGMENT_BARYCENTRIC;
                else if (cap == SpvCapabilitySampleRateShading)
                    meta |= VKD3D_SHADER_META_FLAG_USES_SAMPLE_RATE_SHADING;
                else if (cap == SpvCapabilityFragmentShaderPixelInterlockEXT ||
                         cap == SpvCapabilityFragmentShaderSampleInterlockEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_RASTERIZER_ORDERED_VIEWS;
                else if (cap == SpvCapabilityFloat8CooperativeMatrixEXT)
                    meta |= VKD3D_SHADER_META_FLAG_USES_COOPERATIVE_MATRIX_FP8;
            }
            break;

        case SpvOpEntryPoint:
            if (count >= 2)
                execution_model = spirv[offset + 1];
            break;

        case SpvOpExecutionMode:
            if (count == 3)
            {
                SpvExecutionMode mode = spirv[offset + 2];
                switch (mode)
                {
                case SpvExecutionModeIsolines:
                case SpvExecutionModeOutputLineStrip:
                case SpvExecutionModeOutputLinesEXT:
                    meta |= VKD3D_SHADER_META_FLAG_EMITS_LINES;
                    break;
                case SpvExecutionModeInputPoints:
                    if (execution_model == SpvExecutionModelGeometry)
                        code->meta.gs_input_topology = (uint8_t)VK_PRIMITIVE_TOPOLOGY_POINT_LIST;
                    break;
                case SpvExecutionModeInputLines:
                    if (execution_model == SpvExecutionModelGeometry)
                        code->meta.gs_input_topology = (uint8_t)VK_PRIMITIVE_TOPOLOGY_LINE_LIST;
                    break;
                case SpvExecutionModeInputLinesAdjacency:
                    if (execution_model == SpvExecutionModelGeometry)
                        code->meta.gs_input_topology = (uint8_t)VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY;
                    break;
                case SpvExecutionModeTriangles:
                    if (execution_model == SpvExecutionModelGeometry)
                        code->meta.gs_input_topology = (uint8_t)VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
                    else
                        meta |= VKD3D_SHADER_META_FLAG_EMITS_TRIANGLES;
                    break;
                case SpvExecutionModeInputTrianglesAdjacency:
                    if (execution_model == SpvExecutionModelGeometry)
                        code->meta.gs_input_topology = (uint8_t)VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY;
                    break;
                case SpvExecutionModeQuads:
                case SpvExecutionModeOutputTriangleStrip:
                case SpvExecutionModeOutputTrianglesEXT:
                    meta |= VKD3D_SHADER_META_FLAG_EMITS_TRIANGLES;
                    break;
                case SpvExecutionModeDepthGreater:
                case SpvExecutionModeDepthLess:
                case SpvExecutionModeDepthReplacing:
                case SpvExecutionModeDepthUnchanged:
                case SpvExecutionModeStencilRefReplacingEXT:
                    meta |= VKD3D_SHADER_META_FLAG_USES_DEPTH_STENCIL_WRITE;
                    break;
                case SpvExecutionModePointMode:
                    meta |= VKD3D_SHADER_META_FLAG_POINT_MODE_TESSELLATION;
                    break;
                default:
                    break;
                }
            }
            break;

        case SpvOpDecorate:
        case SpvOpMemberDecorate:
        {
            unsigned delta = (op == SpvOpMemberDecorate) ? 1 : 0;
            if (count == 4 + delta && spirv[offset + delta + 2] == SpvDecorationBuiltIn &&
                spirv[offset + delta + 3] == SpvBuiltInSampleMask)
            {
                if (tracked_count < ARRAY_SIZE(tracked_sample_mask_ids))
                    tracked_sample_mask_ids[tracked_count++] = spirv[offset + 1];
            }
            break;
        }

        case SpvOpVariable:
            if (count >= 4 && spirv[offset + 3] == SpvStorageClassOutput && tracked_count)
            {
                uint32_t var_id = spirv[offset + 2];
                if (tracked_sample_mask_ids[0] == var_id ||
                    (tracked_count > 1 && tracked_sample_mask_ids[1] == var_id))
                    meta |= VKD3D_SHADER_META_FLAG_EXPORTS_SAMPLE_MASK;
            }
            break;

        case SpvOpFunction:
            goto done;

        default:
            break;
        }

        offset += count;
    }
done:
    code->meta.flags |= meta;
}
