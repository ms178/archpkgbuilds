/*=============================================================================
 *  sha1.c  ──  Portable SHA-1 + optional Intel SHA-NI acceleration
 *============================================================================*/
#include "sha1.h"

#include <string.h>        /* memcpy / memset */
#if defined(_MSC_VER)
#include <intrin.h>
#elif defined(__x86_64__) || defined(__i386__)
#include <cpuid.h>
#endif

/*──────────────────── endianness ───────────────────────────────────────*/
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define SHA1_LITTLE_ENDIAN 1
#elif defined(_WIN32) || defined(__LITTLE_ENDIAN__) || defined(__x86_64__) || defined(__i386__)
#define SHA1_LITTLE_ENDIAN 1
#else
#define SHA1_LITTLE_ENDIAN 0
#endif

#if SHA1_LITTLE_ENDIAN
#if defined(__GNUC__) || defined(__clang__)
#define BSWAP32(x) __builtin_bswap32(x)
#elif defined(_MSC_VER)
#define BSWAP32(x) _byteswap_ulong(x)
#else
static inline uint32_t BSWAP32(uint32_t v) {
    return (v>>24)|((v>>8)&0x0000FF00)|((v<<8)&0x00FF0000)|(v<<24);
}
#endif
#endif

/*──────────────────── force-inline helper ──────────────────────────────*/
#if defined(_MSC_VER)
#define FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define FORCE_INLINE inline __attribute__((always_inline))
#else
#define FORCE_INLINE inline
#endif

/*──────────────────── rotate-left --------------------------------------*/
static FORCE_INLINE uint32_t rotl32(uint32_t v, unsigned n) {
    #if defined(_MSC_VER)
    return _rotl(v, n);
    #else
    return (v << n) | (v >> (32u - n));
    #endif
}

/*============================================================================
 * 1.  Portable C reference transformer
 *==========================================================================*/
#define BLK0(i) ( w[i] =                                                   \
(uint32_t)buf[4*i+0]<<24 | (uint32_t)buf[4*i+1]<<16 |                    \
(uint32_t)buf[4*i+2]<< 8 | (uint32_t)buf[4*i+3] )

#define BLK(i) ( w[(i)&15] = rotl32(w[((i)+13)&15] ^                       \
w[((i)+8)&15] ^                       \
w[((i)+2)&15] ^                       \
w[(i)&15]       , 1) )

#define R0(a,b,c,d,e,i) e+=(((b)&((c)^(d)))^(d))+BLK0(i)+0x5A827999u+rotl32(a,5); b=rotl32(b,30)
#define R1(a,b,c,d,e,i) e+=(((b)&((c)^(d)))^(d))+BLK(i) +0x5A827999u+rotl32(a,5); b=rotl32(b,30)
#define R2(a,b,c,d,e,i) e+=( (b)^(c)^(d)          )+BLK(i)+0x6ED9EBA1u+rotl32(a,5); b=rotl32(b,30)
#define R3(a,b,c,d,e,i) e+=(((b)|(c))&(d) | (b)&(c))+BLK(i)+0x8F1BBCDCu+rotl32(a,5); b=rotl32(b,30)
#define R4(a,b,c,d,e,i) e+=( (b)^(c)^(d)          )+BLK(i)+0xCA62C1D6u+rotl32(a,5); b=rotl32(b,30)

static void sha1_transform_c(uint32_t s[5], const uint8_t buf[64])
{
    uint32_t w[16];
    uint32_t a=s[0], b=s[1], c=s[2], d=s[3], e=s[4];

    /* 0-19 */
    R0(a,b,c,d,e,0);  R0(e,a,b,c,d,1);  R0(d,e,a,b,c,2);  R0(c,d,e,a,b,3);
    R0(b,c,d,e,a,4);  R0(a,b,c,d,e,5);  R0(e,a,b,c,d,6);  R0(d,e,a,b,c,7);
    R0(c,d,e,a,b,8);  R0(b,c,d,e,a,9);  R0(a,b,c,d,e,10); R0(e,a,b,c,d,11);
    R0(d,e,a,b,c,12); R0(c,d,e,a,b,13); R0(b,c,d,e,a,14); R0(a,b,c,d,e,15);
    R1(e,a,b,c,d,16); R1(d,e,a,b,c,17); R1(c,d,e,a,b,18); R1(b,c,d,e,a,19);

    /* 20-39 */
    R2(a,b,c,d,e,20); R2(e,a,b,c,d,21); R2(d,e,a,b,c,22); R2(c,d,e,a,b,23);
    R2(b,c,d,e,a,24); R2(a,b,c,d,e,25); R2(e,a,b,c,d,26); R2(d,e,a,b,c,27);
    R2(c,d,e,a,b,28); R2(b,c,d,e,a,29); R2(a,b,c,d,e,30); R2(e,a,b,c,d,31);
    R2(d,e,a,b,c,32); R2(c,d,e,a,b,33); R2(b,c,d,e,a,34); R2(a,b,c,d,e,35);
    R2(e,a,b,c,d,36); R2(d,e,a,b,c,37); R2(c,d,e,a,b,38); R2(b,c,d,e,a,39);

    /* 40-59 */
    R3(a,b,c,d,e,40); R3(e,a,b,c,d,41); R3(d,e,a,b,c,42); R3(c,d,e,a,b,43);
    R3(b,c,d,e,a,44); R3(a,b,c,d,e,45); R3(e,a,b,c,d,46); R3(d,e,a,b,c,47);
    R3(c,d,e,a,b,48); R3(b,c,d,e,a,49); R3(a,b,c,d,e,50); R3(e,a,b,c,d,51);
    R3(d,e,a,b,c,52); R3(c,d,e,a,b,53); R3(b,c,d,e,a,54); R3(a,b,c,d,e,55);
    R3(e,a,b,c,d,56); R3(d,e,a,b,c,57); R3(c,d,e,a,b,58); R3(b,c,d,e,a,59);

    /* 60-79 */
    R4(a,b,c,d,e,60); R4(e,a,b,c,d,61); R4(d,e,a,b,c,62); R4(c,d,e,a,b,63);
    R4(b,c,d,e,a,64); R4(a,b,c,d,e,65); R4(e,a,b,c,d,66); R4(d,e,a,b,c,67);
    R4(c,d,e,a,b,68); R4(b,c,d,e,a,69); R4(a,b,c,d,e,70); R4(e,a,b,c,d,71);
    R4(d,e,a,b,c,72); R4(c,d,e,a,b,73); R4(b,c,d,e,a,74); R4(a,b,c,d,e,75);
    R4(e,a,b,c,d,76); R4(d,e,a,b,c,77); R4(c,d,e,a,b,78); R4(b,c,d,e,a,79);

    s[0]+=a; s[1]+=b; s[2]+=c; s[3]+=d; s[4]+=e;
}

#undef BLK0
#undef BLK
#undef R0
#undef R1
#undef R2
#undef R3
#undef R4

/*============================================================================
 * 2.  SHA-NI transformer (x86-64, Clang/GCC, little-endian)
 *==========================================================================*/
#if defined(__SHA__) && defined(__x86_64__) && SHA1_LITTLE_ENDIAN && !defined(__SANITIZE_MEMORY__)
#include <immintrin.h>

/* byte-swap to BE */
static FORCE_INLINE __m128i load_be128(const void* p) {
    const __m128i shuf = _mm_set_epi64x(0x0001020304050607ULL,
                                        0x08090a0b0c0d0e0fULL);
    return _mm_shuffle_epi8(_mm_loadu_si128((const __m128i*)p), shuf);
}

#define SHA1_RNDS4(abcd,e,msg,imm)                 \
do {                                             \
    (e) = _mm_add_epi32((e), (msg));               \
    (abcd) = _mm_sha1rnds4_epu32((abcd),(e), imm); \
    (e) = _mm_sha1nexte_epu32((e), (msg));         \
} while (0)

static void sha1_transform_sha(uint32_t st[5], const uint8_t data[64])
{
    __m128i abcd = load_be128(st);
    abcd = _mm_shuffle_epi32(abcd, 0x1B);          /* DCBA -> ABCD */
    __m128i e    = _mm_cvtsi32_si128((int)st[4]);

    __m128i msg0 = load_be128(data+ 0);
    __m128i msg1 = load_be128(data+16);
    __m128i msg2 = load_be128(data+32);
    __m128i msg3 = load_be128(data+48);

    __m128i abcd0 = abcd, e0 = e;

    /* rounds  0-15 (imm 0) */
    SHA1_RNDS4(abcd,e,msg0,0); SHA1_RNDS4(abcd,e,msg1,0);
    SHA1_RNDS4(abcd,e,msg2,0); SHA1_RNDS4(abcd,e,msg3,0);

    msg0 = _mm_sha1msg1_epu32(msg0,msg1);
    msg1 = _mm_sha1msg1_epu32(msg1,msg2);
    msg2 = _mm_sha1msg1_epu32(msg2,msg3);
    msg3 = _mm_sha1msg2_epu32(msg0,msg3);

    /* rounds 16-31 (imm 1) */
    SHA1_RNDS4(abcd,e,msg0,1); SHA1_RNDS4(abcd,e,msg1,1);
    SHA1_RNDS4(abcd,e,msg2,1); SHA1_RNDS4(abcd,e,msg3,1);

    msg0 = _mm_sha1msg2_epu32(msg1,msg0);
    msg1 = _mm_sha1msg2_epu32(msg2,msg1);
    msg2 = _mm_sha1msg2_epu32(msg3,msg2);
    msg3 = _mm_sha1msg2_epu32(msg0,msg3);

    /* rounds 32-47 (imm 2) */
    SHA1_RNDS4(abcd,e,msg0,2); SHA1_RNDS4(abcd,e,msg1,2);
    SHA1_RNDS4(abcd,e,msg2,2); SHA1_RNDS4(abcd,e,msg3,2);

    msg0 = _mm_sha1msg2_epu32(msg1,msg0);
    msg1 = _mm_sha1msg2_epu32(msg2,msg1);
    msg2 = _mm_sha1msg2_epu32(msg3,msg2);
    msg3 = _mm_sha1msg2_epu32(msg0,msg3);

    /* rounds 48-63 (imm 3) */
    SHA1_RNDS4(abcd,e,msg0,3); SHA1_RNDS4(abcd,e,msg1,3);
    SHA1_RNDS4(abcd,e,msg2,3); SHA1_RNDS4(abcd,e,msg3,3);

    /* rounds 64-79: two more msg generations */
    msg0 = _mm_sha1msg2_epu32(msg1,msg0);
    msg1 = _mm_sha1msg2_epu32(msg2,msg1);
    msg2 = _mm_sha1msg2_epu32(msg3,msg2);
    msg3 = _mm_sha1msg2_epu32(msg0,msg3);

    SHA1_RNDS4(abcd,e,msg0,3); SHA1_RNDS4(abcd,e,msg1,3);
    SHA1_RNDS4(abcd,e,msg2,3); SHA1_RNDS4(abcd,e,msg3,3);

    abcd = _mm_add_epi32(abcd, abcd0);
    e    = _mm_sha1nexte_epu32(e, e0);

    abcd = _mm_shuffle_epi32(abcd, 0x1B);
    _mm_storeu_si128((__m128i*)st, abcd);
    st[4] = _mm_extract_epi32(e, 3);
}
#endif /* SHA-NI */

/*============================================================================
 * 3.  Dispatch helper
 *==========================================================================*/
typedef void (*sha1_fn)(uint32_t[5], const uint8_t[64]);
static sha1_fn g_impl = NULL;

static sha1_fn detect_impl(void)
{
    #if defined(__SHA__) && defined(__x86_64__) && SHA1_LITTLE_ENDIAN && !defined(__SANITIZE_MEMORY__)
    unsigned int eax, ebx, ecx, edx;
    #if defined(_MSC_VER)
    int info[4];
    __cpuid(info, 0);
    if (info[0] >= 7) { __cpuidex(info, 7, 0); ebx = info[1]; }
    else ebx = 0;
    #else
    if (!__get_cpuid_max(0, NULL)) ebx = 0;
    else {
        __cpuid_count(7, 0, eax, ebx, ecx, edx);
    }
    #endif
    if (ebx & (1u << 29)) return &sha1_transform_sha;
    #endif
    return &sha1_transform_c;
}

/*============================================================================
 * 4.  Public interface
 *==========================================================================*/
void SHA1Init(SHA1_CTX* c)
{
    if (!g_impl) g_impl = detect_impl();
    c->count = 0;
    c->state[0]=0x67452301u; c->state[1]=0xEFCDAB89u;
    c->state[2]=0x98BADCFEu; c->state[3]=0x10325476u;
    c->state[4]=0xC3D2E1F0u;
}

void SHA1Update(SHA1_CTX* c, const uint8_t* data, size_t len)
{
    size_t used = (size_t)((c->count >> 3) & 63u);
    c->count += (uint64_t)len << 3;

    if (used) {
        size_t avail = 64u - used;
        if (len < avail) { memcpy(c->buffer + used, data, len); return; }
        memcpy(c->buffer + used, data, avail);
        g_impl(c->state, c->buffer);
        data += avail; len -= avail; used = 0;
    }

    while (len >= 64u) {
        g_impl(c->state, data);
        data += 64u; len -= 64u;
    }
    if (len) memcpy(c->buffer, data, len);
}

static void sha1_pad(SHA1_CTX* c)
{
    uint8_t lenBuf[8];
    for (int i=7;i>=0;--i) { lenBuf[i]=(uint8_t)(c->count>>(8*(7-i))); }

    uint8_t one = 0x80u;
    SHA1Update(c, &one, 1);
    uint8_t zero = 0;
    while ((c->count & 0x1F8u) != 448u) SHA1Update(c, &zero, 1);
    SHA1Update(c, lenBuf, 8);
}

void SHA1Final(uint8_t digest[SHA1_DIGEST_LENGTH], SHA1_CTX* c)
{
    sha1_pad(c);
    for (int i=0;i<5;++i) {
        uint32_t v = c->state[i];
        #if SHA1_LITTLE_ENDIAN
        v = BSWAP32(v);
        #endif
        memcpy(digest + i*4, &v, 4);
    }
    memset(c, 0, sizeof(*c));
}
