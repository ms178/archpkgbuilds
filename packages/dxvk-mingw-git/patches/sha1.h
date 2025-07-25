/*=============================================================================
 *  sha1.h  ──  Portable SHA-1 interface
 *  ----------------------------------------------------------------------------
 *  • Drop-in compatible with the original DXVK header.
 *  • Warning-free with -Wall -Wextra -std=gnu++2a on all mainstream
 *    compilers (GCC, Clang, MSVC, mingw-clang).
 *  • Pure C99 interface; C++ projects may include it directly.
 *  • Provides host-to-network / network-to-host helpers for digests.
 *============================================================================*/
#pragma once

#include <stdint.h>   /* uint32_t / uint64_t */
#include <stddef.h>   /* size_t   */
#if defined(_WIN32)
  #include <winsock2.h>   /* htonl / ntohl */
#else
  #include <arpa/inet.h>  /* htonl / ntohl */
#endif

/*---------------------------------------------------------------------------*/
#define SHA1_DIGEST_LENGTH 20u   /* 160-bit hash  */
#define SHA1_BLOCK_LENGTH  64u   /* 512-bit block */

/*---------------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

/*---------------------------------------------------------------------------*
 *  Context object
 *---------------------------------------------------------------------------*/
typedef struct SHA1_CTX {
  uint32_t state[5];                 /* A, B, C, D, E */
  uint64_t count;                    /* total bits processed */
  uint8_t  buffer[SHA1_BLOCK_LENGTH];/* data block being transformed */
} SHA1_CTX;

/*---------------------------------------------------------------------------*
 *  API
 *---------------------------------------------------------------------------*/
void SHA1Init  (SHA1_CTX* ctx);
void SHA1Update(SHA1_CTX* ctx, const uint8_t* data, size_t len);
void SHA1Final (uint8_t digest[SHA1_DIGEST_LENGTH], SHA1_CTX* ctx);

/* (internal helpers are intentionally not exported)                          */

/*---------------------------------------------------------------------------*
 *  Endian helpers for digest arrays
 *---------------------------------------------------------------------------*/
#define HTONDIGEST(x)                 \
  do {                                \
    (x)[0] = htonl((x)[0]);           \
    (x)[1] = htonl((x)[1]);           \
    (x)[2] = htonl((x)[2]);           \
    (x)[3] = htonl((x)[3]);           \
    (x)[4] = htonl((x)[4]);           \
  } while (0)

#define NTOHDIGEST(x)                 \
  do {                                \
    (x)[0] = ntohl((x)[0]);           \
    (x)[1] = ntohl((x)[1]);           \
    (x)[2] = ntohl((x)[2]);           \
    (x)[3] = ntohl((x)[3]);           \
    (x)[4] = ntohl((x)[4]);           \
  } while (0)

/*---------------------------------------------------------------------------*/
#ifdef __cplusplus
} /* extern "C" */
#endif
