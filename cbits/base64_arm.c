#include <stddef.h>
#include <stdint.h>

#if defined(__aarch64__)

#include <arm_neon.h>

static const uint8_t b64_alphabet[64] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/*
 * Encode 'l' input bytes at 'src' into ((l+2)/3)*4 ASCII chars at 'dst'.
 *
 * NEON kernel processes 12 input bytes per iteration:
 *   - vld1q_u8 loads 16 bytes (we use the first 12; reading 4 ahead is
 *     safe as long as l - i >= 16)
 *   - vqtbl1q_u8 with a shuffle mask gathers each 4-byte output lane as
 *     [b1, b0, b2, b1], the order that lets a single shift+mask extract
 *     each 6-bit index
 *   - 4 vshrq_n_u32 + vandq_u32 pull out indices i0..i3 (one per lane
 *     byte); see comments below for the bit math
 *   - vqtbl4q_u8 looks up each index in the 64-byte alphabet
 *   - vst1q_u8 stores 16 output chars
 *
 * A scalar loop finishes any full triplet that didn't make the NEON
 * cut-off, and a final branch emits the 0/1/2-byte padded tail.
 */
void base64_encode_arm(const uint8_t *src, uint8_t *dst, size_t l) {
    uint8x16x4_t lut;
    lut.val[0] = vld1q_u8(b64_alphabet);
    lut.val[1] = vld1q_u8(b64_alphabet + 16);
    lut.val[2] = vld1q_u8(b64_alphabet + 32);
    lut.val[3] = vld1q_u8(b64_alphabet + 48);

    /* For each 4-byte lane of output of vqtbl1q_u8, we want
     * [b1, b0, b2, b1] in memory order — viewed as a little-endian u32
     * lane that is (b1) | (b0 << 8) | (b2 << 16) | (b1 << 24).         */
    static const uint8_t shuf_enc[16] = {
        1, 0, 2, 1,
        4, 3, 5, 4,
        7, 6, 8, 7,
       10, 9,11,10,
    };
    uint8x16_t shuf = vld1q_u8(shuf_enc);

    size_t i = 0, o = 0;
    while (i + 16 <= l) {
        uint8x16_t in       = vld1q_u8(src + i);
        uint8x16_t shuffled = vqtbl1q_u8(in, shuf);
        uint32x4_t lane     = vreinterpretq_u32_u8(shuffled);
        uint32x4_t mask6    = vdupq_n_u32(0x3F);

        /* lane (LE) = b1 | (b0 << 8) | (b2 << 16) | (b1 << 24)
         *  i0 (top 6 of b0)          = (lane >> 10) & 0x3F
         *  i1 (lo 2 of b0|hi 4 of b1)= (lane >>  4) & 0x3F
         *  i2 (lo 4 of b1|hi 2 of b2)= (lane >> 22) & 0x3F  [uses b1 copy at byte 3]
         *  i3 (lo 6 of b2)           = (lane >> 16) & 0x3F          */
        uint32x4_t i0 = vandq_u32(vshrq_n_u32(lane, 10), mask6);
        uint32x4_t i1 = vandq_u32(vshrq_n_u32(lane,  4), mask6);
        uint32x4_t i2 = vandq_u32(vshrq_n_u32(lane, 22), mask6);
        uint32x4_t i3 = vandq_u32(vshrq_n_u32(lane, 16), mask6);

        /* assemble per-lane u32 = i0 | (i1 << 8) | (i2 << 16) | (i3 << 24) */
        uint32x4_t idx_u32 = vorrq_u32(
            vorrq_u32(i0, vshlq_n_u32(i1, 8)),
            vorrq_u32(vshlq_n_u32(i2, 16), vshlq_n_u32(i3, 24)));

        uint8x16_t indices = vreinterpretq_u8_u32(idx_u32);
        uint8x16_t chars   = vqtbl4q_u8(lut, indices);
        vst1q_u8(dst + o, chars);

        i += 12;
        o += 16;
    }

    /* scalar tail: full triplets */
    for (; i + 3 <= l; i += 3, o += 4) {
        uint32_t v = ((uint32_t)src[i] << 16)
                   | ((uint32_t)src[i + 1] << 8)
                   |  (uint32_t)src[i + 2];
        dst[o]     = b64_alphabet[(v >> 18) & 0x3F];
        dst[o + 1] = b64_alphabet[(v >> 12) & 0x3F];
        dst[o + 2] = b64_alphabet[(v >>  6) & 0x3F];
        dst[o + 3] = b64_alphabet[ v        & 0x3F];
    }

    /* 1- or 2-byte padded tail */
    if (i + 1 == l) {
        uint8_t b = src[i];
        dst[o]     = b64_alphabet[(b >> 2) & 0x3F];
        dst[o + 1] = b64_alphabet[(b & 0x03) << 4];
        dst[o + 2] = '=';
        dst[o + 3] = '=';
    } else if (i + 2 == l) {
        uint8_t b0 = src[i];
        uint8_t b1 = src[i + 1];
        dst[o]     = b64_alphabet[(b0 >> 2) & 0x3F];
        dst[o + 1] = b64_alphabet[((b0 & 0x03) << 4) | (b1 >> 4)];
        dst[o + 2] = b64_alphabet[(b1 & 0x0F) << 2];
        dst[o + 3] = '=';
    }
}

/*
 * Convert 16 ASCII base64 chars to 6-bit values in 'val'.
 * Each lane of 'bad' is 0xff if the corresponding input is not a
 * valid base64 char ('A'..'Z', 'a'..'z', '0'..'9', '+', '/'), else 0.
 * '=' is treated as invalid here; the caller handles padding.
 */
static inline void ascii_to_b64(uint8x16_t c,
                                uint8x16_t *val,
                                uint8x16_t *bad) {
    uint8x16_t is_upper = vandq_u8(vcgeq_u8(c, vdupq_n_u8('A')),
                                    vcleq_u8(c, vdupq_n_u8('Z')));
    uint8x16_t is_lower = vandq_u8(vcgeq_u8(c, vdupq_n_u8('a')),
                                    vcleq_u8(c, vdupq_n_u8('z')));
    uint8x16_t is_digit = vandq_u8(vcgeq_u8(c, vdupq_n_u8('0')),
                                    vcleq_u8(c, vdupq_n_u8('9')));
    uint8x16_t is_plus  = vceqq_u8(c, vdupq_n_u8('+'));
    uint8x16_t is_slash = vceqq_u8(c, vdupq_n_u8('/'));

    /* Per-lane additive offset that takes c to its 6-bit value:
     *   'A'..'Z':  +(-65) = 0xBF mod 256   ('A' + 0xBF = 0)
     *   'a'..'z':  +(-71) = 0xB9
     *   '0'..'9':  +4
     *   '+':       +19
     *   '/':       +16
     * Invalid lanes get +0; 'bad' flags them.                          */
    uint8x16_t add = vorrq_u8(
        vandq_u8(is_upper, vdupq_n_u8((uint8_t)(0u - 65))),
        vorrq_u8(
            vandq_u8(is_lower, vdupq_n_u8((uint8_t)(0u - 71))),
            vorrq_u8(
                vandq_u8(is_digit, vdupq_n_u8(4)),
                vorrq_u8(
                    vandq_u8(is_plus,  vdupq_n_u8(19)),
                    vandq_u8(is_slash, vdupq_n_u8(16))))));

    *val = vaddq_u8(c, add);

    uint8x16_t any_valid = vorrq_u8(is_upper,
                            vorrq_u8(is_lower,
                              vorrq_u8(is_digit,
                                vorrq_u8(is_plus, is_slash))));
    *bad = vmvnq_u8(any_valid);
}

static inline uint8_t scalar_b64(uint8_t c) {
    if (c >= 'A' && c <= 'Z') return (uint8_t)(c - 'A');
    if (c >= 'a' && c <= 'z') return (uint8_t)(c - 'a' + 26);
    if (c >= '0' && c <= '9') return (uint8_t)(c - '0' + 52);
    if (c == '+') return 62;
    if (c == '/') return 63;
    return 0x80; /* invalid sentinel */
}

/*
 * Decode 'inlen' ASCII base64 chars at 'src' into 'outlen' bytes at
 * 'dst'.  Returns 1 on success, 0 on any decoding error: malformed
 * length, malformed padding, invalid char in body, or invalid char /
 * non-zero non-data bits in the padded final quartet (RFC 4648 §3.5).
 *
 * Caller must allocate 'outlen' bytes at 'dst' and pass the correct
 * outlen for the given inlen and padding; mismatch returns 0 with
 * 'dst' unspecified.
 *
 * Body NEON kernel processes 16 input chars (= 4 quartets) per
 * iteration:
 *   - vld1q_u8 loads 16 chars
 *   - ascii_to_b64 validates each lane and yields 6-bit values
 *   - per u32x4 lane: build the 24-bit packed value V = (v0 << 18) |
 *     (v1 << 12) | (v2 << 6) | v3, whose bytes in LE are [V_low,
 *     V_mid, V_high, 0]
 *   - vqtbl1q_u8 reshuffles those bytes into [V_high, V_mid, V_low]
 *     per triplet, yielding 12 output bytes at the bottom of the
 *     output vector
 *   - vst1q_u8 stores 16 bytes (writing 12 valid + 4 spurious; the
 *     loop bound 'o + 16 <= body_outlen' keeps the overrun within
 *     the allocated buffer, and the spurious bytes get clobbered by
 *     the next iteration or by the scalar tail / final quartet)
 *
 * A scalar tail finishes any body quartets that didn't make the
 * NEON cut-off, then the padded final quartet is decoded explicitly.
 */
int base64_decode_arm(const uint8_t *src, uint8_t *dst,
                      size_t inlen, size_t outlen) {
    if (inlen == 0) return outlen == 0;
    if (inlen & 0x3) return 0;

    uint8_t c_pre = src[inlen - 2];
    uint8_t c_end = src[inlen - 1];
    size_t pad = 0;
    if (c_end == '=') {
        if (c_pre == '=') pad = 2;
        else              pad = 1;
    } else if (c_pre == '=') {
        return 0; /* '=' at offset -2 only is malformed */
    }

    size_t nfull = inlen >> 2;
    if (outlen != nfull * 3 - pad) return 0;

    size_t body_chars  = (pad > 0) ? (inlen - 4) : inlen;
    size_t body_outlen = (body_chars >> 2) * 3;

    uint8x16_t bad_acc = vdupq_n_u8(0);

    static const uint8_t pack_shuf[16] = {
         2, 1, 0,
         6, 5, 4,
        10, 9, 8,
        14,13,12,
         0xFF, 0xFF, 0xFF, 0xFF
    };
    uint8x16_t pshuf = vld1q_u8(pack_shuf);

    size_t i = 0, o = 0;
    while (o + 16 <= body_outlen) {
        uint8x16_t c = vld1q_u8(src + i);
        uint8x16_t val, this_bad;
        ascii_to_b64(c, &val, &this_bad);
        bad_acc = vorrq_u8(bad_acc, this_bad);

        uint32x4_t v32   = vreinterpretq_u32_u8(val);
        uint32x4_t mask8 = vdupq_n_u32(0xFF);

        uint32x4_t p0 = vshlq_n_u32(vandq_u32(v32, mask8), 18);
        uint32x4_t p1 = vshlq_n_u32(
            vandq_u32(vshrq_n_u32(v32,  8), mask8), 12);
        uint32x4_t p2 = vshlq_n_u32(
            vandq_u32(vshrq_n_u32(v32, 16), mask8),  6);
        uint32x4_t p3 = vshrq_n_u32(v32, 24);

        uint32x4_t V       = vorrq_u32(vorrq_u32(p0, p1),
                                       vorrq_u32(p2, p3));
        uint8x16_t V_bytes = vreinterpretq_u8_u32(V);
        uint8x16_t packed  = vqtbl1q_u8(V_bytes, pshuf);

        vst1q_u8(dst + o, packed); /* 12 valid bytes + 4 spurious */

        i += 16;
        o += 12;
    }

    uint8_t tail_bad = 0;

    /* scalar body tail (full quartets, no '=') */
    while (o + 3 <= body_outlen) {
        uint8_t v0 = scalar_b64(src[i]);
        uint8_t v1 = scalar_b64(src[i + 1]);
        uint8_t v2 = scalar_b64(src[i + 2]);
        uint8_t v3 = scalar_b64(src[i + 3]);
        tail_bad |= (v0 | v1 | v2 | v3) & 0x80;
        dst[o]     = (uint8_t)((v0 << 2)         | (v1 >> 4));
        dst[o + 1] = (uint8_t)(((v1 & 0x0F) << 4) | (v2 >> 2));
        dst[o + 2] = (uint8_t)(((v2 & 0x03) << 6) | (v3 & 0x3F));
        i += 4;
        o += 3;
    }

    /* padded final quartet */
    if (pad > 0) {
        uint8_t v0 = scalar_b64(src[i]);
        uint8_t v1 = scalar_b64(src[i + 1]);
        if ((v0 | v1) & 0x80) return 0;

        if (pad == 2) {
            /* "XX==" -> 1 output byte; bottom 4 bits of v1 must be 0 */
            if (v1 & 0x0F) return 0;
            dst[o] = (uint8_t)((v0 << 2) | (v1 >> 4));
        } else {
            /* "XXX=" -> 2 output bytes; bottom 2 bits of v2 must be 0 */
            uint8_t v2 = scalar_b64(src[i + 2]);
            if (v2 & 0x80)  return 0;
            if (v2 & 0x03) return 0;
            dst[o]     = (uint8_t)((v0 << 2)        | (v1 >> 4));
            dst[o + 1] = (uint8_t)(((v1 & 0x0F) << 4) | (v2 >> 2));
        }
    }

    return (vmaxvq_u8(bad_acc) == 0) && (tail_bad == 0);
}

int base64_arm_available(void) {
    return 1;
}

#else

/* stubs for non-aarch64 builds; never reached because dispatch is
 * gated on 'base64_arm_available' returning 0                     */

void base64_encode_arm(const uint8_t *src, uint8_t *dst, size_t l) {
    (void)src; (void)dst; (void)l;
}

int base64_decode_arm(const uint8_t *src, uint8_t *dst,
                      size_t inlen, size_t outlen) {
    (void)src; (void)dst; (void)inlen; (void)outlen;
    return 0;
}

int base64_arm_available(void) {
    return 0;
}

#endif
