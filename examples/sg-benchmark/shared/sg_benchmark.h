/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#ifndef SG_BENCHMARK_H
#define SG_BENCHMARK_H

#include <stdint.h>

/* Input to nsc_sg_call, bundled into one struct -- the same reason
 * examples/microhastee/S/main.c's `sg` takes one struct pointer
 * (reinterpreting a MicroHs BFILE) instead of a separate pointer+length
 * pair: cmse_nonsecure_entry only allows arguments passed in r0-r3, and a
 * separate (buf, len, out_buf, out_capacity, out_len) signature needs a
 * fifth, stack-passed argument, which the compiler rejects for NSC entry
 * points. */
typedef struct {
    const uint8_t *buf;
    int            len;
} sg_input_t;

#endif /* SG_BENCHMARK_H */
