/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include "flash-drv.h"

/* All three ops map udb's (seg, off) onto an absolute flash address in this
 * domain's alias and forward to the HAL flash driver. */

static int flashdrv_read(void *ctx, int seg, uint32_t off, void *buf, uint32_t len) {
    udb_flash_ctx_t *c = ctx;
    if (off + len > flash_page_size(c->dev)) {
        return -1;
    }
    return flash_read(c->dev, c->seg_addr[seg] + off, buf, len);
}

/* udb always hands us prog_size-aligned offsets and lengths; the HAL also
 * enforces program-unit alignment. */
static int flashdrv_write(void *ctx, int seg, uint32_t off, const void *data, uint32_t len) {
    udb_flash_ctx_t *c = ctx;
    if (off + len > flash_page_size(c->dev)) {
        return -1;
    }
    return flash_program(c->dev, c->seg_addr[seg] + off, data, len);
}

/* reset: page-erase the segment. A segment is exactly one erase page here. */
static int flashdrv_reset(void *ctx, int seg) {
    udb_flash_ctx_t *c = ctx;
    return flash_erase_page(c->dev, c->seg_addr[seg]);
}

void flashdrv_build(udb_driver_t *drv, udb_flash_ctx_t *ctx) {
    drv->ctx       = ctx;
    drv->prog_size = flash_prog_size(ctx->dev);
    drv->seg_size  = flash_page_size(ctx->dev);
    drv->read      = flashdrv_read;
    drv->write     = flashdrv_write;
    drv->reset     = flashdrv_reset;
}
