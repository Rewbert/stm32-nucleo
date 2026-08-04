/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#ifndef FLASH_DRV_H
#define FLASH_DRV_H

#include <stdint.h>

#include "drivers/flash.h"
#include "udb.h"

/*
 * On-chip NOR flash backend for udb.
 *
 * The program/erase/read logic lives in the HAL flash driver
 * (firmware/drivers/.../flash.c), described once per MCU. This adapter only maps
 * udb's (segment, offset) addressing onto absolute flash addresses and forwards
 * to flash_read()/flash_program()/flash_erase_page(); prog_size and seg_size are
 * read from the device. Each udb segment is exactly one erase page placed
 * directly in memory-mapped flash.
 *
 * The per-MCU files (stm32l5-drv.c / stm32u5-drv.c) fill in the flash_dev_t
 * (from board_flash()) and the two segment addresses, then hand them here.
 */
typedef struct {
    flash_dev_t *dev;         /* HAL flash device for this domain (board_flash()) */
    uint32_t     seg_addr[2]; /* absolute addresses of the two segments */
} udb_flash_ctx_t;

/*
 * Populate drv (read/write/reset, prog_size, seg_size, ctx) from ctx. ctx must
 * have static storage duration: drv->ctx keeps pointing at it.
 */
void flashdrv_build(udb_driver_t *drv, udb_flash_ctx_t *ctx);

#endif /* FLASH_DRV_H */
