/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * STM32L5 udb flash backend.
 *
 * The flash program/erase geometry now lives in the HAL flash driver
 * (firmware/drivers/src/drivers/stm32l5/flash.c); this file only decides where
 * the database lives: the two segments are the last two erase pages of this
 * domain's flash. Compiled twice — -DSECURE puts the database in the secure
 * flash bank (0x0C…), the non-secure build uses the 0x08… alias; board_flash()
 * already targets the right one.
 *
 * The UDB_FLASH_END values below MUST match the 2 pages reserved at the end of
 * the matching linker script (firmware/bootloader/stm32l5/{S,NS}/ls-*.ld) — on
 * the secure side this has to be reconciled with the existing PERSIST/FLASH_NSC
 * regions. We do not edit the linker scripts here.
 */

#include "domain/domain.h"

#include "firmware/boards/board.h"

#include "flash-drv.h"
#include "udb-drv.h"

#if HAL_SECURE
#  define UDB_FLASH_END  0x0C040000u   /* end of secure bank 1 (256 KB) */
#else
#  define UDB_FLASH_END  0x08080000u   /* end of non-secure bank 2 (512 KB) */
#endif

static udb_flash_ctx_t ctx;

void udb_drv_create(udb_driver_t *drv) {
    ctx.dev = board_flash();

    uint32_t page = flash_page_size(ctx.dev);
    ctx.seg_addr[0] = UDB_FLASH_END - 2u * page;
    ctx.seg_addr[1] = UDB_FLASH_END - 1u * page;

    flashdrv_build(drv, &ctx);
}
