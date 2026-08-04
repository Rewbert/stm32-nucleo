/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * STM32U5 udb flash backend.
 *
 * The flash program/erase geometry now lives in the HAL flash driver
 * (firmware/drivers/src/drivers/stm32u5/flash.c); this file only decides where
 * the database lives: the two segments are the last two erase pages of this
 * domain's flash. Compiled twice — -DSECURE puts the database in the secure
 * flash bank (0x0C…), the non-secure build uses the 0x08… alias; board_flash()
 * already targets the right one.
 *
 * The UDB_FLASH_END values below MUST match the 2 pages reserved at the end of
 * the matching linker script (firmware/bootloader/stm32u5/{S,NS}/ls-*.ld) — on
 * the secure side this has to be reconciled with the existing PERSIST/FLASH_NSC
 * regions. We do not edit the linker scripts here.
 */

#include "domain/domain.h"

#include "firmware/boards/board.h"

#include "flash-drv.h"
#include "udb-drv.h"

#if HAL_SECURE
   /* The last 8 KB page of secure flash is the NSC veneer page (0x0C1FE000), so
    * the two udb pages sit just below it: segments at 0x0C1FA000 and 0x0C1FC000.
    * This is the PERSIST region in firmware/bootloader/stm32u5/S/ls-s.ld. */
#  define UDB_FLASH_END  0x0C1FE000u
#else
   /* Last two 8 KB pages of NS flash; the PERSIST region in
    * firmware/bootloader/stm32u5/NS/ls-ns.ld. */
#  define UDB_FLASH_END  0x08400000u   /* end of flash (4 MB), NS bank 2 end */
#endif

static udb_flash_ctx_t ctx;

void udb_drv_create(udb_driver_t *drv) {
    ctx.dev = board_flash();

    uint32_t page = flash_page_size(ctx.dev);
    ctx.seg_addr[0] = UDB_FLASH_END - 2u * page;
    ctx.seg_addr[1] = UDB_FLASH_END - 1u * page;

    flashdrv_build(drv, &ctx);
}
