/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include <stdbool.h>

#include "domain/domain.h"

#include "firmware/boards/board.h"

#include "flash-drv.h"
#include "udb-drv.h"

#if HAL_SECURE
   /* The last 8 KB page of secure flash is the NSC veneer page (0x0C1FE000), so
    * the two udb pages sit just below it: segments at 0x0C1FA000 and 0x0C1FC000.
    * This is the PERSIST region in firmware/bootloader/stm32u5/S/ls-s.ld. */
#define UDB_FLASH_END  0x0C1FE000u
#else
   /* Last two 8 KB pages of NS flash; the PERSIST region in
    * firmware/bootloader/stm32u5/NS/ls-ns.ld. */
#define UDB_FLASH_END  0x08400000u   /* end of flash (4 MB), NS bank 2 end */
#endif

static udb_flash_ctx_t ctx;
static udb_driver_t    drv;
static udb_config_t    cfg;
static udb_t           db;
static bool             built = false;

static void ensure_driver(void) {
    if (built) {
        return;
    }

    ctx.dev = board_flash();

    uint32_t page = flash_page_size(ctx.dev);
    ctx.seg_addr[0] = UDB_FLASH_END - 2u * page;
    ctx.seg_addr[1] = UDB_FLASH_END - 1u * page;

    flashdrv_build(&drv, &ctx);
    cfg.drv = &drv;
    built = true;
}

udb_t *board_udb(void) {
    ensure_driver();
    return &db;
}

const udb_config_t *board_udb_config(void) {
    ensure_driver();
    return &cfg;
}
