/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * Non-secure side of the udb-test example.
 *
 * Runs a udb smoke test against the non-secure flash database and prints the
 * result. Clocks and the console were configured (and the console released to
 * us) by the secure world, so we only bring up SysTick and use the console.
 *
 * NOTE: this is a stub (see S/main.c for the same flash-bank caveat).
 */

#include <string.h>

#include "drivers/uart.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"

#include "udb.h"
#include "udb-drv.h"

static udb_t        db;
static udb_driver_t drv;

static void console_puts(const char *s) {
    uart_write(board_console(), (const uint8_t *)s, strlen(s));
}

/* Build the driver, mount (or format on first run), and do a put/get round-trip. */
static udb_status_t udb_smoke(void) {
    udb_drv_create(&drv);

    const udb_config_t cfg = { .drv = &drv };

    udb_status_t st = udb_mount(&db, &cfg);
    if (st == UDB_ERR_NO_DB) {
        st = udb_format(&db, &cfg);
    }
    if (st != UDB_OK) {
        return st;
    }

    uint32_t v = 42u;
    st = udb_put(&db, 7u, &v, sizeof v);
    if (st != UDB_OK) {
        return st;
    }

    uint32_t out = 0u, n = 0u;
    st = udb_get(&db, 7u, &out, sizeof out, &n);
    if (st != UDB_OK) {
        return st;
    }

    return (out == v) ? UDB_OK : UDB_ERR_CORRUPT;
}

void main(void) {
    board_init();
    systick_configure(board_sysclk_hz() / 1000u);
    irq_enable();

    console_puts(udb_smoke() == UDB_OK ? "nonsecure udb: OK\r\n" : "nonsecure udb: FAIL\r\n");

    while (1) { }
}
