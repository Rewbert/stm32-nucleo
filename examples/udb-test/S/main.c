/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * Secure side of the udb-test example.
 *
 * Configures clocks and the console, runs a udb smoke test against the secure
 * flash database, prints the result, releases the console to the non-secure
 * world, and returns. Returning lets the bootloader's tz_init() launch the
 * non-secure application (see firmware/bootloader/<mcu>/S/src/tz_init.c).
 *
 * NOTE: this is a stub. The smoke test erases and programs the secure flash
 * bank while executing from that same bank, which stalls the core for the
 * duration of each operation — fine for a one-shot test, something to revisit
 * if this grows into a real workload.
 */

#include <string.h>

#include "domain/domain.h"

#include "drivers/uart.h"
#include "drivers/tzsc.h"
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
    st = udb_put(&db, 7u, &v, sizeof(v));
    if (st != UDB_OK) {
        return st;
    }

    uint32_t out = 0u;
    uint32_t n = 0u;
    st = udb_get(&db, 7u, &out, sizeof(out), &n);
    if (st != UDB_OK) {
        return st;
    }

    return (out == v) ? UDB_OK : UDB_ERR_CORRUPT;
}

void main(void) {
    board_init();
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000u);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);
    irq_enable();

    console_puts(udb_smoke() == UDB_OK ? "secure udb: OK\r\n" : "secure udb: FAIL\r\n");

    /* Release the console so the non-secure world can use it, then return: the
     * bootloader launches the non-secure application next. */
    tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_NONSECURE);
}
