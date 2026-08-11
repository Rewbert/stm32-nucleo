/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * Secure side of the door-lock-c example -- a hand-written C baseline for
 * examples/microhastee's Haskell DoorLock.hs, meant for a fair LOC/complexity
 * comparison (see paper-roadmap-.md's evaluation plan). Same trust split,
 * same board wiring, same behavior; no TrustZone-M discipline enforced by a
 * type system this time, just the raw CMSE mechanism and manual care.
 *
 * Owns the PIN, the failed-attempt lockout counter (both persisted via udb --
 * a RAM counter would be defeated by power-cycling), and the door actuation:
 * three LEDs stand in for a real actuator (red = locked, green = unlocked,
 * blue = locked out), same assignment as the Haskell version and for the same
 * reason -- they're standing in for the thing being protected, so they stay
 * secure. nsc_unlock_attempt is the only thing the non-secure world can call;
 * unlike the Haskell version it takes a full 4-digit attempt in one call,
 * because plain C has no restriction against a non-secure EXTI callback
 * keeping its own static buffer (see NS/main.c) -- the Haskell version only
 * buffers secure-side because Setup/Secure/Nonsecure has no non-secure
 * mutable-state primitive.
 *
 * CAUTION: nsc_unlock_attempt runs synchronously underneath whichever
 * non-secure EXTI ISR called it (see NS/main.c) -- there is no separate
 * "bottom half" mechanism in this codebase. On a correct PIN it blocks for 2s
 * in systick_delay_ms(). Each security domain has its own SysTick instance
 * (see drivers/systick.h), and whether the secure SysTick can preempt
 * whatever non-secure context this call was entered from depends on
 * AIRCR.PRIS, which neither this file nor board_init() configures explicitly
 * and which was not verified against the reference manual while writing
 * this. Worth checking before relying on the unlock pulse on real hardware.
 */

#include <string.h>

#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"

#include "udb.h"
#include "udb-drv.h"
#include "keypad.h"

#define MAX_ATTEMPTS    3
#define UDB_KEY_PIN     1u
#define UDB_KEY_LOCKOUT 2u

static const uint8_t factory_pin[4] = { 1, 2, 3, 4 };

static uint32_t read_lockout_count(void) {
    uint32_t count = 0, n = 0;
    udb_status_t st = udb_get(board_udb(), UDB_KEY_LOCKOUT, &count, sizeof count, &n);
    return (st == UDB_OK) ? count : 0u;
}

static void write_lockout_count(uint32_t count) {
    udb_put(board_udb(), UDB_KEY_LOCKOUT, &count, sizeof count);
}

/* Reads the persisted PIN into out[4], seeding it with the factory default on
 * first use (no live entry yet). */
static void read_pin(uint8_t out[4]) {
    uint32_t n = 0;
    udb_status_t st = udb_get(board_udb(), UDB_KEY_PIN, out, 4u, &n);
    if (st != UDB_OK) {
        memcpy(out, factory_pin, 4);
        udb_put(board_udb(), UDB_KEY_PIN, factory_pin, 4u);
    }
}

/* Return encoding, mirroring DoorLock.hs's UnlockResult minus Collecting
 * (this version buffers the attempt non-secure side, so the gateway only
 * ever sees complete attempts): -1 = locked out, 0 = granted, N>0 = denied
 * with N attempts remaining. */
static int secure_unlock_attempt(uint8_t d1, uint8_t d2, uint8_t d3, uint8_t d4) {
    uint32_t count = read_lockout_count();

    if (count >= MAX_ATTEMPTS) {
        gpio_write(board_led(BOARD_LED_BLUE), GPIO_HIGH);
        return -1;
    }

    uint8_t pin[4];
    read_pin(pin);

    uint8_t attempt[4] = { d1, d2, d3, d4 };

    if (memcmp(attempt, pin, 4) == 0) {
        write_lockout_count(0u);
        gpio_write(board_led(BOARD_LED_BLUE), GPIO_LOW);
        gpio_write(board_led(BOARD_LED_RED), GPIO_LOW);
        gpio_write(board_led(BOARD_LED_GREEN), GPIO_HIGH);
        systick_delay_ms(2000); /* momentary unlock, like an electric strike */
        gpio_write(board_led(BOARD_LED_GREEN), GPIO_LOW);
        gpio_write(board_led(BOARD_LED_RED), GPIO_HIGH);
        return 0;
    }

    count++;
    write_lockout_count(count);
    if (count >= MAX_ATTEMPTS) {
        gpio_write(board_led(BOARD_LED_BLUE), GPIO_HIGH);
    }
    return (int)(MAX_ATTEMPTS - count);
}

NONSECURE_CALLABLE int nsc_unlock_attempt(int d1, int d2, int d3, int d4) {
    return secure_unlock_attempt((uint8_t)d1, (uint8_t)d2, (uint8_t)d3, (uint8_t)d4);
}

static void configure_leds(void) {
    rcc_enable(board_rcc(), RCC_GPIOB);
    rcc_enable(board_rcc(), RCC_GPIOC);
    /* GPIOG (red LED, PG2) is already enabled by board_init() for the VDDIO2 rail. */

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_RED),   &led_cfg);
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);
    gpio_init(board_led(BOARD_LED_BLUE),  &led_cfg);

    gpio_write(board_led(BOARD_LED_RED), GPIO_HIGH); /* door starts locked */
}

static void mount_or_format_db(void) {
    udb_status_t st = udb_mount(board_udb(), board_udb_config());
    if (st == UDB_ERR_NO_DB) {
        udb_format(board_udb(), board_udb_config());
    }
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

    configure_leds();
    mount_or_format_db();
    keypad_init();

    irq_enable();

    /* release the console and the keypad's interrupts to the non-secure
     * world, then return: the bootloader's tz_init() launches it next */
    tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_NONSECURE);
}
