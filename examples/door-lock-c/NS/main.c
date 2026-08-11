/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

/*
 * Non-secure side of the door-lock-c example. Owns the keypad (4 GPIOs, each
 * with its own EXTI line, interrupt-driven -- same pattern as the onboard
 * button in examples/blink-and-button) and the UART, both released to us by
 * the secure world.
 *
 * Buffers a 4-digit attempt in an ordinary static array: nothing stops a
 * non-secure EXTI callback from having its own local state in plain C. That's
 * the one place this baseline is structurally simpler than
 * examples/microhastee's DoorLock.hs, which had to move the equivalent buffer
 * secure-side (via SRef) because its Setup/Secure/Nonsecure split has no
 * non-secure mutable-state primitive at all.
 */

#include <string.h>

#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"

#include "keypad.h"

extern int nsc_unlock_attempt(int d1, int d2, int d3, int d4);

static void console_puts(const char *s) {
    uart_write(board_console(), (const uint8_t *)s, strlen(s));
}

/* d is always a single digit (0-9) here -- attempts-remaining and the
 * in-progress count both stay well under 10 -- so this is simpler than
 * pulling in sprintf. */
static void console_put_digit(int d) {
    char c = (char)('0' + d);
    uart_write(board_console(), (const uint8_t *)&c, 1);
}

static void report_result(int result) {
    if (result == 0) {
        console_puts("door: unlock granted\r\n");
    } else if (result < 0) {
        console_puts("door: locked out\r\n");
    } else {
        console_puts("door: wrong pin, ");
        console_put_digit(result);
        console_puts(" attempt(s) left\r\n");
    }
}

static uint8_t buffer[4];
static int     buffer_len = 0;

static void handle_digit(uint8_t digit) {
    if (buffer_len < 4) {
        buffer[buffer_len++] = digit;
    }

    if (buffer_len < 4) {
        console_puts("key ");
        console_put_digit(buffer_len);
        console_puts("/4\r\n");
        return;
    }

    int result = nsc_unlock_attempt(buffer[0], buffer[1], buffer[2], buffer[3]);
    buffer_len = 0;
    report_result(result);
}

/* One EXTI callback per key -- all identical modulo which digit they report,
 * same macro trick as examples/breadboard-buttons/NS/main.c. */
#define KEY_CALLBACK(n) \
static void key##n##_callback(exti_edge_t edge) { \
    (void)edge; \
    handle_digit(n); \
}

KEY_CALLBACK(1)
KEY_CALLBACK(2)
KEY_CALLBACK(3)
KEY_CALLBACK(4)

void main(void) {
    board_init();
    systick_configure(board_sysclk_hz() / 1000u);

    keypad_init();
    exti_register_callback(keypad_exti(0), key1_callback);
    exti_register_callback(keypad_exti(1), key2_callback);
    exti_register_callback(keypad_exti(2), key3_callback);
    exti_register_callback(keypad_exti(3), key4_callback);

    irq_enable();

    console_puts("door lock ready\r\n");

    while (1) {
    }
}
