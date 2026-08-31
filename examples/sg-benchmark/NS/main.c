/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include <string.h>
#include <stdint.h>

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/systick.h"

#include "firmware/boards/board.h"

#include "sg_benchmark.h"

extern void nsc_noop(void);
extern void nsc_sg_call(const sg_input_t *in,
                         uint8_t *out_buf, int out_capacity, int *out_len);

static void console_puts(const char *s) {
    uart_write(board_console(), (const uint8_t *)s, strlen(s));
}

/* b is always a nibble here, so this is simpler than pulling in sprintf --
 * same trick as examples/door-lock-c. */
static void console_put_hex_byte(uint8_t b) {
    static const char digits[] = "0123456789abcdef";
    char out[2] = { digits[b >> 4], digits[b & 0xFu] };
    uart_write(board_console(), (const uint8_t *)out, 2);
}

/* Fixed 16-byte payload, well under MAX_SG_INPUT_LEN in S/main.c. */
static const uint8_t payload[16] = "benchmark-input";

void main(void) {
    board_init();
    systick_configure(board_sysclk_hz() / 1000u);

    console_puts("sg-benchmark ready\r\n");

    while (1) {
        nsc_noop();

        sg_input_t in      = { .buf = payload, .len = (int)sizeof payload };
        uint8_t    out_buf[sizeof(uint32_t)];
        int        out_len = 0;
        nsc_sg_call(&in, out_buf, (int)sizeof out_buf, &out_len);

        gpio_toggle(board_led(BOARD_LED_GREEN));

        if (out_len == (int)sizeof(uint32_t)) {
            console_puts("sg_call checksum: 0x");
            console_put_hex_byte(out_buf[3]);
            console_put_hex_byte(out_buf[2]);
            console_put_hex_byte(out_buf[1]);
            console_put_hex_byte(out_buf[0]);
            console_puts("\r\n");
        } else {
            console_puts("sg_call rejected\r\n");
        }

        systick_delay_ms(500);
    }
}
