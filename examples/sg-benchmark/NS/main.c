/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include <string.h>
#include <stdint.h>

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"
#include "firmware/profile/profile.h"

#include "sg_benchmark.h"
#include "sg_gpio.h"

extern void nsc_noop(void);
extern void nsc_sg_call(const sg_input_t *in,
                         uint8_t *out_buf, int out_capacity, int *out_len);

profile_dev_t g_profile;

/* invoke nsc_noop here */
static void sg_pin0_callback(exti_edge_t edge) {
    (void)edge;
    profile_emit(&g_profile, 1);
    nsc_noop();
    profile_emit(&g_profile, 3);
}

/* Fixed 16-byte payload, well under MAX_SG_INPUT_LEN in S/main.c. */
static const uint8_t payload[16] = "benchmark-input";

/* invoke nsc_sg_call here */
static void sg_pin1_callback(exti_edge_t edge) {
    (void)edge;
    sg_input_t in      = { .buf = payload, .len = (int)sizeof payload };
    uint8_t    out_buf[sizeof(uint32_t)];
    int        out_len = 0;

    profile_emit(&g_profile, 4);
    nsc_sg_call(&in, out_buf, (int)sizeof out_buf, &out_len);
    profile_emit(&g_profile, 8);
}

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

void main(void) {
    board_init();
    profile_init(&g_profile);

    sg_gpio_init();
    exti_register_callback(sg_gpio_get_exti(SG_PIN_0), sg_pin0_callback);
    exti_register_callback(sg_gpio_get_exti(SG_PIN_1), sg_pin1_callback);

    systick_configure(board_sysclk_hz() / 1000u);

    irq_enable();

    console_puts("sg-benchmark ready\r\n");

    while (1) {
    }
}
