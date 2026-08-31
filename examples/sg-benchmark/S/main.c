/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#include <string.h>
#include <stdint.h>
#include <arm_cmse.h>

#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "firmware/boards/board.h"

#include "sg_benchmark.h"

#define MAX_SG_INPUT_LEN 64

NONSECURE_CALLABLE void nsc_noop(void) {
}

NONSECURE_CALLABLE void nsc_sg_call(const sg_input_t *in,
                                     uint8_t *out_buf, int out_capacity, int *out_len) {
    if (!cmse_check_address_range(out_len, sizeof(*out_len), CMSE_NONSECURE | CMSE_MPU_READWRITE)) {
        return;
    }

    if (!cmse_check_address_range((void *)in, sizeof(*in), CMSE_NONSECURE | CMSE_MPU_READ)) {
        *out_len = -1;
        return;
    }

    /* Read the struct fields only after the struct pointer itself is
     * confirmed to be NS memory we may read. */
    const uint8_t *in_buf = in->buf;
    int            in_len = in->len;

    /* Bounds first, address-range check on in_buf second: in_len is an
     * NS-supplied int, and a negative value would turn into a huge size_t
     * once handed to cmse_check_address_range. microhastee's real `sg`
     * avoids this because its length field is already a size_t; this one
     * isn't, so the bound has to come first. */
    if (in_len < 0 || in_len > MAX_SG_INPUT_LEN || out_capacity < (int)sizeof(uint32_t)) {
        *out_len = -1;
        return;
    }

    if (!cmse_check_address_range((void *)in_buf, (size_t)in_len, CMSE_NONSECURE | CMSE_MPU_READ) ||
        !cmse_check_address_range(out_buf, (size_t)out_capacity, CMSE_NONSECURE | CMSE_MPU_READWRITE)) {
        *out_len = -1;
        return;
    }

    /* in_buf is validated above, but it's still NS-owned memory */
    uint8_t secure_copy[MAX_SG_INPUT_LEN];
    memcpy(secure_copy, in_buf, (size_t)in_len);

    /* Stand-in for "the handler does something with the call" -- deliberately
     * trivial so the measured time stays dominated by the crossing and
     * verification cost, not by this. */
    uint32_t checksum = 0;
    for (int i = 0; i < in_len; i++) {
        checksum = (checksum * 31u) + secure_copy[i];
    }

    memcpy(out_buf, &checksum, sizeof checksum);
    *out_len = (int)sizeof checksum;
}

void main(void) {
    board_init();
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000u);

    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_GPIOB);
    rcc_enable(board_rcc(), RCC_GPIOC);

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);
    gpio_set_security(board_led(BOARD_LED_GREEN), GPIO_NONSECURE);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);

    irq_enable();

    /* Release the console and the heartbeat LED to the non-secure world,
     * then return: the bootloader jumps to the non-secure reset vector
     * next. */
    tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_NONSECURE);
}
