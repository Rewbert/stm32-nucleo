#include <stdint.h>
#include <stdio.h>

#include "firmware/boards/board.h"
#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/systick.h"
#include "drivers/irq.h"
#include "drivers/rcc.h"

/* The Arduino A2 pin is bound to the boards PC3, and is connected
 * to the the MOSFET gate.
 */
static board_gpio_backend_t gate_backend;
static gpio_dev_t gate;

#define UNLOCK_MS 3000
static volatile uint32_t unlock_until = 0;

static void button_edge_changed(button_edge_t edge) {
    if (edge != BUTTON_EDGE_PRESS) {
        return;
    }
    unlock_until = systick_get_ticks() + UNLOCK_MS;
    gpio_write(&gate, GPIO_HIGH);
    gpio_write(board_led(BOARD_LED_GREEN), GPIO_HIGH);
}

void main(void) {
    board_init();
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);

    /* Gate, green LED (PC7), and the user button (PC13) are all on GPIOC. */
    rcc_enable(board_rcc(), RCC_GPIOC);

    board_gpio_create(&gate, BOARD_GPIO_PORT_C, 3, &gate_backend);
    gpio_config_t output_nopull_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(&gate, &output_nopull_cfg);
    gpio_init(board_led(BOARD_LED_GREEN), &output_nopull_cfg);

    gpio_write(&gate, GPIO_LOW); /* keep the MOSFET off until driven on purpose */
    gpio_write(board_led(BOARD_LED_GREEN), GPIO_LOW);

    /* No nonsecure image on this board yet, so GPIO_SECURE is a no-op here. */
    board_button_init(board_button(BOARD_BUTTON_USER), GPIO_SECURE, BUTTON_EDGE_BOTH, button_edge_changed);

    irq_enable();

    uart_write(board_console(), "booted\n\r", 8);
    while (1) {
        if (unlock_until != 0 && systick_get_ticks() >= unlock_until) {
            gpio_write(&gate, GPIO_LOW);
            gpio_write(board_led(BOARD_LED_GREEN), GPIO_LOW);
            unlock_until = 0;
        }
    }
}
