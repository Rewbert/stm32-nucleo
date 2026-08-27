#include <stdint.h>
#include <stdio.h>

#include "firmware/boards/board.h"
#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/systick.h"
#include "drivers/irq.h"
#include "drivers/rcc.h"
#include "drivers/exti.h"
#include "drivers/nvic.h"

/* Arduino A2 == PC3 (UM2861 Table 17, Zio connector CN9) — the MOSFET gate.
 * File-scope so the button's EXTI callback below can reach it. */
static board_gpio_backend_t gate_backend;
static gpio_dev_t gate;

/* U5 Nucleo-144 user button (PC13) is active-high: an external pull-down
 * holds it low, and pressing it drives the pin high (see board.c). */
static void button_edge_changed(exti_edge_t edge) {
    gpio_level_t level = (edge == EXTI_EDGE_RISING) ? GPIO_HIGH : GPIO_LOW;
    gpio_write(&gate, level);
    gpio_write(board_led(BOARD_LED_GREEN), level);
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
    gpio_config_t gate_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(&gate, &gate_cfg);
    gpio_write(&gate, GPIO_LOW); /* keep the MOSFET off until driven on purpose */

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);
    gpio_write(board_led(BOARD_LED_GREEN), GPIO_LOW);

    gpio_config_t button_cfg = {
        .mode      = GPIO_MODE_INPUT,
        .pull      = GPIO_PULLDOWN,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_button(BOARD_BUTTON_USER), &button_cfg);

    exti_config_t button_exti_cfg = {
        .port = EXTI_PORT_C,
        .pin  = 13,
        .edge = EXTI_EDGE_BOTH, /* need both: press turns the gate/LED on, release turns them off */
    };
    exti_init(board_button_exti(BOARD_BUTTON_USER), &button_exti_cfg);

    int button_irq = exti_irqn(board_button_exti(BOARD_BUTTON_USER));
    nvic_set_priority(button_irq, 0);
    nvic_enable_irq(button_irq);

    exti_register_callback(board_button_exti(BOARD_BUTTON_USER), button_edge_changed);

    irq_enable();

    uart_write(board_console(), "booted\n\r", 8);
    while (1) {
    }
}
