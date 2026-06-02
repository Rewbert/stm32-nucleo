#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/pwr.h"
#include "drivers/systick.h"
#include "drivers/irq.h"
#include "drivers/tzsc.h"

#include "boards/board.h"
#include "breadboard.h"

/**
 * @brief Configure PLL to run board at 110 MHz, systick handler to implement a 1ms tick, and
 * enable the uart.
 * 
 */
void sys_init() {
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000);

    // make the lpuart1 secure only
    tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_SECURE);
    tzsc_lock(board_tzsc());

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);
}

/**
 * @brief Configure the board LED GPIO's to act as actual LEDs.
 * 
 */
static inline void configure_board_leds() {
    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_GPIOB);
    rcc_enable(board_rcc(), RCC_GPIOC);

    gpio_config_t led_cfg = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };
    gpio_init(board_led(BOARD_LED_RED),   &led_cfg);
    gpio_init(board_led(BOARD_LED_BLUE),  &led_cfg);
    gpio_init(board_led(BOARD_LED_GREEN), &led_cfg);
}

/* Placeholder NSC function — gives the linker something to put in the import lib. Replace with real entry points. */
int __attribute__((cmse_nonsecure_entry)) nsc_placeholder(void) { return 0; }

NONSECURE_CALLABLE void secure_print(const uint8_t *str, const uint8_t len) {
    uart_write(board_console(), str, len);
}

void main(void) {
    board_init();
    sys_init();
    breadboard_init();
    configure_board_leds();

    irq_enable();

    // secure app
}