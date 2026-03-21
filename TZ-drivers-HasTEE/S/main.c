#include "domain/domain.h"

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/rcc.h"
#include "drivers/tzsc.h"
#include "drivers/systick.h"
#include "drivers/irq.h"

#include "boards/board.h"

int mhs_main(int argc, char **argv);

/* Configure clocks, SysTick, UART (assigned to nonsecure), and LEDs. */
void stm32_init() {
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000);

    /* Release UART to the nonsecure world, then configure it. */
    tzsc_set_periph(board_tzsc(), board_console_periph(), TZSC_NONSECURE);

    uart_config_t uart_cfg = {
        .baudrate    = 115200,
        .word_length = 8,
        .stop_bits   = 1,
        .parity      = UART_PARITY_NONE,
    };
    uart_init(board_console(), &uart_cfg);

    /* Enable GPIO clocks and configure LEDs. */
    rcc_enable(board_rcc(), RCC_GPIOA);
    rcc_enable(board_rcc(), RCC_GPIOC);

    gpio_config_t red_cfg = {
        .mode            = GPIO_MODE_OUTPUT,
        .pull            = GPIO_NOPULL,
        .alternate       = GPIO_AF0,
        .security_domain = GPIO_SECURE,
    };
    gpio_init(board_led(BOARD_LED_RED), &red_cfg);

    gpio_config_t green_cfg = {
        .mode            = GPIO_MODE_OUTPUT,
        .pull            = GPIO_NOPULL,
        .alternate       = GPIO_AF0,
        .security_domain = GPIO_NONSECURE,
    };
    gpio_init(board_led(BOARD_LED_GREEN), &green_cfg);

    irq_enable();
}

void stm32_exit(int n) {
    gpio_toggle(board_led(BOARD_LED_RED));
}

void main(void) {
    board_init();
    stm32_init();
    mhs_main(0, 0);
}

NONSECURE_CALLABLE int add(int x) {
    return x + 5;
}
