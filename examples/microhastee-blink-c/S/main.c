#include "domain/domain.h"

#include "firmware/boards/board.h"

#include "drivers/gpio.h"
#include "drivers/rcc.h"
#include "drivers/systick.h"

NONSECURE_CALLABLE void secure_toggle_red(void) {
    gpio_toggle(board_led(BOARD_LED_RED));
}

void main(void) {
    board_init();
    board_configure_pll();
    systick_configure(board_sysclk_hz() / 1000U);

    rcc_enable(board_rcc(), RCC_GPIOG);
    rcc_enable(board_rcc(), RCC_GPIOC);

    gpio_config_t output = {
        .mode      = GPIO_MODE_OUTPUT,
        .pull      = GPIO_NOPULL,
        .alternate = GPIO_AF0,
    };

    gpio_init(board_led(BOARD_LED_RED), &output);
    gpio_set_security(board_led(BOARD_LED_RED), GPIO_SECURE);

    gpio_init(board_led(BOARD_LED_GREEN), &output);
    gpio_set_security(board_led(BOARD_LED_GREEN), GPIO_NONSECURE);
}
