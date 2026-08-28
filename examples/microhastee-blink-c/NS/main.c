#include "firmware/boards/board.h"

#include "drivers/gpio.h"
#include "drivers/systick.h"

extern void secure_toggle_red(void);

void main(void) {
    board_init();
    systick_configure(board_sysclk_hz() / 1000U);

    while (1) {
        gpio_toggle(board_led(BOARD_LED_GREEN));
        secure_toggle_red();
        systick_delay_ms(500U);
    }
}
