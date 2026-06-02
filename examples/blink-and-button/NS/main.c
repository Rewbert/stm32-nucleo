#include "boards/board.h"
#include "drivers/gpio.h"
#include "drivers/systick.h"

void main(void) {
    board_init(); /* initialise board struct pointers */
    systick_configure(board_sysclk_hz() / 1000);
    while (1) {
        gpio_toggle(board_led(BOARD_LED_GREEN));
        systick_delay_ms(500);
    }
}
