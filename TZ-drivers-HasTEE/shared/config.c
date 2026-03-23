#include "domain/cmsis_select.h"
#include "drivers/gpio.h"
#include "boards/board.h"

int ffs(int x) {
    if (!x)
        return 0;
    x &= -x;           /* keep lowest bit */
    int i = __CLZ(x);  /* count leading 0s */
    return 32 - i;     /* 31 leading zeros should return 1 */
}

void toggle_red_led(void) {
    gpio_toggle(board_led(BOARD_LED_RED));
}

void toggle_green_led(void) {
    gpio_toggle(board_led(BOARD_LED_GREEN));
}

void toggle_blue_led(void) {
    gpio_toggle(board_led(BOARD_LED_BLUE));
}