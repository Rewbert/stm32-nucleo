#ifndef BOARD_H
#define BOARD_H

/*
 * This is my attempt at some kind of board abstraction. The drivers abstract away the peripherals, and this file
 * abstracts away the board.
 * 
 * TODO: There is currently no board agnostic way to get your own gpio_dev_t from naming ports and pins. This
 * must be added, otherwise working with a breadboard is difficult.
 */

#include "drivers/gpio.h"
#include "drivers/uart.h"
#include "drivers/exti.h"
#include "drivers/rcc.h"
#include "drivers/flash.h"
#include "drivers/pwr.h"
#include "drivers/mpcbb.h"

typedef enum {
    BOARD_LED_GREEN = 0,
    BOARD_LED_BLUE,
    BOARD_LED_RED,
} board_led_t;

typedef enum {
    BOARD_BUTTON_USER = 0,
} board_button_t;

/*
 * This init is quite simple. It wires upp the relevant backends for the drivers, but does otherwise not do
 * much configuration. That must be done by the application (for now).
 *
 * As an example, the uart backend is wired up and the driver constructed, but actually initialising the uart
 * must be done by the application via the said driver.
 */
void board_init(void);

gpio_dev_t  *board_led(board_led_t led);
gpio_dev_t  *board_button(board_button_t btn);
exti_dev_t  *board_button_exti(board_button_t btn);
uart_dev_t  *board_console(void);
rcc_dev_t   *board_rcc(void);
flash_dev_t *board_flash(void);
pwr_dev_t   *board_pwr(void);

#define BOARD_MPCBB_COUNT 2
#define BOARD_MPCBB1_SUPERBLOCKS 24
#define BOARD_MPCBB2_SUPERBLOCKS 8

mpcbb_dev_t *board_mpcbb(int index);

#endif // BOARD_H
