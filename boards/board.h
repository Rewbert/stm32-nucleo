#ifndef BOARD_H
#define BOARD_H

/*
 * This is my attempt at some kind of board abstraction. The drivers abstract away the peripherals, and this file
 * abstracts away the board.
 * 
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

/**
 * @brief GPIO ports that can be accessed.
 *
 * NOTE: I am not sure how to handle implementing boards that have more or fewer than these..
 * these are the ports available on my boards.
 * 
 */
typedef enum {
    BOARD_GPIO_PORT_A = 0,
    BOARD_GPIO_PORT_B,
    BOARD_GPIO_PORT_C,
    BOARD_GPIO_PORT_D,
    BOARD_GPIO_PORT_E,
    BOARD_GPIO_PORT_F,
    BOARD_GPIO_PORT_G,
    BOARD_GPIO_PORT_H,
} board_gpio_port_t;

#define BOARD_GPIO_BACKEND_SIZE 8
typedef struct {
    /* Do not access this member yourself */
    uint8_t _opaque[BOARD_GPIO_BACKEND_SIZE];
} board_gpio_backend_t;

#define BOARD_EXTI_BACKEND_SIZE 1
typedef struct {
    /* Do not access this member yourself */
    uint8_t _opaque[BOARD_EXTI_BACKEND_SIZE];
} board_exti_backend_t;

/**
 * @brief Create a GPIO device
 * 
 */
void board_gpio_create(gpio_dev_t *dev, board_gpio_port_t port, uint8_t pin, board_gpio_backend_t *backend);

/**
 * @brief Create an EXTI device
 * 
 */
void board_exti_create(exti_dev_t *dev, board_exti_backend_t *backend);

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
