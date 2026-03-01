/*
 * Board: STM32L552ZEQ Nucleo-144
 *
 * Pins:

 *   green LED : PC7
 *   blue  LED : PB7
 *   red   LED : PA9
 *
 *   The blue button: PC13 (active low)
 *
 *   UART : LPUART1 (PG7=TX, PG8=RX)
 */

#include "boards/board.h"

#include "domain/cmsis_select.h"
#include "stm32l5xx.h"

#include "backends/stm32l5/gpio.h"
#include "backends/stm32l5/exti.h"
#include "backends/stm32l5/lpuart1.h"
#include "backends/stm32l5/flash.h"
#include "backends/stm32l5/rcc.h"
#include "backends/stm32l5/pwr.h"
#include "backends/stm32l5/mpcbb.h"

static stm32l5_gpio_backend_t led_backends[3];
static gpio_dev_t             leds[3];

static stm32l5_gpio_backend_t button_gpio_backend;
static gpio_dev_t             button_gpio;

static stm32l5_exti_backend_t button_exti_backend;
static exti_dev_t             button_exti;

static stm32l5_lpuart1_backend_t console_backend;
static uart_dev_t                console;

static rcc_dev_t   rcc;
static pwr_dev_t   pwr;

static stm32l5_flash_backend_t flash_backend;
static flash_dev_t             flash;

void board_init(void) {
    stm32l5_gpio_create(&leds[BOARD_LED_GREEN], GPIO_CMSIS(C), 7,  &led_backends[BOARD_LED_GREEN]);
    stm32l5_gpio_create(&leds[BOARD_LED_BLUE],  GPIO_CMSIS(B), 7,  &led_backends[BOARD_LED_BLUE]);
    stm32l5_gpio_create(&leds[BOARD_LED_RED],   GPIO_CMSIS(A), 9,  &led_backends[BOARD_LED_RED]);

    stm32l5_gpio_create(&button_gpio, GPIO_CMSIS(C), 13, &button_gpio_backend);
    stm32l5_exti_create(&button_exti, &button_exti_backend);

    stm32l5_lpuart1_create(&console, LPUART1x, &console_backend);

    stm32l5_rcc_create(&rcc);
    stm32l5_pwr_create(&pwr);
    stm32l5_flash_create(&flash, FLASHx, &flash_backend);
}

gpio_dev_t *board_led(board_led_t led) {
    return &leds[led];
}

gpio_dev_t *board_button(board_button_t btn) {
    (void)btn;
    return &button_gpio;
}

exti_dev_t *board_button_exti(board_button_t btn) {
    (void)btn;
    return &button_exti;
}

uart_dev_t *board_console(void) {
    return &console;
}

rcc_dev_t *board_rcc(void) {
    return &rcc;
}

flash_dev_t *board_flash(void) {
    return &flash;
}

pwr_dev_t *board_pwr(void) {
    return &pwr;
}

static stm32l5_mpcbb_backend_t mpcbb1_backend;
static mpcbb_dev_t mpcbb1;

static stm32l5_mpcbb_backend_t mpcbb2_backend;
static mpcbb_dev_t mpcbb2;

#if HAL_SECURE
uint8_t gtzc_enabled;
uint8_t mpcbb1_created;
uint8_t mpcbb2_created;
#endif

mpcbb_dev_t *board_mpcbb(int index) {
#if HAL_SECURE
    if(index >= BOARD_MPCBB_COUNT) return NULL;

    if(!gtzc_enabled) {
        rcc_dev_t rcc;
        stm32l5_rcc_create(&rcc);
        rcc_enable(&rcc, RCC_GTZC);
        gtzc_enabled = 1;
    }

    switch(index) {
        case 0: {
            if(!mpcbb1_created) {
                stm32l5_mpcbb_create(&mpcbb1, GTZC_MPCBB1, &mpcbb1_backend);
                mpcbb1_created = 1;
            }
            return &mpcbb1;
        }
        case 1: {
            if(!mpcbb2_created) {
                stm32l5_mpcbb_create(&mpcbb2, GTZC_MPCBB2, &mpcbb2_backend);
                mpcbb2_created = 1;
            }
            return &mpcbb2;
        }
        default: return NULL;
    }
#endif
    return NULL;
}