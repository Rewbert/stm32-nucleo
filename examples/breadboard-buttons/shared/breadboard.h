#ifndef BREADBOARD_H
#define BREADBOARD_H

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/exti.h"

#include "board.h"

#include <stdint.h>

#define A2  0
#define A3  1
#define A5  2
#define A6  3
#define A7  4
#define A8  5
#define A10 6

/**
 * @brief This file documents my attempt at trying to model my breadboard setup. Currently, there
 * are only buttons on my breadboard lol.
 * 
 */

 /**
  * @brief The buttons on my breadboard needs to be configured via both GPIO and EXTI.
  * This struct captures both, and collects all information in one place.
  * 
  */
typedef struct {
    board_gpio_port_t port;
    uint8_t pin;

    board_gpio_backend_t gpio_backend;
    gpio_dev_t gpio;
    gpio_config_t gpio_cfg;

    board_exti_backend_t exti_backend;
    exti_dev_t exti;
    exti_config_t exti_cfg;
} breadboard_button_t;

#define BREADBOARD_NUM_BUTTONS 7

/**
 * @brief These are the actual buttons on the breadboard.
 * 
 */
extern breadboard_button_t buttons[BREADBOARD_NUM_BUTTONS];

/**
 * @brief Both applications (Secure and Nonsecure) call this function to initialise the relevant parts.
 * 
 */
void breadboard_init();

/**
 * @brief Fetch the gpio device associated with a breadboard button.
 * This function is intended to be invoked with one of the defined A2,A3...
 * 
 */
static inline gpio_dev_t *breadboard_get_gpio(uint32_t idx) {
    if(idx < BREADBOARD_NUM_BUTTONS) {
        return &buttons[idx].gpio;
    }

    return NULL; // idx > BREADBOARD_NUM_BUTTONS
}

/**
 * @brief Fetch the exti device associated with a breadboard button.
 * This function is intended to be invoked with one of the defined A2,A3...
 * 
 */
static inline exti_dev_t *breadboard_get_exti(uint32_t idx) {
    if(idx < BREADBOARD_NUM_BUTTONS) {
        return &buttons[idx].exti;
    }

    return NULL; // idx > BREADBOARD_NUM_BUTTONS
}

#endif // BREADBOARD_H