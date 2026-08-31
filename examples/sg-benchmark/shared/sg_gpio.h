/* Copyright 2026 Robert Krook, see LICENSE file for the full license. */

#ifndef SG_GPIO_H
#define SG_GPIO_H

#include "domain/domain.h"
#include "domain/cmsis_select.h"

#include "drivers/gpio.h"
#include "drivers/exti.h"

#include "firmware/boards/board.h"

#include <stdint.h>

#define SG_PIN_0 0
#define SG_PIN_1 1

/* Both pins are configured pull-down, rising edge: idle LOW, an external
 * source driving the pin HIGH fires the EXTI callback
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
} sg_gpio_pin_t;

#define SG_GPIO_NUM_PINS 2

extern sg_gpio_pin_t sg_gpio_pins[SG_GPIO_NUM_PINS];

/**
 * @brief Both applications (Secure and Nonsecure) call this function to set up the shared
 * GPIO/EXTI devices. Only the secure world actually configures the peripherals.
 */
void sg_gpio_init(void);

static inline exti_dev_t *sg_gpio_get_exti(uint32_t idx) {
    if (idx < SG_GPIO_NUM_PINS) {
        return &sg_gpio_pins[idx].exti;
    }

    return NULL; /* idx > SG_GPIO_NUM_PINS */
}

#endif /* SG_GPIO_H */
