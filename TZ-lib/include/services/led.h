#ifndef SERVICES_LED_H
#define SERVICES_LED_H

#include "hal/core/gpio.h"
#include "hal/platform/domain.h"

/**
 * @brief Red LED hard-wired on the STM32L552ZEQ board
 * 
 */
extern gpio_t red_led;

/**
 * @brief Blue LED hard-wired on the STM32L552ZEQ board
 * 
 */
extern gpio_t blue_led ;

/**
 * @brief Green LED hard-wired on the STM32L552ZEQ board
 * 
 */
extern gpio_t green_led;

/**
 * @brief Initialise a LED, such that it is ready to be used.
 * 
 */
void init_led(gpio_t gpio, security_domain_t domain);

#endif // SERVICES_LED_H